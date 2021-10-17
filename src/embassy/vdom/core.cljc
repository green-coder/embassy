(ns embassy.vdom.core
  (:refer-clojure :exclude [comp])
  (:require
    [clojure.core :as cc]
    [clojure.set :as set]
    [embassy.client.util :as u]))

;; Things to do:
;; [x] Add support for the on-xxx.
;; [x] Add helper functions to write the vdom-diff structures.
;; [x] Add functions to compose vdom-diff together.
;; [ ] Implement the move of the children.
;;     Make sure it is easy enough to combine multiple vdom-diffs together.
;; [x] Test apply-vdom using vdom-diff.
;; [ ] The app-element should be able to have multiple children.
;; [ ] Review API structure: Do we still need an empty text node by default?
;; [ ] Check how Phoenix Live View deals with controlled text inputs, w.r.t. async and lag.


(comment
  ;; When we want to clear everything on the page, the root element should be the empty string.
  ;;
  ;; A vdom-diff (a.k.a. "vdom", which is shorter) structure can be either:
  ;; - a string: replaces existing node with a text node.
  ;; - a hashmap containing :tag, and optionally :attrs, :listeners and :children.
  ;; - a hashmap without :tag, and optionally :remove-attrs, :add-attrs,
  ;;   :remove-listeners, :add-listeners, and :children-ops.

  ;; Examples:

  ;; When it is a string
  "Hello, world"

  ;; When it contains :tag
  {:tag "div"
   :attrs {"xxx" "value"}
   :listeners {"click" (fn [event] ,,,)}
   :children [vdom0 vdom1 ,,,]}

  ;; When it does not contain :tag
  {;; The keys in :remove-attrs and :add-attrs should be mutually exclusive.
   :remove-attrs #{"yyy"}
   :add-attrs {"xxx" "new-value"}

   ;; The keys are not necessarily mutually exclusive,
   ;; previous listeners have to be removed explicitly before new ones are added.
   :remove-listeners #{"focus"}
   :add-listeners {"click" (fn [event] ,,,)}

   ;;;; Re-use the vector format (index-op) from Diffuse.
   ;;:children-diff [[:no-op size0]
   ;;                [:update [vdom-diff0 vdom-diff1 ,,,]]
   ;;                [:remove size1]
   ;;                [:insert [vdom0 vdom1 ,,,]]
   ;;                [:take size2 id0]
   ;;                [:update-take [vdom-diff2 vdom-diff3 ,,,] id0]
   ;;                [:put id0]
   ;;                ,,,]

   :children-ops [{:type :no-op
                   :size n}
                  {:type :update
                   :elements [vdom-diff0 vdom-diff1 ,,,]}
                  {:type :remove
                   :size n}
                  {:type :insert
                   :elements [vdom0 vdom1 ,,,]}
                  {:type :take
                   :operations [{:type :no-op
                                 :size n}
                                {:type :update
                                 :elements [vdom-diff2 vdom-diff3 ,,,]}]
                   :move-id x}
                  {:type :put
                   :move-id x}]}


  ,)


#?(:cljs
   (defn- add-event-listener [^js element event-type event-handler]
     (let [listeners (or (-> element .-event-listeners) {})]
       (set! (-> element .-event-listeners)
             (update listeners event-type (fnil conj #{}) event-handler))
       (-> element (.addEventListener event-type event-handler)))))

#?(:cljs
   (defn- remove-event-listeners [^js element event-type]
     (let [listeners (or (-> element .-event-listeners) {})]
       (doseq [event-handler (get listeners event-type)]
         (-> element (.removeEventListener event-type event-handler)))
       (set! (-> element .-event-listeners)
             (dissoc listeners event-type)))))

#?(:cljs
   (defn- create-dom [vdom]
     (if (string? vdom)
       ;; Text node
       (-> js/document (.createTextNode vdom))

       ;; Non-text node
       (let [{:keys [tag attrs listeners children]} vdom
             node (-> js/document (.createElement tag))]
         ;; Set the attributes
         (doseq [[k v] attrs]
           (-> node (.setAttribute k v)))

         ;; Set the listeners
         (doseq [[event-type event-handler] listeners]
           (add-event-listener node event-type event-handler))

         ;; Set the children
         (doseq [child children]
           (-> node (.appendChild (create-dom child))))

         ;; Return the node
         node))))

#?(:cljs
   ;; The parent is responsible for replacing the previous dom node
   ;; by the new one if they are different nodes.
   (defn- apply-vdom* [^js dom vdom]
     (cond
       (nil? vdom)
       dom

       (or (string? vdom)
           (contains? vdom :tag))
       (create-dom vdom)

       :else
       (let [^js dom-child-nodes (-> dom .-childNodes)
             {:keys [remove-attrs add-attrs
                     remove-listeners add-listeners
                     children-ops]} vdom]

         (doseq [attr remove-attrs]
           (-> dom (.removeAttribute attr)))

         (doseq [[k v] add-attrs]
           (-> dom (.setAttribute k v)))

         (doseq [event-type remove-listeners]
           (remove-event-listeners dom event-type))

         (doseq [[event-type event-handler] add-listeners]
           (add-listeners dom event-type event-handler))

         (let [[move-id->dom-nodes _] (reduce (fn [[move-id->dom-nodes index] operation]
                                                (case (:type operation)
                                                  (:no-op :remove) [move-id->dom-nodes (+ index (:size operation))]
                                                  (:update :insert) [move-id->dom-nodes (+ index (count (:elements operation)))]
                                                  :take (let [take-size (loop [taken-operations (seq (:operations operation))
                                                                               i                0]
                                                                          (if taken-operations
                                                                            (let [taken-operation (first taken-operations)]
                                                                              (case (:type taken-operation)
                                                                                :no-op (recur (next taken-operations) (+ i (:size taken-operation)))
                                                                                :update (do
                                                                                          (doseq [[j vdom-diff] (u/seq-indexed (:elements taken-operation))]
                                                                                            (let [^js child-element     (-> dom-child-nodes (.item (+ index i j)))
                                                                                                  ^js new-child-element (apply-vdom* child-element vdom-diff)]
                                                                                              (-> child-element (.replaceWith new-child-element))))
                                                                                          (recur (next taken-operations) (+ i (count (:elements taken-operation)))))))
                                                                            i))]
                                                          ;; Collect the references to the DOM elements.
                                                          [(assoc move-id->dom-nodes (:move-id operation)
                                                                  (mapv (fn [index]
                                                                          (-> dom-child-nodes (.item index)))
                                                                        (range index (+ index take-size))))
                                                           (+ index take-size)])
                                                  :put [move-id->dom-nodes index]))
                                              [{} 0]
                                              children-ops)]
           (loop [operations (seq children-ops)
                  index 0]
             (when operations
               (let [operation (first operations)
                     next-operations (next operations)]
                 (case (:type operation)
                   :no-op (recur next-operations (+ index (:size operation)))
                   :update (do
                             (dotimes [i (count (:elements operation))]
                               (let [^js child-element     (-> dom-child-nodes (.item (+ index i)))
                                     ^js new-child-element (apply-vdom* child-element (-> (:elements operation) (nth i)))]
                                 (-> child-element (.replaceWith new-child-element))))
                             (recur next-operations (+ index (count (:elements operation)))))
                   :remove (do
                             (dotimes [_ (:size operation)]
                               (-> dom (.removeChild (-> dom-child-nodes (.item index)))))
                             (recur next-operations index))
                   :insert (do
                             (if (< index (-> dom-child-nodes .-length))
                               (let [node-after (-> dom-child-nodes (.item index))]
                                 (doseq [child-vdom (:elements operation)]
                                   (-> dom (.insertBefore (create-dom child-vdom) node-after))))
                               (doseq [child-vdom (:elements operation)]
                                 (-> dom (.appendChild (create-dom child-vdom)))))
                             (recur next-operations (+ index (count (:elements operation)))))
                   :take (recur next-operations (transduce (map (fn [taken-operation]
                                                                  (case (:type taken-operation)
                                                                    :no-op (:size taken-operation)
                                                                    :update (count (:elements taken-operation)))))
                                                           + index
                                                           (:operations operation)))
                   :put (let [^js node-to (-> dom-child-nodes (.item index))
                              nodes-from (move-id->dom-nodes (:move-id operation))]
                          (-> node-to .-before (.apply node-to (into-array nodes-from)))
                          (recur next-operations (+ index (count nodes-from)))))))))
         dom))))

#?(:cljs
   (defn apply-vdom [^js dom-element vdom]
     ;; Ensures that we have a "root" node under app-element.
     (when (zero? (-> dom-element .-childNodes .-length))
       (-> dom-element (.appendChild (create-dom ""))))

     ;; Apply the vdom
     (let [current-dom-root (-> dom-element .-firstChild)
           new-dom-root (apply-vdom* current-dom-root vdom)]
       (-> current-dom-root (.replaceWith new-dom-root)))))



;; ****************************************************************

(declare comp)


(defn- make-move-id->state
  "Returns an hashmap of move-id -> {:size n, :operations [op1 op2 ,,,]}"
  [children-ops move-id-offset]
  (into {}
        (keep (fn [child-op]
                (when (= (:type child-op) :take)
                  [(+ (:move-id child-op) move-id-offset)
                   {:size (transduce (map (fn [operation]
                                            (case (:type operation)
                                              :no-op (:size operation)
                                              :update (count (:elements operation)))))
                                     +
                                     (:operations child-op))
                    :operations (:operations child-op)}])))
        children-ops))

(defn- update-takes-and-puts
  "Returns a sequence of operations where :take and :put operations will have
   an updated :move-id and a calculated :size"
  [children-ops move-id->state move-id-offset]
  (into []
        (map (fn [child-op]
               (case (:type child-op)
                 (:no-op :update :remove :insert)
                 child-op

                 :take
                 (let [move-id (+ (:move-id child-op) move-id-offset)]
                   (-> child-op
                       (dissoc :operations)
                       (assoc :move-id move-id
                              :size (-> move-id move-id->state :size))))

                 :put
                 (let [move-id (+ (:move-id child-op) move-id-offset)]
                   (-> child-op
                       (assoc :move-id move-id
                              :size (-> move-id move-id->state :size)))))))
        children-ops))


;; Different types of taken operations:
;;
;; {:type :no-op
;;  :size n}
;; From splitting the operation into 2.
;;
;; {:type :update
;;  :elements [[vdom-diffs0 vdom-diffs1 ,,,]}
;; From :update then :take or :put then :update.
;; At pass 3, the :take is turned into a :update-take.
;;
;; {:type :remove
;;  :size n}
;; From :put then :remove.
;; At pass 3, the :take is replaced by a :remove and the :put is removed.
;;
;; {:type :insert
;;  :elements [vdom0 vdom1 ,,,]}
;; From :insert then :take.
;; At pass 3, the :take is removed and the :put is replaced by an :insert.
;;
;; {:type :move
;;  :size n
;;  :new-move-id x} (on the old take state)
;; From :put then :take.
;; At pass 3, ... things become complicated, the old move take operation has to split into multiple move-id
;; around the old part which is put.

;; The output sequence of operations replaced :take and :put by this format:
;; - {:type :take, :size n, :move-id x}
;; - {:type :put, :size n, :move-id x}



(defn- operation-size
  "Returns the size of the operation in number of DOM elements."
  [operation]
  (case (:type operation)
    (:no-op :remove :take :put :move) (:size operation)
    (:update :insert) (count (:elements operation))))


(defn- sub-operation [operation index size]
  (when (and (pos? size)
             (<= 0 index)
             (< index (operation-size operation)))
    (case (:type operation)
      (:no-op :remove :take :put :move) (assoc operation :size size)
      (:update :insert) (update operation :elements subvec index (+ index size)))))


(defn- index-op-split
  "Splits an operation into 2 pieces so that the size of the first piece is the given size,
   and then return a vector containing those 2 pieces."
  [operation size]
  (case (:type operation)
    (:no-op :remove :take :put)
    [(assoc operation :size size)
     (update operation :size - size)]

    (:update :insert)
    [(update operation :elements subvec 0 size)
     (update operation :elements subvec size)]))


(defn- head-split
  "Transforms 2 sequences of index operations so that their first elements have the same size."
  [new-iops base-iops]
  (let [new-iop (first new-iops)
        base-iop (first base-iops)
        new-size (operation-size new-iop)
        base-size (operation-size base-iop)]
    (cond
      (= new-size base-size)
      [base-size new-iops base-iops]

      (< new-size base-size)
      (let [[base-head base-tail] (index-op-split base-iop new-size)]
        [new-size new-iops (list* base-head base-tail (rest base-iops))])

      (> new-size base-size)
      (let [[new-head new-tail] (index-op-split new-iop base-size)]
        [base-size (list* new-head new-tail (rest new-iops)) base-iops]))))


(defn- get-sub-op [ops index size]
  (loop [ops      (seq ops)
         op-index 0]
    (when ops
      (let [op            (first ops)
            op-size       (operation-size op)
            int-min-index (max op-index index)
            int-max-index (min (+ op-index op-size) (+ index size))
            f0-size       (-> (- (min int-min-index int-max-index) op-index) (max 0))
            f1-size       (-> (- int-max-index int-min-index) (max 0))
            f1            (sub-operation op f0-size f1-size)]
        (if (some? f1)
          f1
          (recur (next ops)
                 (+ op-index op-size)))))))


(defn- update-taken-operations [ops index size f]
  (loop [result   []
         ops      (seq ops)
         op-index 0]
    (if ops
      (let [op            (first ops)
            op-size       (operation-size op)
            int-min-index (max op-index index)
            int-max-index (min (+ op-index op-size) (+ index size))]
        (recur (let [f0-size (-> (- (min int-min-index int-max-index) op-index) (max 0))
                     f1-size (-> (- int-max-index int-min-index) (max 0))
                     f2-size (-> (- (+ op-index op-size) (max int-min-index int-max-index)) (max 0))
                     f0      (sub-operation op 0 f0-size)
                     f1      (some-> (sub-operation op f0-size f1-size) f)
                     f2      (sub-operation op (+ f0-size f1-size) f2-size)]
                 (into result (remove nil?) [f0 f1 f2]))
               (next ops)
               (+ op-index op-size)))
      result)))


(defn- append-last-operations [move-id->state output children-ops]
  [(reduce (fn [move-id->state child-op]
             (case (:type child-op)
               :take (update-in move-id->state [(:move-id child-op) :take-index] + (:size child-op))
               :put (update-in move-id->state [(:move-id child-op) :put-index] + (:size child-op))
               move-id->state))
           move-id->state
           children-ops)
   (into output children-ops)])


(defn- index-ops-comp
  "Composes 2 sequences of index operations, and return the result.
   Note 2: the result is not guaranteed to be canonical/normalized."
  [new-move-id->state new-ops
   base-move-id->state base-ops]
  (loop [output-move-id->state (into {}
                                     (map (fn [[move-id state]]
                                            [move-id (assoc state
                                                       :take-index 0
                                                       :put-index 0)]))
                                     (concat base-move-id->state new-move-id->state))
         output []
         new-ops new-ops
         base-ops base-ops]
    (cond
      (empty? base-ops) (append-last-operations output-move-id->state output new-ops)
      (empty? new-ops) (append-last-operations output-move-id->state output base-ops)
      :else (let [[op-size split-new-ops split-base-ops] (head-split new-ops base-ops)
                  new-op  (first split-new-ops)
                  base-op (first split-base-ops)]
              (case (:type new-op)
                :insert
                (recur output-move-id->state
                       (conj output new-op)
                       (rest split-new-ops)
                       split-base-ops)

                :put
                (recur (update-in output-move-id->state [(:move-id new-op) :put-index] + op-size)
                       (conj output new-op)
                       (rest split-new-ops)
                       split-base-ops)

                ;; case's else
                (case (:type base-op)
                  :no-op
                  (recur (case (:type new-op)
                           :take (update-in output-move-id->state [(:move-id new-op) :take-index] + op-size)
                           :put (update-in output-move-id->state [(:move-id new-op) :put-index] + op-size)
                           output-move-id->state)
                         (conj output new-op)
                         (rest split-new-ops)
                         (rest split-base-ops))

                  :remove
                  (recur output-move-id->state
                         (conj output base-op)
                         split-new-ops
                         (rest split-base-ops))

                  :take
                  (recur (update-in output-move-id->state [(:move-id base-op) :take-index] + op-size)
                         (conj output base-op)
                         split-new-ops
                         (rest split-base-ops))

                  :update
                  (case (:type new-op)
                    :no-op
                    (recur output-move-id->state
                           (conj output base-op)
                           (rest split-new-ops)
                           (rest split-base-ops))

                    :update
                    (recur output-move-id->state
                           (conj output {:type :update
                                         :elements (mapv comp (:elements new-op) (:elements base-op))})
                           (rest split-new-ops)
                           (rest split-base-ops))

                    :remove
                    (recur output-move-id->state
                           (conj output new-op)
                           (rest split-new-ops)
                           (rest split-base-ops))

                    :take
                    (recur (update output-move-id->state (:move-id new-op)
                                   (fn [{:keys [take-index] :as move-state}]
                                     (-> move-state
                                         (update :take-index + op-size)
                                         (update :operations update-taken-operations take-index op-size
                                                 (fn [taken-op]
                                                   (case (:type taken-op)
                                                     :no-op base-op
                                                     :update {:type :update
                                                              :elements (mapv comp (:elements taken-op) (:elements base-op))}))))))
                           (conj output new-op)
                           (rest split-new-ops)
                           (rest split-base-ops)))

                  :insert (case (:type new-op)
                            :no-op (recur output-move-id->state
                                          (conj output base-op)
                                          (rest split-new-ops)
                                          (rest split-base-ops))
                            :update (recur output-move-id->state
                                           (conj output {:type :insert
                                                         :elements (mapv comp (:elements new-op) (:elements base-op))})
                                           (rest split-new-ops)
                                           (rest split-base-ops))
                            :remove (recur output-move-id->state
                                           output
                                           (rest split-new-ops)
                                           (rest split-base-ops))
                            :take (recur (update output-move-id->state (:move-id new-op)
                                                 (fn [{:keys [take-index] :as move-state}]
                                                   (-> move-state
                                                       (update :take-index + op-size)
                                                       (update :operations update-taken-operations take-index op-size
                                                               (fn [taken-op]
                                                                 (case (:type taken-op)
                                                                   :no-op base-op
                                                                   :update {:type :insert
                                                                            :elements (mapv comp (:elements taken-op) (:elements base-op))}))))))
                                         (conj output new-op)
                                         (rest split-new-ops)
                                         (rest split-base-ops)))
                  :put (case (:type new-op)
                         :no-op (recur (update-in output-move-id->state [(:move-id base-op) :put-index] + op-size)
                                       (conj output base-op)
                                       (rest split-new-ops)
                                       (rest split-base-ops))
                         :update (recur (update output-move-id->state (:move-id base-op)
                                                (fn [{:keys [put-index] :as move-state}]
                                                  (-> move-state
                                                      (update :put-index + op-size)
                                                      (update :operations update-taken-operations put-index op-size
                                                              (fn [taken-op]
                                                                (case (:type taken-op)
                                                                  :no-op new-op
                                                                  :update {:type :update
                                                                           :elements (mapv comp (:elements new-op) (:elements taken-op))}))))))
                                        (conj output base-op)
                                        (rest split-new-ops)
                                        (rest split-base-ops))
                         :remove (recur (update output-move-id->state (:move-id base-op)
                                                (fn [{:keys [put-index] :as move-state}]
                                                  (-> move-state
                                                      (update :put-index + op-size)
                                                      (update :operations update-taken-operations put-index op-size
                                                              (fn [_] new-op)))))
                                        (conj output base-op)
                                        (rest split-new-ops)
                                        (rest split-base-ops))
                         :take (recur (-> output-move-id->state
                                          (update (:move-id base-op)
                                                  (fn [{:keys [put-index] :as move-state}]
                                                    (-> move-state
                                                        (update :put-index + op-size)
                                                        (update :operations update-taken-operations put-index op-size
                                                                (fn [base-op]
                                                                  (let [new-move-state (output-move-id->state (:move-id new-op))
                                                                        new-frag       (get-sub-op (:operations new-move-state)
                                                                                                   (:take-index new-move-state)
                                                                                                   op-size)]
                                                                    (case [(:type new-frag) (:type base-op)]
                                                                      [:no-op :no-op] base-op
                                                                      [:no-op :update] base-op
                                                                      [:update :no-op] new-frag
                                                                      [:update :update] [:update (mapv comp (:elements new-frag) (:elements base-op))])))))))
                                          (update (:move-id new-op)
                                                  (fn [{:keys [take-index] :as state}]
                                                    (-> state
                                                        (update :take-index + op-size)
                                                        (update :operations update-taken-operations take-index op-size
                                                                (fn [_] {:type :do-not-take
                                                                         :size op-size}))))))
                                      (conj output base-op new-op) ;; new-iop will be discarded in the phase 3
                                      (rest split-new-ops)
                                      (rest split-base-ops)))))))))


(defn- merge-same-children-ops
  "Merges together a sequence of child operations of the same type."
  [children-ops]
  (let [first-child (first children-ops)]
    (case (:type first-child)
      :no-op {:type :no-op
              :size (transduce (map :size) + children-ops)}

      :update {:type :update
               :elements (into [] (mapcat :elements) children-ops)}

      :remove {:type :remove
               :size (transduce (map :size) + children-ops)}

      :take (let [move-id (:move-id first-child)]
              {:type :take
               :move-id move-id})

      :insert {:type :insert
               :elements (into [] (mapcat :elements) children-ops)}

      :put (let [move-id (:move-id first-child)]
             {:type :put
              :move-id move-id}))))


(defn- canonical-children-ops
  "Transform a sequence of children operations into its canonical form.
   The goal is to regroup operations with the same type, as well as to order the
   operations whose order can be reversed so that they are always in the same order.
   It's a kind of normalization process."
  [move-id->state children-ops]
  (into []
        (cc/comp (partition-by (fn [child-op]
                                 (let [op-type (:type child-op)]
                                   (if (#{:remove :take :insert :put} op-type)
                                     :remove-take-insert-put
                                     op-type))))
                 (mapcat (fn [children-ops]
                           (if (#{:remove :take :insert :put} (:type (first children-ops)))
                             (let [;; Divide the partition into 2 groups while maintaining the order in each group.
                                   {:keys [removes-takes inserts-puts]} (group-by (cc/comp {:remove :removes-takes
                                                                                            :take   :removes-takes
                                                                                            :insert :inserts-puts
                                                                                            :put    :inserts-puts} :type) children-ops)
                                   ;; Merge the consecutive :remove and :take ops with the same :move-id.
                                   removes-takes (into []
                                                       (cc/comp (partition-by (juxt :type :move-id))
                                                                (map merge-same-children-ops))
                                                       removes-takes)
                                   ;; Merge the consecutive :insert and :put ops with the same :move-id.
                                   inserts-puts (into []
                                                      (cc/comp (partition-by (juxt :type :move-id))
                                                               (map merge-same-children-ops))
                                                      inserts-puts)]
                               ;; TODO: merge the removes-takes with the insert-puts. Some pairs can be simplified:
                               ;; - :remove + :insert child -> :update child
                               ;; - :take move-id + :put move-id -> :no-op or :update moved-elements
                               (into removes-takes inserts-puts))
                             children-ops)))
                 ;; Merge the :no-op and :update operations
                 (partition-by (juxt :type :move-id))
                 (map merge-same-children-ops)
                 (map (fn [child-op]
                        (cond-> child-op
                          (= (:type child-op) :take)
                          (assoc :operations (-> (:move-id child-op) move-id->state :operations))))))
        children-ops))

(defn comp
  ([vdom] vdom)
  ([vdom2 vdom1]
   (cond
     ;; nil represents a no-change diff
     (nil? vdom1) vdom2
     (nil? vdom2) vdom1

     ;; If any vdom is a string
     (or (string? vdom1)
         (string? vdom2)) vdom2

     ;; vdom2 overwrites vdom1
     (contains? vdom2 :tag) vdom2

     ;; vdom2 applies on vdom1's content
     (contains? vdom1 :tag)
     (let [attrs (-> (:attrs vdom1)
                     (as-> xxx (reduce dissoc xxx (:remove-attrs vdom2)))
                     (into (:add-attrs vdom2)))
           listeners (-> (:listeners vdom1)
                         (as-> xxx (reduce dissoc xxx (:remove-listeners vdom2)))
                         (into (:add-listeners vdom2)))
           children-ops (:children-ops vdom2)

           ;; Pass 1, preprocess
           move-id->state (make-move-id->state children-ops 0)
           children-ops (update-takes-and-puts children-ops move-id->state 0)

           ;; Pass 2, apply the updates on the moved elements and store them in (:elements move-state)
           move-id->state (loop [move-id->state move-id->state
                                 children-in  (:children vdom1)
                                 children-ops (seq children-ops)]
                            (if children-ops
                              (let [child-op (first children-ops)]
                                (recur (if (= (:type child-op) :take)
                                         (let [updated-elements (loop [operations (seq (-> child-op :move-id move-id->state :operations))
                                                                       children-in children-in
                                                                       elements []]
                                                                  (if operations
                                                                    (let [operation (first operations)
                                                                          size (operation-size operation)]
                                                                      (recur (next operations)
                                                                             (subvec children-in size)
                                                                             (->> (case (:type operation)
                                                                                    :no-op (subvec children-in 0 size)
                                                                                    :update (mapv comp (:elements operation) children-in))
                                                                                  (into elements))))
                                                                    elements))]
                                           (assoc-in move-id->state [(:move-id child-op) :elements] updated-elements))
                                         move-id->state)
                                       (case (:type child-op)
                                         (:no-op :remove :take) (subvec children-in (:size child-op))
                                         :update (subvec children-in (count (:elements child-op)))
                                         (:insert :put) children-in)
                                       (next children-ops)))
                              move-id->state))

           ;; Pass 3, apply all the operations except the :take ones.
           children (loop [children-out []
                           children-in  (:children vdom1)
                           children-ops (seq children-ops)]
                      (if children-ops
                        (let [child-op (first children-ops)
                              next-children-ops (next children-ops)]
                          (case (:type child-op)
                            :no-op (recur (into children-out (take (:size child-op)) children-in)
                                          (subvec children-in (:size child-op))
                                          next-children-ops)
                            :update (recur (into children-out (mapv comp (:elements child-op) children-in))
                                           (subvec children-in (count (:elements child-op)))
                                           next-children-ops)
                            :remove (recur children-out
                                           (subvec children-in (:size child-op))
                                           next-children-ops)
                            :insert (recur (into children-out (:elements child-op))
                                           children-in
                                           next-children-ops)
                            :take (recur children-out
                                         (subvec children-in (:size child-op))
                                         next-children-ops)
                            :put (recur (into children-out (-> child-op :move-id move-id->state :elements))
                                        children-in
                                        next-children-ops)))
                        (into children-out children-in)))]
       (cond-> {:tag (:tag vdom1)}
         (some? attrs) (assoc :attrs attrs)
         (some? listeners) (assoc :listeners listeners)
         (some? children) (assoc :children children)))

     ;; vdom1 and vdom2 are both diffs (i.e. no :tags)
     :else
     (let [{remove-attrs1     :remove-attrs
            add-attrs1        :add-attrs
            remove-listeners1 :remove-listeners
            add-listeners1    :add-listeners
            children-ops1     :children-ops} vdom1
           {remove-attrs2     :remove-attrs
            add-attrs2        :add-attrs
            remove-listeners2 :remove-listeners
            add-listeners2    :add-listeners
            children-ops2     :children-ops} vdom2
           remove-attrs (-> remove-attrs1
                            (set/union remove-attrs2)
                            (as-> xxx (reduce disj xxx (keys add-attrs2))))
           add-attrs (-> add-attrs1
                         (as-> xxx (reduce dissoc xxx remove-attrs2))
                         (into add-attrs2))
           remove-listeners (-> remove-listeners1
                                (set/union remove-listeners2))
           add-listeners (-> add-listeners1
                             (as-> xxx (reduce dissoc xxx remove-listeners2))
                             (into add-listeners2))
           ;; - Pass 1 -
           ;; 1. Collect the information about the :take operations
           ;; 2. Offset the move-id when needed and add the :size to the :take and :put
           move-id->state1 (make-move-id->state children-ops1 0)
           children-ops1 (update-takes-and-puts children-ops1 move-id->state1 0)

           move-id-offset (count move-id->state1)
           move-id->state2 (make-move-id->state children-ops2 move-id-offset)
           children-ops2 (update-takes-and-puts children-ops2 move-id->state2 move-id-offset)

           ;; - Pass 2 -
           ;; Merge 2 sequences of operations into 1 sequence.
           [move-id->state children-ops] (index-ops-comp move-id->state2 children-ops2
                                                         move-id->state1 children-ops1)

           ;; - Pass 3 -
           ;; Get rid or replace the [:take size id] and [:put size id] operations based on the take states.
           ;; Some Defragmentation may happen on the replaced :take or :put.
           ;; TBD

           ;; - Pass 4 -
           ;; Normalize the sequence of all the children operations.
           children-ops (canonical-children-ops move-id->state children-ops)]
       (-> {}
           (cond->
             (seq remove-attrs) (assoc :remove-attrs remove-attrs)
             (seq add-attrs) (assoc :add-attrs add-attrs)
             (seq remove-listeners) (assoc :remove-listeners remove-listeners)
             (seq add-listeners) (assoc :add-listeners add-listeners)
             (seq children-ops) (assoc :children-ops children-ops))
           not-empty))))
  ([vdom2 vdom1 & more-vdoms]
   (reduce comp (comp vdom2 vdom1) more-vdoms)))


(defn comp-> [& vdoms]
  (apply comp (reverse vdoms)))


(comment
  (comp-> (h/insert 2 (h/hiccup "xxx"))
          (h/insert 4 (h/hiccup "yyy")))

  (comp-> (h/remove-in [0] 1)
          (h/remove-in [0 1 3] 3)
          (h/insert-in [0 1 3]
                       (h/hiccup [:li "aaa"])
                       (h/hiccup [:li "bbb"]))
          (h/insert-in [0 2]
                       (h/hiccup [:p "xxx"])
                       (h/hiccup [:div "yyy"])))

  (vdom/comp (h/move 2 1 4)
             (h/move 2 1 4))

  ,)
