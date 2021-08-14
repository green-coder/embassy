(ns ambassy.vdom.core
  (:refer-clojure :exclude [comp])
  (:require
    [clojure.core :as cc]
    [clojure.set :as set]
    [ambassy.client.util :as u]))

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
  ;;   :remove-listeners, :add-listeners, :children-diff and :children-moves.

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

   ;; Re-use the vector format (index-op) from Diffuse.
   :children-diff [[:no-op size0]
                   [:update [vdom-diff0 vdom-diff1 ,,,]]
                   [:remove size1]
                   [:insert [vdom0 vdom1 ,,,]]
                   [:take size2 id0]
                   [:update-take [vdom-diff2 vdom-diff3 ,,,] id0]
                   [:put id0]
                   ,,,]}

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
   (defn- extract-take-id->arg1 [children-diff]
     (into {}
           (keep (fn [[op-type size-or-vdom-diffs id]]
                   (when (or (= op-type :take)
                             (= op-type :update-take))
                     [id size-or-vdom-diffs])))
           children-diff)))

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
                     children-diff]} vdom]

         (doseq [attr remove-attrs]
           (-> dom (.removeAttribute attr)))

         (doseq [[k v] add-attrs]
           (-> dom (.setAttribute k v)))

         (doseq [event-type remove-listeners]
           (remove-event-listeners dom event-type))

         (doseq [[event-type event-handler] add-listeners]
           (add-listeners dom event-type event-handler))

         (let [take-id->arg1 (extract-take-id->arg1 children-diff)
               take-id->dom-nodes (-> (reduce (fn [[m index] [op arg1 arg2]]
                                                (case op
                                                  (:no-op :remove) [m (+ index arg1)]
                                                  (:update :insert) [m (+ index (count arg1))]
                                                  :take [(assoc m arg2 (mapv (fn [index]
                                                                               (-> dom-child-nodes (.item index)))
                                                                             (range index (+ index arg1))))
                                                         (+ index arg2)]
                                                  :update-take (do
                                                                 ;; Does the update of the element
                                                                 (dotimes [i (count arg1)]
                                                                   (let [^js child-element     (-> dom-child-nodes (.item (+ index i)))
                                                                         ^js new-child-element (apply-vdom* child-element (nth arg1 i))]
                                                                     (-> child-element (.replaceWith new-child-element))))

                                                                 ;; TODO: simplify the code?
                                                                 [(assoc m arg2 (mapv (fn [index]
                                                                                        (-> dom-child-nodes (.item index)))
                                                                                      (range index (+ index (count arg1)))))
                                                                  (+ index arg2)])
                                                  :put [m (+ index (let [take-arg1 (take-id->arg1 arg1)]
                                                                     (if (vector? take-arg1)
                                                                       (count take-arg1)
                                                                       take-arg1)))]))
                                              [{} 0]
                                              children-diff)
                                      first)]
           (loop [operations (seq children-diff)
                  index 0]
             (when operations
               (let [[op arg1 arg2] (first operations)
                     next-operations (next operations)]
                 (case op
                   :no-op (recur next-operations (+ index arg1))
                   :update (do
                             (dotimes [i (count arg1)]
                               (let [^js child-element     (-> dom-child-nodes (.item (+ index i)))
                                     ^js new-child-element (apply-vdom* child-element (nth arg1 i))]
                                 (-> child-element (.replaceWith new-child-element))))
                             (recur next-operations (+ index (count arg1))))
                   :remove (do
                             (dotimes [_ arg1]
                               (-> dom (.removeChild (-> dom-child-nodes (.item index)))))
                             (recur next-operations index))
                   :insert (do
                             (if (< index (-> dom-child-nodes .-length))
                               (let [node-after (-> dom-child-nodes (.item index))]
                                 (doseq [child-vdom arg1]
                                   (-> dom (.insertBefore (create-dom child-vdom) node-after))))
                               (doseq [child-vdom arg1]
                                 (-> dom (.appendChild (create-dom child-vdom)))))
                             (recur next-operations (+ index (count arg1))))
                   :take (recur next-operations (+ index arg1))
                   :update-take (recur next-operations (+ index (count arg1)))
                   :put (let [^js node-to (-> dom-child-nodes (.item index))
                              take-arg1 (take-id->arg1 arg1)
                              size (if (vector? take-arg1)
                                     (count take-arg1)
                                     take-arg1)
                              nodes-from (take-id->dom-nodes arg1)]
                          (-> node-to .-before (.apply node-to (into-array nodes-from)))
                          (recur next-operations (+ index size))))))))

         dom))))

#?(:cljs
   (defn apply-vdom [^js app-element vdom]
     ;; Ensures that we have a "root" node under app-element.
     (when (zero? (-> app-element .-childNodes .-length))
       (-> app-element (.appendChild (create-dom ""))))

     ;; Apply the vdom
     (let [current-dom-root (-> app-element .-firstChild)
           new-dom-root (apply-vdom* current-dom-root vdom)]
       (-> current-dom-root (.replaceWith new-dom-root)))))



;; ****************************************************************

(declare comp)

;; Different types of fragment:
;;
;; [:no-op size]
;; From splitting the operation into 2.
;;
;; [:update [vdom-diffs0 vdom-diffs1 ,,,]]
;; From :update then :take or :put then :update.
;; At pass 3, the :take is turned into a :update-take.
;;
;; [:remove size]
;; From :put then :remove.
;; At pass 3, the :take is replaced by a :remove and the :put is removed.
;;
;; [:insert [vdom0 vdom1 ,,,]]
;; From :insert then :take.
;; At pass 3, the :take is removed and the :put is replaced by an :insert.
;;
;; [:move size new-take-id] (on the old take state)
;; From :put then :take.
;; At pass 3, ... things become complicated, the old move take operation has to split into multiple take-id
;; around the old part which is put.

;; The output sequence of operations replaced :take, :update-take and :put by this format:
;; - [:take size id]
;; - [:put size id]
(defn- preprocess-moves [iops take-id-offset]
  (let [take-id->state (into {}
                             (keep (fn [[op-type size-or-vdom-diffs take-id]]
                                     (when (or (= op-type :take)
                                               (= op-type :update-take))
                                        [(+ take-id take-id-offset)
                                         (if (vector? size-or-vdom-diffs)
                                           {:size (count size-or-vdom-diffs)
                                            :fragments [[:update size-or-vdom-diffs]]}
                                           {:size size-or-vdom-diffs
                                            :fragments [[:no-op size-or-vdom-diffs]]})])))
                             iops)
        simpler-iops (into []
                           (map (fn [[op-type arg1 arg2 :as iop]]
                                  (case op-type
                                    (:no-op :update :remove :insert)
                                    iop

                                    (:take :update-take)
                                    (let [take-id (+ arg2 take-id-offset)]
                                      [:take (-> take-id take-id->state :size) take-id])

                                    :put
                                    (let [take-in (+ arg1 take-id-offset)]
                                      [:put (-> take-in take-id->state :size) take-in]))))
                           iops)]
    [take-id->state simpler-iops]))


(defn- index-op-size
  "Returns the size of the operation in number of DOM elements."
  [[op arg1 _]]
  (case op
    (:no-op :remove :take :put)
    arg1

    (:update :insert)
    (count arg1)))


(defn- index-op-split
  "Splits an operation into 2 pieces so that the size of the first piece is the given size,
   and then return a vector containing those 2 pieces."
  [[op arg1 arg2] size]
  (case op
    (:no-op :remove)
    [[op size]
     [op (- arg1 size)]]

    (:update :insert)
    [[op (subvec arg1 0 size)]
     [op (subvec arg1 size)]]

    (:take :put)
    [[op size arg2]
     [op (- arg1 size) arg2]]))


(defn- head-split
  "Transforms 2 sequences of index operations so that their first elements have the same size."
  [new-iops base-iops]
  (let [new-iop (first new-iops)
        base-iop (first base-iops)
        new-size (index-op-size new-iop)
        base-size (index-op-size base-iop)]
    (cond
      (= new-size base-size)
      [base-size new-iops base-iops]

      (< new-size base-size)
      (let [[base-head base-tail] (index-op-split base-iop new-size)]
        [new-size new-iops (list* base-head base-tail (rest base-iops))])

      (> new-size base-size)
      (let [[new-head new-tail] (index-op-split new-iop base-size)]
        [base-size (list* new-head new-tail (rest new-iops)) base-iops]))))


(defn- fragment-size [[op-type arg1 _]]
  (case op-type
    (:no-op :remove :move) arg1
    (:update :insert) (count arg1)))


(defn- sub-fragment [[op-type arg1 arg2 :as fragment] index size]
  (when (and (pos? size)
             (<= 0 index)
             (< index (fragment-size fragment)))
    (case op-type
      (:no-op :remove) [op-type size]
      (:update :insert) [op-type (subvec arg1 index (+ index size))]
      :move [op-type size arg2])))


(defn- get-fragment [fragments index size]
  (loop [fragments (seq fragments)
         fragment-index 0]
    (when fragments
      (let [fragment (first fragments)
            fragment-size' (fragment-size fragment)
            int-min-index (max fragment-index index)
            int-max-index (min (+ fragment-index fragment-size') (+ index size))
            f0-size (-> (- (min int-min-index int-max-index) fragment-index) (max 0))
            f1-size (-> (- int-max-index int-min-index) (max 0))
            f1 (sub-fragment fragment f0-size f1-size)]
        (if (some? f1)
          f1
          (recur (next fragments)
                 (+ fragment-index fragment-size')))))))


(defn- update-fragments [fragments index size f]
  (loop [result []
         fragments (seq fragments)
         fragment-index 0]
    (if fragments
      (let [fragment (first fragments)
            fragment-size' (fragment-size fragment)
            int-min-index (max fragment-index index)
            int-max-index (min (+ fragment-index fragment-size') (+ index size))]
        (recur (let [f0-size (-> (- (min int-min-index int-max-index) fragment-index) (max 0))
                     f1-size (-> (- int-max-index int-min-index) (max 0))
                     f2-size (-> (- (+ fragment-index fragment-size') (max int-min-index int-max-index)) (max 0))
                     f0 (sub-fragment fragment 0 f0-size)
                     f1 (some-> (sub-fragment fragment f0-size f1-size) f)
                     f2 (sub-fragment fragment (+ f0-size f1-size) f2-size)]
                 (into result (remove nil?) [f0 f1 f2]))
               (next fragments)
               (+ fragment-index fragment-size')))
      result)))


(defn- index-ops-comp
  "Composes 2 sequences of index operations, and return the result.
   Note 2: the result is not guaranteed to be canonical/normalized."
  [new-take-id->state new-iops
   base-take-id->state base-iops]
  (let [] ;; TODO: remove the empty let.
    (loop [output-take-id->state (into base-take-id->state
                                       (map (fn [[take-id state]]
                                              [take-id (assoc state
                                                         :take-index 0
                                                         :put-index 0)]))
                                       new-take-id->state)
           output []
           new-iops new-iops
           base-iops base-iops]
      (cond
        (empty? base-iops) [output-take-id->state (into output new-iops)]
        (empty? new-iops) [output-take-id->state (into output base-iops)]
        :else (let [[op-size split-new-iops split-base-iops] (head-split new-iops base-iops)
                    [new-op new-arg1 new-arg2 :as new-iop] (first split-new-iops)
                    [base-op base-arg1 base-arg2 :as base-iop] (first split-base-iops)]
                (case new-op
                  :insert
                  (recur output-take-id->state
                         (conj output new-iop)
                         (rest split-new-iops)
                         split-base-iops)

                  :put
                  (recur (update-in output-take-id->state [new-arg2 :put-index] + op-size)
                         (conj output new-iop)
                         (rest split-new-iops)
                         split-base-iops)

                  ;; case's else
                  (case base-op
                    :no-op
                    (recur (case new-op
                             :take (update-in output-take-id->state [new-arg2 :take-index] + op-size)
                             :put (update-in output-take-id->state [new-arg2 :put-index] + op-size)
                             output-take-id->state)
                           (conj output new-iop)
                           (rest split-new-iops)
                           (rest split-base-iops))

                    :remove
                    (recur output-take-id->state
                           (conj output base-iop)
                           split-new-iops
                           (rest split-base-iops))

                    :take
                    (recur (update-in output-take-id->state [base-arg2 :take-index] + op-size)
                           (conj output base-iop)
                           split-new-iops
                           (rest split-base-iops))

                    :update
                    (case new-op
                      :no-op
                      (recur output-take-id->state
                             (conj output base-iop)
                             (rest split-new-iops)
                             (rest split-base-iops))

                      :update
                      (recur output-take-id->state
                             (conj output [:update (mapv comp new-arg1 base-arg1)])
                             (rest split-new-iops)
                             (rest split-base-iops))

                      :remove
                      (recur output-take-id->state
                             (conj output new-iop)
                             (rest split-new-iops)
                             (rest split-base-iops))

                      :take
                      (recur (update output-take-id->state new-arg2
                                     (fn [{:keys [take-index] :as state}]
                                       (-> state
                                           (update :take-index + op-size)
                                           (update :fragments update-fragments take-index op-size
                                                   (fn [[fragment-type fragment-arg]]
                                                     (case fragment-type
                                                       :no-op base-iop
                                                       :update [:update (mapv comp fragment-arg base-arg1)]))))))
                             (conj output new-iops)
                             (rest split-new-iops)
                             (rest split-base-iops)))

                    :insert (case new-op
                              :no-op (recur output-take-id->state
                                            (conj output base-iop)
                                            (rest split-new-iops)
                                            (rest split-base-iops))
                              :update (recur output-take-id->state
                                             (conj output [:insert (mapv comp new-arg1 base-arg1)])
                                             (rest split-new-iops)
                                             (rest split-base-iops))
                              :remove (recur output-take-id->state
                                             output
                                             (rest split-new-iops)
                                             (rest split-base-iops))
                              :take (recur (update output-take-id->state new-arg2
                                                   (fn [{:keys [take-index] :as state}]
                                                     (-> state
                                                         (update :take-index + op-size)
                                                         (update :fragments update-fragments take-index op-size
                                                                 (fn [[fragment-type fragment-arg]]
                                                                   (case fragment-type
                                                                     :no-op base-iop
                                                                     :update [:insert (mapv comp fragment-arg base-arg1)]))))))
                                           (conj output new-iop)
                                           (rest split-new-iops)
                                           (rest split-base-iops)))
                    :put (case new-op
                           :no-op (recur (update-in output-take-id->state [base-arg2 :put-index] + op-size)
                                         (conj output base-iop)
                                         (rest split-new-iops)
                                         (rest split-base-iops))
                           :update (recur (update output-take-id->state base-arg2
                                                  (fn [{:keys [put-index] :as state}]
                                                    (-> state
                                                        (update :put-index + op-size)
                                                        (update :fragments update-fragments put-index op-size
                                                                (fn [[fragment-type fragment-arg]]
                                                                  (case fragment-type
                                                                    :no-op new-iop
                                                                    :update [:update (mapv comp new-arg1 fragment-arg)]))))))
                                          (conj output base-iop)
                                          (rest split-new-iops)
                                          (rest split-base-iops))
                           :remove (recur (update output-take-id->state base-arg2
                                                  (fn [{:keys [put-index] :as state}]
                                                    (-> state
                                                        (update :put-index + op-size)
                                                        (update :fragments update-fragments put-index op-size
                                                                (fn [_] new-iop)))))
                                          (conj output base-iop)
                                          (rest split-new-iops)
                                          (rest split-base-iops))
                           :take (recur (-> output-take-id->state
                                            (update base-arg2
                                                    (fn [{:keys [put-index] :as state}]
                                                      (-> state
                                                          (update :put-index + op-size)
                                                          (update :fragments update-fragments put-index op-size
                                                                  (fn [[base-frag-type base-frag-arg :as base-frag]]
                                                                    (let [new-take-state (output-take-id->state new-arg2)
                                                                          [new-frag-type new-frag-arg :as new-frag] (get-fragment (:fragments new-take-state)
                                                                                                                                  (:take-index new-take-state)
                                                                                                                                  op-size)]
                                                                      (case [new-frag-type base-frag-type]
                                                                        [:no-op :no-op] base-frag
                                                                        [:no-op :update] base-frag
                                                                        [:update :no-op] new-frag
                                                                        [:update :update] [:update (mapv comp new-frag-arg base-frag-arg)])))))))
                                            (update new-arg2
                                                    (fn [{:keys [take-index] :as state}]
                                                      (-> state
                                                          (update :take-index + op-size)
                                                          (update :fragments update-fragments take-index op-size
                                                                  (fn [_] [:do-not-take op-size]))))))
                                        (conj output base-iop new-iop) ;; new-iop will be discarded in the phase 3
                                        (rest split-new-iops)
                                        (rest split-base-iops))))))))))


(defn- transform-orphan-takes-into-removes
  "Returns the operations where any :take which do not have a matching :put is changed into a :remove.
   ... also return a transformed take-id->size.
   This function should be called before index-ops-canonical."
  [take-id->size iops]
  (let [orphan-take-ids (set/difference (set (keys take-id->size))
                                        ;; all the used take-ids
                                        (->> iops
                                             (keep (fn [[op-type arg1]]
                                                     (when (= op-type :put)
                                                       arg1)))
                                             set))
        transformed-iops (into []
                               (map (fn [[op-type arg1 :as operation]]
                                      (if (and (= op-type :take)
                                               (contains? orphan-take-ids arg1))
                                        [:remove arg1]
                                        operation)))
                               iops)
        transformed-take-id->size (apply dissoc take-id->size orphan-take-ids)]
    [transformed-take-id->size transformed-iops]))


(defn- index-ops-canonical
  "Transform a sequence of index operations into its canonical form.
   The goal is to regroup operations with the same type, as well as to order the
   operations whose order can be reversed so that they are always in the same order.
   It's a kind of normalization process."
  [iops]
  (into []
        (cc/comp (partition-by (cc/comp {:no-op :no-op
                                         :update :update
                                         :remove :remsert
                                         :insert :remsert} first))
                 (mapcat (fn [index-ops]
                           (let [op (ffirst index-ops)]
                             (case op
                               :no-op [[op (transduce (map second) + index-ops)]]
                               :update [[op (into [] (mapcat second) index-ops)]]
                               (:remove :insert) (let [{removes :remove
                                                        inserts :insert} (group-by first index-ops)
                                                       remove-count (transduce (map second) + removes)
                                                       insert-elms (into [] (mapcat second) inserts)]
                                                   (cond-> []
                                                     (pos? remove-count) (conj [:remove remove-count])
                                                     (pos? (count insert-elms)) (conj [:insert insert-elms]))))))))
        iops))


(defn comp
  ([vdom] vdom)
  ([vdom2 vdom1]
   (cond
     ;; nil represents a no-change diff
     (nil? vdom1) vdom2
     (nil? vdom2) vdom1

     ;; vdom2 overwrites vdom1
     (contains? vdom2 :tag) vdom2

     ;; vdom2 applies on vdom1's content
     (contains? vdom1 :tag)
     (let [attrs (-> (:attrs vdom1)
                     (reduce dissoc (:remove-attrs vdom2))
                     (into (:add-attrs vdom2)))
           listeners (-> (:listeners vdom1)
                         (reduce dissoc (:remove-listeners vdom2))
                         (into (:add-listeners vdom2)))
           children (loop [children-out []
                           children-in  (:children vdom1)
                           operations   (seq (:children-diff vdom2))]
                      (if operations
                        (let [[op arg] (first operations)
                              next-operations (next operations)]
                          (case op
                            :no-op (recur (into children-out (take arg) children-in)
                                          (subvec children-in arg)
                                          next-operations)
                            :update (recur (into children-out (mapv comp arg children-in))
                                           (subvec children-in (count arg))
                                           next-operations)
                            :remove (recur children-out
                                           (subvec children-in arg)
                                           next-operations)
                            :insert (recur (into children-out arg)
                                           children-in
                                           next-operations)))
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
            children-diff1    :children-diff} vdom1
           {remove-attrs2     :remove-attrs
            add-attrs2        :add-attrs
            remove-listeners2 :remove-listeners
            add-listeners2    :add-listeners
            children-diff2    :children-diff} vdom2
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
           ;; Collect the information about the :take operations
           [take-id->state1 children-diff1] (preprocess-moves children-diff1 0)
           [take-id->state2 children-diff2] (preprocess-moves children-diff2 (count take-id->state1))

           ;; - Pass 2 -
           ;; Merge 2 sequences of operations into 1 sequence.
           ;; Enrich the take states. In the output, [:take size id] and [:put size id].
           [take-id->state children-diff] (index-ops-comp take-id->state2 children-diff2
                                                          take-id->state1 children-diff1)

           ;; - Pass 3 -
           ;; Get rid or replace the [:take size id] and [:put size id] operations based on the take states.
           ;; Some Defragmentation may happen on the replaced :take or :put.
           ;; TBD

           ;; - Pass 4 -
           ;; Defragment the sequence of all the vdom-diff operations.
           defragmented-children-diff (index-ops-canonical children-diff)]
       (-> {}
           (cond->
             (seq remove-attrs) (assoc :remove-attrs remove-attrs)
             (seq add-attrs) (assoc :add-attrs add-attrs)
             (seq remove-listeners) (assoc :remove-listeners remove-listeners)
             (seq add-listeners) (assoc :add-listeners add-listeners)
             (seq children-diff) (assoc :children-diff defragmented-children-diff))
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
