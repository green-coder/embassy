(ns ambassy.client.helper
  (:refer-clojure :exclude [update remove update-in comp])
  (:require
    [clojure.core :as cl]
    [clojure.set :as set]
    [clojure.string :as str]
    [camel-snake-kebab.core :as csk]))

(defn hiccup [x]
  (cond
    (vector? x)
    (let [[props & children] (if (map? (second x))
                               (rest x)
                               (cons nil (rest x)))
          expended-children (into []
                                  (mapcat (fn [child]
                                            (cond
                                              (seq? child) child
                                              (nil? child) []
                                              :else [child])))
                                  children)
          attrs (into {}
                      (keep (fn [[k v]]
                              (let [k-str (-> k name csk/->camelCase str/lower-case)]
                                (when-not (str/starts-with? k-str "on")
                                  [k-str
                                   (cond
                                     (vector? v) (->> v
                                                      (mapv name)
                                                      (str/join " "))
                                     (map? v) (->> v
                                                   (map (fn [[k v]]
                                                          (str (name k) ": " (name v) ";")))
                                                   (apply str))
                                     :else (name v))]))))
                      props)
          listeners (into {}
                          (keep (fn [[k v]]
                                  (let [k-str (-> k name csk/->camelCase str/lower-case)]
                                    (when (str/starts-with? k-str "on")
                                      [(subs k-str 2) v]))))
                          props)]
      (cond-> {:tag (name (first x))}
              (seq attrs) (assoc :attrs attrs)
              (seq listeners) (assoc :listeners listeners)
              (seq expended-children) (assoc :children (mapv hiccup expended-children))))

    (keyword? x)
    (name x)

    (nil? x)
    x

    :else
    (str x)))

#_
(hiccup->vdom [:main
               [:h1 {:title "foobar"
                     :class [:c1 :c2]} "hello, " "world " nil 42]
               [:p {:class :foobar
                    :style {:color :red}} "This is a paragraph."]
               [:button {:on-click (fn [event] (js/console.log event))} "Click me"]])

(defn- skip-n-elements [children-diff n-elements]
  (cond-> children-diff
    (> n-elements 0)
    (conj [:no-op n-elements])))

(defn update [index vdom & more-vdoms]
  {:children-diff (-> []
                      (skip-n-elements index)
                      (conj [:update (into [vdom] more-vdoms)]))})

(defn insert [index vdom & more-vdoms]
  {:children-diff (-> []
                      (skip-n-elements index)
                      (conj [:insert (into [vdom] more-vdoms)]))})

(defn remove [index size]
  {:children-diff (-> []
                      (skip-n-elements index)
                      (conj [:remove size]))})

(defn update-in [path vdom & more-vdoms]
  (if (seq path)
    (let [[last-path & reversed-path] (reverse path)]
      (reduce (fn [vdom index]
                (update index vdom))
              (apply update last-path vdom more-vdoms)
              reversed-path))
    ;; Act like the identity function
    vdom))

(defn insert-in [path vdom & more-vdoms]
  (if (seq path)
    (let [last-path (last path)
          butlast-path (butlast path)]
      (update-in butlast-path (apply insert last-path vdom more-vdoms)))
    ;; Replace anything with the given vdom value
    vdom))

(defn remove-in [path size]
  (if (seq path)
    (let [last-path (last path)
          butlast-path (butlast path)]
      (update-in butlast-path (remove last-path size)))
    (if (zero? size)
      ;; The absence of change
      nil
      ;; The great /dev/null operator
      {:tag nil})))

(defn move [from-index size to-index]
  {:children-diff (if (<= from-index to-index)
                    [[:no-op from-index]
                     [:take size 0]
                     [:no-op (- to-index from-index size)]
                     [:put 0]]
                    [[:no-op to-index]
                     [:put 0]
                     [:no-op (- from-index to-index size)]
                     [:take size 0]])})

(declare comp)


(defn- extract-take-id->state [children-diff]
  (into {}
        (keep (fn [[op-type size-or-vdom-diffs id]]
                (when (or (= op-type :take)
                          (= op-type :update-take))
                   [id (-> {:take-index 0
                            :put-index 0}
                           (assoc (if (vector? size-or-vdom-diffs)
                                    :updates
                                    :size)
                                  size-or-vdom-diffs))])))
        children-diff))


;; Copied from the Diffuse library
(defn- index-op-size
  "Returns the size of the operation in number of DOM elements."
  [take-id->state [op arg1 _]]
  (case op
    (:no-op :remove :take)
    arg1

    (:update :insert :update-take)
    (count arg1)

    :put
    (let [take-state (take-id->state arg1)]
      (or (:size take-state)
          (count (:updates take-state))))))


;; Copied from the Diffuse library
(defn- index-op-split
  "Splits an operation into 2 pieces so that the size of the first piece is the given size,
   and then return a vector containing those 2 pieces."
  [take-id->state [op arg1 arg2] size]
  (case op
    (:no-op :remove)
    [[op size]
     [op (- arg1 size)]]

    (:update :insert)
    [[op (subvec arg1 0 size)]
     [op (subvec arg1 size)]]

    ;; FIXME: splitting the :take implies trouble for the :put and the take-id.
    ;; In which situations do we need to split the :take and :put ?
    :take
    [[op size arg2]
     [op (- arg1 size) arg2]]

    ;; FIXME: splitting the :take implies trouble for the :put and the take-id.
    ;; In which situations do we need to split the :take and :put ?
    :update-take
    [[op (subvec arg1 0 size) arg2]
     [op (subvec arg1 size) arg2]]

    ;; FIXME: splitting the :take implies trouble for the :put and the take-id.
    ;; In which situations do we need to split the :take and :put ?
    :put
    [[op arg1 size]
     [op arg1 (- (let [take-state (take-id->state arg1)]
                   (or (:size take-state)
                       (count (:updates take-state))))
                 size)]]))


;; Copied from the Diffuse library
(defn- head-split
  "Transform 2 sequences of index operations so that their first elements
   have the same size."
  [new-take-id->state new-iops
   base-take-id->state base-iops]
  (let [new-iop (first new-iops)
        base-iop (first base-iops)
        new-size (index-op-size new-take-id->state new-iop)
        base-size (index-op-size base-take-id->state base-iop)]
    (cond
      (= new-size base-size)
      [base-size new-iops base-iops]

      (< new-size base-size)
      (let [[base-head base-tail] (index-op-split base-take-id->state base-iop new-size)]
        [new-size new-iops (list* base-head base-tail (rest base-iops))])

      (> new-size base-size)
      (let [[new-head new-tail] (index-op-split new-take-id->state new-iop base-size)]
        [base-size (list* new-head new-tail (rest new-iops)) base-iops]))))

(defn- replace-subvec
  "Returns a vector with a sub-section at position `index` replaced by the vector `sv`."
  [v index sv]
  (reduce (fn [v [index element]]
            (assoc v index element))
          v
          (mapv vector (range index (+ index (count sv))) sv)))

#_(replace-subvec [:a :b :c] 1 [:x])
#_(replace-subvec [:a :b :c] 3 [:x :y :z])

(defn- ensure-take-state-has-updates [state]
  (cond-> state
    (not (contains? state :updates))
    (assoc :updates (vec (repeat (:size state) nil)))))

#_(ensure-take-state-has-updates {:size 3})

;; Copied from the Diffuse library
(defn- index-ops-comp
  "Composes 2 sequences of index operations, and return the result.
   Note 2: the result is not guaranteed to be canonical/normalized."
  [new-take-id->state new-iops
   base-take-id->state base-iops]
  (let [take-id-offset (count base-take-id->state)]
    (loop [output-take-id->state (into base-take-id->state
                                       (map (fn [[take-id state]]
                                              [(+ take-id take-id-offset) state]))
                                       new-take-id->state)
           output []
           new-iops new-iops
           base-iops base-iops]
      (cond
        (empty? base-iops) [output-take-id->state (into output new-iops)]
        (empty? new-iops) [output-take-id->state (into output base-iops)]
        :else (let [[op-size split-new-iops split-base-iops] (head-split new-take-id->state new-iops
                                                                         base-take-id->state base-iops)
                    [new-op new-arg1 new-arg2 :as new-iop] (first split-new-iops)
                    [base-op base-arg1 base-arg2 :as base-iop] (first split-base-iops)]
                (if (or (= new-op :insert)
                        (= new-op :put))
                  (recur output-take-id->state
                         (conj output new-iop)
                         (rest split-new-iops)
                         split-base-iops)
                  (case base-op
                    :remove (recur output-take-id->state
                                   (conj output base-iop)
                                   split-new-iops
                                   (rest split-base-iops))
                    :take (recur (update-in output-take-id->state
                                            [base-arg2 :take-index]
                                            + base-arg1)
                                 (conj output base-iop)
                                 split-new-iops
                                 (rest split-base-iops))
                    :update-take (recur (update-in output-take-id->state
                                                   [base-arg2 :take-index]
                                                   + op-size)
                                        (conj output [:take op-size base-arg2])
                                        split-new-iops
                                        (rest split-base-iops))
                    :no-op (recur output-take-id->state
                                  (conj output (if (= new-op :update-take)
                                                 [:take op-size (+ new-arg2 take-id-offset)]
                                                 new-iop))
                                  (rest split-new-iops)
                                  (rest split-base-iops))
                    :update (case new-op
                              :no-op (recur output-take-id->state
                                            (conj output base-iop)
                                            (rest split-new-iops)
                                            (rest split-base-iops))
                              :update (recur output-take-id->state
                                             (conj output [:update (mapv comp new-arg1 base-arg1)])
                                             (rest split-new-iops)
                                             (rest split-base-iops))
                              :remove (recur output-take-id->state
                                             (conj output new-iop)
                                             (rest split-new-iops)
                                             (rest split-base-iops))
                              :take (recur (update output-take-id->state
                                                   (+ new-arg2 take-id-offset)
                                                   (fn [{:keys [take-index] :as state}]
                                                     (-> state
                                                         (update :take-index + op-size)
                                                         ensure-take-state-has-updates
                                                         (update :updates replace-subvec take-index base-arg1))))
                                           (conj output new-iops)
                                           (rest split-new-iops)
                                           (rest split-base-iops))
                              :update-take (recur (update output-take-id->state
                                                          (+ new-arg2 take-id-offset)
                                                          (fn [{:keys [take-index] :as state}]
                                                            (-> state
                                                                (update :take-index + op-size)
                                                                (update :updates replace-subvec take-index (mapv comp new-arg1 base-arg1)))))
                                                  (conj output [:take op-size (+ new-arg2 take-id-offset)])
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
                              ;; FIXME: :insert then :take -> replace each :put with the insert.
                              ;; PROBLEM: we ommit :take from the output, how will we know that the :put
                              ;;          should be turned into an :insert ?
                              :take (recur (update output-take-id->state
                                                   (+ new-arg2 take-id-offset)
                                                   (fn [{:keys [take-index] :as state}]
                                                     (-> state
                                                         (update :take-index + op-size)
                                                         ;; FIXME: correct it to become an :insert
                                                         #_#_
                                                         ensure-take-state-has-updates
                                                         (update :updates replace-subvec take-index base-arg1))))
                                           output
                                           (rest split-new-iops)
                                           (rest split-base-iops))
                              ;; Moved updated insert
                              :update-take (recur (update output-take-id->state
                                                          (+ new-arg2 take-id-offset)
                                                          (fn [{:keys [take-index] :as state}]
                                                            (-> state
                                                                (update :take-index + op-size)
                                                                ;; FIXME: correct it to become an :insert
                                                                #_#_
                                                                ensure-take-state-has-updates
                                                                (update :updates replace-subvec take-index (mapv comp new-arg1 base-arg1)))))
                                                  output
                                                  (rest split-new-iops)
                                                  (rest split-base-iops)))
                    ;; TODO: ... finish this part
                    :put (case new-op
                           :no-op (recur output-take-id->state
                                         (conj output base-iop)
                                         (rest split-new-iops)
                                         (rest split-base-iops))
                           ;; TODO: apply the update on the source of the :take
                           :update (recur (update output-take-id->state
                                                  base-arg1
                                                  (fn [{:keys [put-index] :as state}]
                                                    (-> state
                                                        (update :put-index + op-size)
                                                        ensure-take-state-has-updates
                                                        (update :updates (fn [updates]
                                                                           (replace-subvec updates
                                                                                           put-index
                                                                                           ;; FIXME: incorrect - go to sleep !
                                                                                           (mapv comp new-arg1 base-arg1)))))))
                                          ;;(conj output [:insert (mapv comp new-arg1 base-arg1)])
                                          (conj output base-iop)
                                          (rest split-new-iops)
                                          (rest split-base-iops))
                           ;; TODO: remove the :take and the :put, insert the :remove in the output.
                           :remove (recur output-take-id->state
                                          output
                                          (rest split-new-iops)
                                          (rest split-base-iops))
                           ;; TODO: remove the base :put and the new :take, make the new put use the base's take-id.
                           :take (recur (update output-take-id->state
                                                (+ new-arg2 take-id-offset)
                                                (fn [{:keys [take-index] :as state}]
                                                  (-> state
                                                      (update :take-index + op-size)
                                                      ;; FIXME: correct it to become an :insert
                                                      #_#_
                                                      ensure-take-state-has-updates
                                                      (update :updates replace-subvec take-index base-arg1))))
                                        output
                                        (rest split-new-iops)
                                        (rest split-base-iops))
                           ;; TODO: update the base :take, remove the base :put and the new :update-take, make new put use the base's take-id.
                           ;; Moved updated insert
                           :update-take (recur (update output-take-id->state
                                                       (+ new-arg2 take-id-offset)
                                                       (fn [{:keys [take-index] :as state}]
                                                         (-> state
                                                             (update :take-index + op-size)
                                                             ;; FIXME: correct it to become an :insert
                                                             #_#_
                                                             ensure-take-state-has-updates
                                                             (update :updates replace-subvec take-index (mapv comp new-arg1 base-arg1)))))
                                               output
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

;; Copied from the Diffuse library
(defn- index-ops-canonical
  "Transform a sequence of index operations into its canonical form.
   The goal is to regroup operations with the same type, as well as to order the
   operations whose order can be reversed so that they are always in the same order.
   It's a kind of normalization process."
  [iops]
  (into []
        (cl/comp (partition-by (cl/comp {:no-op :no-op
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
           take-id->state1 (extract-take-id->state children-diff1)
           take-id->state2 (extract-take-id->state children-diff2)
           children-diff (-> (index-ops-comp take-id->state2 children-diff2
                                             take-id->state1 children-diff1)
                             second ;; <- TMP
                             index-ops-canonical)]
       (-> {}
           (cond->
             (seq remove-attrs) (assoc :remove-attrs remove-attrs)
             (seq add-attrs) (assoc :add-attrs add-attrs)
             (seq remove-listeners) (assoc :remove-listeners remove-listeners)
             (seq add-listeners) (assoc :add-listeners add-listeners)
             (seq children-diff) (assoc :children-diff children-diff))
           not-empty))))
  ([vdom2 vdom1 & more-vdoms]
   (reduce comp (comp vdom2 vdom1) more-vdoms)))

(defn comp-> [& vdoms]
  (apply comp (reverse vdoms)))

(defn comp-in-> [path & vdoms]
  (update-in path (apply comp-> vdoms)))

(defn comp-in [path & vdoms]
  (update-in path (apply comp vdoms)))

(comment

  (update 5 (hiccup "xxx"))
  (update-in [9 5] (hiccup "xxx"))

  (remove 5 2)
  (remove-in [5] 2)

  (insert-in [0 0] (hiccup "xxx"))
  (insert-in [0 1 5] :a)

  (comp-> (insert 2 (hiccup "xxx"))
          (insert 4 (hiccup "yyy")))

  (comp-> (remove-in [0] 1)
          (remove-in [0 1 3] 3)
          (insert-in [0 1 3] (hiccup [:li "aaa"]) (hiccup [:li "bbb"]))
          (insert-in [0 2] (hiccup [:p "xxx"]) (hiccup [:div "yyy"])))

  (comp (move 2 1 4)
        (move 2 1 4))

  ,)
