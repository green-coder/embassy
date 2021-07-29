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

(declare comp->)

;; Copied from the Diffuse library
(defn- index-op-size [[op arg]]
  (if (or (= op :update)
          (= op :insert))
    (count arg)
    arg))

;; Copied from the Diffuse library
(defn- index-op-split [[op arg] size]
  (if (or (= op :update)
          (= op :insert))
    [[op (subvec arg 0 size)] [op (subvec arg size)]]
    [[op size] [op (- arg size)]]))

;; Copied from the Diffuse library
(defn- head-split [new-iops base-iops]
  (let [new-iop (first new-iops)
        base-iop (first base-iops)
        new-size (index-op-size new-iop)
        base-size (index-op-size base-iop)]
    (cond
      (= new-size base-size)
      [new-iops base-iops]

      (< new-size base-size)
      (let [[base-head base-tail] (index-op-split base-iop new-size)]
        [new-iops (->> (rest base-iops)
                       (cons base-tail)
                       (cons base-head))])

      (> new-size base-size)
      (let [[new-head new-tail] (index-op-split new-iop base-size)]
        [(->> (rest new-iops)
              (cons new-tail)
              (cons new-head)) base-iops]))))

;; Copied from the Diffuse library
(defn- index-ops-comp [new-iops base-iops]
  (loop [output []
         new-iops new-iops
         base-iops base-iops]
    (cond
      (empty? base-iops) (into output new-iops)
      (empty? new-iops) (into output base-iops)
      :else (let [[split-new-iops split-base-iops] (head-split new-iops base-iops)
                  [new-op new-arg :as new-iop] (first split-new-iops)
                  [base-op base-arg :as base-iop] (first split-base-iops)]
              (if (= new-op :insert)
                (recur (conj output new-iop)
                       (rest split-new-iops)
                       split-base-iops)
                (case base-op
                  :remove (recur (conj output base-iop)
                                 split-new-iops
                                 (rest split-base-iops))
                  :no-op (recur (conj output new-iop)
                                (rest split-new-iops)
                                (rest split-base-iops))
                  :update (case new-op
                            :no-op (recur (conj output base-iop)
                                          (rest split-new-iops)
                                          (rest split-base-iops))
                            :update (recur (conj output [:update (mapv comp-> base-arg new-arg)])
                                           (rest split-new-iops)
                                           (rest split-base-iops))
                            :remove (recur (conj output new-iop)
                                           (rest split-new-iops)
                                           (rest split-base-iops)))
                  :insert (case new-op
                            :no-op (recur (conj output base-iop)
                                          (rest split-new-iops)
                                          (rest split-base-iops))
                            :update (recur (conj output [:insert (mapv comp-> base-arg new-arg)])
                                           (rest split-new-iops)
                                           (rest split-base-iops))
                            :remove (recur output
                                           (rest split-new-iops)
                                           (rest split-base-iops)))))))))

;; Copied from the Diffuse library
(defn- index-ops-canonical [iops]
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

(defn comp->
  ([vdom] vdom)
  ([vdom1 vdom2]
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
                            :update (recur (into children-out (mapv comp-> children-in arg))
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
           children-diff (-> (index-ops-comp children-diff2 children-diff1)
                             index-ops-canonical)]
       (-> {}
           (cond->
             (seq remove-attrs) (assoc :remove-attrs remove-attrs)
             (seq add-attrs) (assoc :add-attrs add-attrs)
             (seq remove-listeners) (assoc :remove-listeners remove-listeners)
             (seq add-listeners) (assoc :add-listeners add-listeners)
             (seq children-diff) (assoc :children-diff children-diff))
           not-empty))))
  ([vdom1 vdom2 & more-vdoms]
   (reduce comp-> (comp-> vdom1 vdom2) more-vdoms)))

(defn comp [& vdoms]
  (apply comp-> (reverse vdoms)))

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

  ,)