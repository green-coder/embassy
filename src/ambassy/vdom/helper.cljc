(ns ambassy.vdom.helper
  (:refer-clojure :exclude [update remove update-in])
  (:require
    [clojure.string :as str]
    [camel-snake-kebab.core :as csk]
    [ambassy.vdom.core :as vdom]
    [ambassy.client.util :as u]))

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

(defn comp-in-> [path & vdoms]
  (update-in path (apply vdom/comp-> vdoms)))

(defn comp-in [path & vdoms]
  (update-in path (apply vdom/comp vdoms)))

(comment

  (update 5 (hiccup "xxx"))
  (update-in [9 5] (hiccup "xxx"))

  (remove 5 2)
  (remove-in [5] 2)

  (insert-in [0 0] (hiccup "xxx"))
  (insert-in [0 1 5] :a)

  ,)
