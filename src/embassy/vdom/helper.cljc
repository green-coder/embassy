(ns embassy.vdom.helper
  (:refer-clojure :exclude [update remove update-in])
  (:require
    [clojure.string :as str]
    [camel-snake-kebab.core :as csk]
    [embassy.vdom.core :as vdom]
    [embassy.client.util :as u]))

;; This is a vdom which, when applied on a dom element, does not change it.
(def identity-vdom nil)

;; A vdom representing a value to be removed, used for removing a root dom element.
(def removed-vdom {:tag nil})

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
    nil

    :else
    (str x)))

#_
(hiccup->vdom [:main
               [:h1 {:title "foobar"
                     :class [:c1 :c2]} "hello, " "world " nil 42]
               [:p {:class :foobar
                    :style {:color :red}} "This is a paragraph."]
               [:button {:on-click (fn [event] (js/console.log event))} "Click me"]])

(defn- skip-n-elements [children-ops n-elements]
  (cond-> children-ops
    (> n-elements 0)
    (conj {:type :no-op
           :size n-elements})))

(defn update [index vdom & more-vdoms]
  {:children-ops (-> []
                     (skip-n-elements index)
                     (conj {:type :update
                            :elements (into [vdom] more-vdoms)}))})

(defn insert [index vdom & more-vdoms]
  {:children-ops (-> []
                     (skip-n-elements index)
                     (conj {:type :insert
                            :elements (into [vdom] more-vdoms)}))})

(defn remove [index size]
  {:children-ops (-> []
                     (skip-n-elements index)
                     (conj {:type :remove
                            :size size}))})

(defn move [from-index size to-index]
  (when (pos? size)
    {:children-ops (if (<= from-index to-index)
                     (-> []
                         (skip-n-elements from-index)
                         (conj {:type     :take
                                :operations [{:type :no-op
                                              :size size}]
                                :move-id 0})
                         (skip-n-elements (- to-index from-index size))
                         (conj {:type :put
                                :move-id 0}))
                     (-> []
                         (skip-n-elements to-index)
                         (conj {:type :put
                                :move-id 0})
                         (skip-n-elements (- from-index to-index))
                         (conj {:type     :take
                                :operations [{:type :no-op
                                              :size size}]
                                :move-id 0})))}))

(defn update-in [path vdom & more-vdoms]
  (when (seq path)
    (let [[last-path & reversed-path] (reverse path)]
      (reduce (fn [vdom index]
                (update index vdom))
              (apply update last-path vdom more-vdoms)
              reversed-path))))

(defn insert-in [path vdom & more-vdoms]
  (when (seq path)
    (update-in (butlast path) (apply insert (last path) vdom more-vdoms))))

(defn remove-in [path size]
  (when (seq path)
    (update-in (butlast path) (remove (last path) size))))

(defn move-in [path size to-index]
  (when (seq path)
    (update-in (butlast path) (move (last path) size to-index))))

(defn comp-in-> [path & vdoms]
  (update-in path (apply vdom/comp-> vdoms)))

(defn comp-in [path & vdoms]
  (update-in path (apply vdom/comp vdoms)))
