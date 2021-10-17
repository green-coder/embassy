(ns embassy.vdom.util)

(defn reduce-right
  ([f init coll]
   (reduce (fn [x y] (f y x))
           init
           (reverse coll)))
  ([f coll]
   (reduce (fn
             ([] (f))
             ([x y] (f y x)))
           (reverse coll))))
