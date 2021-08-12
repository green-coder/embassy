(ns ambassy.client.util
  (:refer-clojure :exclude [get-in]))

#?(:cljs
   (defn get-in
     "Returns a dom element from a sequence of indexes."
     [^js dom-element path]
     (reduce (fn [dom-element index]
               (-> dom-element .-childNodes (.item index)))
             dom-element
             path)))

#_
(get-in (-> js/document (.getElementById "app"))
        [0 0 1 5])


(defn replace-subvec
  "Returns a vector with a sub-section at position `index` replaced by the vector `sv`."
  [v index sv]
  (reduce (fn [v [index element]]
            (assoc v index element))
          v
          (mapv vector (range index (+ index (count sv))) sv)))

#_(replace-subvec [:a :b :c] 1 [:x])
#_(replace-subvec [:a :b :c] 3 [:x :y :z])
