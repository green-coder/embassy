(ns ambassy.client.core
  (:require
    [clojure.string :as str]
    [lambdaisland.dom-types]
    [ambassy.client.helper :as h]))

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


(defn- add-event-listener [^js element event-type event-handler]
  (let [listeners (or (-> element .-event-listeners) {})]
    (set! (-> element .-event-listeners)
          (update listeners event-type (fnil conj #{}) event-handler))
    (-> element (.addEventListener event-type event-handler))))

(defn- remove-event-listeners [^js element event-type]
  (let [listeners (or (-> element .-event-listeners) {})]
    (doseq [event-handler (get listeners event-type)]
      (-> element (.removeEventListener event-type event-handler)))
    (set! (-> element .-event-listeners)
          (dissoc listeners event-type))))

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
      node)))

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

      (let [take-id->size (into {}
                                (keep (fn [[op-type id size]]
                                        (when (= op-type :take)
                                          [id size])))
                                children-diff)
            take-id->dom-nodes (-> (reduce (fn [[m index] [op arg1 arg2]]
                                             (case op
                                               (:no-op :remove) [m (+ index arg1)]
                                               (:update :insert) [m (+ index (count arg1))]
                                               :take [(assoc m arg1 (mapv (fn [index]
                                                                            (-> dom-child-nodes (.item index)))
                                                                          (range index (+ index arg2))))
                                                      (+ index arg2)]
                                               :put [m (+ index (take-id->size arg1))]))
                                           [{} 0]
                                           children-diff)
                                   first)]
        (loop [operations (seq children-diff)
               index 0]
          (when operations
            (let [[op arg1 arg2] (first operations) ;; TODO: simplify this destructure
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
                :take (recur next-operations (+ index arg2))
                :put (let [^js node-to (-> dom-child-nodes (.item index))
                           size (take-id->size arg1)
                           nodes-from (take-id->dom-nodes arg1)]
                       (-> node-to .-before (.apply node-to (into-array nodes-from)))
                       (recur next-operations (+ index size))))))))

      dom)))

(defn apply-vdom [^js app-element vdom]
  ;; Ensures that we have a "root" node under app-element.
  (when (zero? (-> app-element .-childNodes .-length))
    (-> app-element (.appendChild (create-dom ""))))

  ;; Apply the vdom
  (let [current-dom-root (-> app-element .-firstChild)
        new-dom-root (apply-vdom* current-dom-root vdom)]
    (-> current-dom-root (.replaceWith new-dom-root))))

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
                   [:take id0 size2]
                   [:put id0]
                   ,,,]}

  ,)

(defn render []
  (doto (-> js/document (.getElementById "app"))
    (apply-vdom (h/hiccup [:main
                           [:section
                            [:h1 {:title "foobar"} "hello, " "world"]
                            [:p {:style {:color :red}} "This is a paragraph."]
                            [:button {:on-click (fn [event] (js/console.log event))} "Click me"]]
                           [:section
                            [:p "Things to do"]
                            [:ul
                             (for [i (range 10)]
                               [:li "Item " i])]]]))
    #_
    (apply-vdom {:children-diff [
                                 ;;[:remove 1]
                                 [:no-op 1]
                                 [:update [#_(h/hiccup [:div "zzz"])
                                           {:children-diff [[:no-op 1]
                                                            [:update [{:children-diff [[:no-op 3]
                                                                                       [:remove 2]
                                                                                       [:insert [(h/hiccup [:li "aaa"])
                                                                                                 (h/hiccup [:li "bbb"])]]]
                                                                       :children-moves [[4 1 8]
                                                                                        [1 7 6]]}]]]}]]
                                 [:insert [(h/hiccup [:p "xxx"])
                                           (h/hiccup [:div "yyy"])]]]})
    (apply-vdom (h/remove-in [0] 1))
    (apply-vdom (h/update-in [0 1] (h/move 2 6 2)))
    ;;(apply-vdom (h/remove-in [0 1 3] 3))
    ;;(apply-vdom (h/insert-in [0 1 3] (h/hiccup [:li "aaa"]) (h/hiccup [:li "bbb"])))
    ;;(apply-vdom (h/insert-in [0 2] (h/hiccup [:p "xxx"]) (h/hiccup [:div "yyy"])))

    #_
    (apply-vdom (h/comp-> (h/remove-in [0] 1)
                          (h/remove-in [0 1 3] 3)
                          (h/insert-in [0 1 3] (h/hiccup [:li "aaa"]) (h/hiccup [:li "bbb"]))
                          (h/insert-in [0 2] (h/hiccup [:p "xxx"]) (h/hiccup [:div "yyy"]))))
    ,)
  ,)


(defn ^:dev/after-load after-load-hook []
  (prn "Restart")
  (render))

(defn run []
  (prn "Start")
  (render))



;; REPL Playground

(comment

  (prn (-> js/document (.getElementById "app")))
  (cljs.pprint/pprint (-> js/document (.getElementById "app")))
  (cljs.pprint/pprint [:div {:id "app"} [:main [:section [:p "Things to do"] [:ul [:li "Item " "0"] [:li "Item " "5"] [:li "Item " "7"] [:li "Item " "6"] [:li "Item " "1"] [:li "Item " "2"] [:li "aaa"] [:li "bbb"] [:li "Item " "8"] [:li "Item " "9"]]] [:p "xxx"] [:div "yyy"]]]),)
