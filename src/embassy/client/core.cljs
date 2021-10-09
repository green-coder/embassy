(ns embassy.client.core
  (:require
    [lambdaisland.dom-types]
    [embassy.vdom.core :as vdom]
    [embassy.vdom.helper :as h]
    [embassy.client.util :as u]))

(defn render []
  (doto (-> js/document (.getElementById "app"))
    (vdom/apply-vdom (h/hiccup [:main
                                [:h1 "hello world"]
                                [:p "Something"]]))
    (vdom/apply-vdom (vdom/comp {:children-ops [{:type :no-op, :size 2}
                                                {:type :put, :move-id 0}
                                                {:type :no-op, :size 1}
                                                {:type :take
                                                 :move-id 0
                                                 :operations [{:type :no-op, :size 1}
                                                              {:type :update, :elements [(h/hiccup [:li "Foobar"])]}]}]}
                                (h/hiccup [:ul
                                           (for [i (range 10)]
                                             [:li "Item " i])])))

    #_#_#_
    (vdom/apply-vdom (h/hiccup [:main
                                [:section
                                 [:h1 {:title "foobar"} "hello, " "world"]
                                 [:p {:style {:color :red}} "This is a paragraph."]
                                 [:button {:on-click (fn [event] (js/console.log event))} "Click me"]]
                                [:section
                                 [:p "Things to do"]
                                 [:ul
                                  (for [i (range 10)]
                                    [:li "Item " i])]]]))
    (vdom/apply-vdom (h/remove-in [0] 1))
    (vdom/apply-vdom (h/update-in [0 1]
                                  {:children-ops [{:type :no-op, :size 2}
                                                  {:type :put, :move-id 0}
                                                  {:type :no-op, :size 1}
                                                  {:type :take
                                                   :move-id 0
                                                   :operations [{:type :no-op, :size 1}
                                                                {:type :update, :elements [(h/hiccup [:li "Foobar"])]}]}]}))
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
  (u/get-in (-> js/document (.getElementById "app")) [0 0 1])

  (prn (-> js/document (.getElementById "app")))
  (cljs.pprint/pprint (-> js/document (.getElementById "app")))
  (cljs.pprint/pprint [:div {:id "app"} [:main [:section [:p "Things to do"] [:ul [:li "Item " "0"] [:li "Item " "5"] [:li "Item " "7"] [:li "Item " "6"] [:li "Item " "1"] [:li "Item " "2"] [:li "aaa"] [:li "bbb"] [:li "Item " "8"] [:li "Item " "9"]]] [:p "xxx"] [:div "yyy"]]]),)
