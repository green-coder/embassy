(ns ambassy.client.core
  (:require
    [lambdaisland.dom-types]
    [ambassy.vdom.core :as vdom]
    [ambassy.vdom.helper :as h]
    [ambassy.client.util :as u]))

(defn render []
  (doto (-> js/document (.getElementById "app"))
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
    #_
    (vdom/apply-vdom {:children-diff [
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
    (vdom/apply-vdom (h/remove-in [0] 1))
    (vdom/apply-vdom (h/update-in [0 1] {:children-diff [[:no-op 2]
                                                         [:put 0]
                                                         [:no-op 3]
                                                         [:update-take [(h/insert 2 " foo")
                                                                        (h/insert 2 " bar")] 0]]}))
    ;;(apply-vdom (h/update-in [0 1] (h/move 2 2 6)))
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
  (u/get-in (-> js/document (.getElementById "app")) [0 0 1])

  (prn (-> js/document (.getElementById "app")))
  (cljs.pprint/pprint (-> js/document (.getElementById "app")))
  (cljs.pprint/pprint [:div {:id "app"} [:main [:section [:p "Things to do"] [:ul [:li "Item " "0"] [:li "Item " "5"] [:li "Item " "7"] [:li "Item " "6"] [:li "Item " "1"] [:li "Item " "2"] [:li "aaa"] [:li "bbb"] [:li "Item " "8"] [:li "Item " "9"]]] [:p "xxx"] [:div "yyy"]]]),)
