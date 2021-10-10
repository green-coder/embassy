(ns embassy.vdom.helper-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [embassy.vdom.core :as vdom]
            [embassy.vdom.helper :as h]))

(deftest hiccup-test
  (testing "h/hiccup discards nil leaves."
    (is (= (h/hiccup [:div "hello, " "world!"])
           (h/hiccup [:div nil nil "hello, " nil "world!"]))))

  (testing "(h/hiccup nil) returns the neutral element of `apply-vdom`, aka `nil`."
    (is (= (h/hiccup nil)
           h/identity-vdom
           nil)))

  (testing "h/hiccup coerces non-nil leaves into strings."
    (is (= (h/hiccup [:div "true"])
           (h/hiccup [:div true])))
    (is (= (h/hiccup [:div "false"])
           (h/hiccup [:div false])))
    (is (= (h/hiccup [:div "123"])
           (h/hiccup [:div 123]))))

  (testing "h/hiccup handles lazy sequences."
    (is (= (h/hiccup [:ul
                      (for [i (range 5)]
                        [:li "Item " i])])
           (h/hiccup [:ul
                      [:li "Item " 0]
                      [:li "Item " 1]
                      (for [i (range 2 4)]
                        [:li "Item " i])
                      [:li "Item " 4]]))))

  (testing "h/hiccup does not validate tag names and ignores the namespace part of keywords."
    (is (= (h/hiccup [:potato])
           (h/hiccup [:extra/potato])))))

(deftest update-test
  (testing "Updates with multiple vdoms are contiguous updates."
    (is (= (h/update 1 (h/hiccup "new this") (h/hiccup "new that"))
           (vdom/comp (h/update 1 (h/hiccup "new this"))
                      (h/update 2 (h/hiccup "new that"))))))

  (testing "Compose a vdom with a vdom update to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:h1 "My title, " "index 0"]
                      [:p "My updated paragraph, " "index 1"]
                      [:p "My updated paragraph, " "index 2"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/update 1
                                (h/hiccup [:p "My updated paragraph, " "index 1"])
                                (h/hiccup [:p "My updated paragraph, " "index 2"])))))))

(deftest insert-test
  (testing "Insertions with multiple vdoms are contiguous insertions."
    (is (= (h/insert 1 (h/hiccup "new this") (h/hiccup "new that"))
           (vdom/comp (h/insert 1 (h/hiccup "new this"))
                      (h/insert 1 (h/hiccup "new that"))))))

  (testing "Compose a vdom with a vdom insert to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:h1 "My title, " "index 0"]
                      [:p "My inserted paragraph X"]
                      [:p "My inserted paragraph Y"]
                      [:p "My paragraph, " "index 1"]
                      [:p "My paragraph, " "index 2"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/insert 1
                                (h/hiccup [:p "My inserted paragraph X"])
                                (h/hiccup [:p "My inserted paragraph Y"])))))))

(deftest remove-test
  (testing "Removals with multiple vdoms are contiguous updates"
    (is (= (h/remove 1 2)
           (vdom/comp (h/remove 1 1)
                      (h/remove 2 1)))))

  (testing "Compose a vdom with a vdom removal to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:h1 "My title, " "index 0"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/remove 1 2))))))

(deftest update-in-test
  (testing "Update-in is equivalent to nested calls to update."
    (is (= (h/update 1 (h/update 2 (h/update 3 (h/hiccup [:div "hi"]))))
           (h/update-in [1 2 3] (h/hiccup [:div "hi"])))))

  (testing "Compose a vdom with a nested vdom update to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:h1 "My title, " "index 0"]
                      [:p "My paragraph, " "index 1"]
                      [:p "My updated paragraph, " "index 2"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/update-in [2 0] (h/hiccup "My updated paragraph, ")))))))

(deftest insert-in-test
  (testing "Insert-in is equivalent to an insert nested into update calls."
    (is (= (h/update 1 (h/update 2 (h/insert 3 (h/hiccup [:div "hi"]))))
           (h/update-in [1 2] (h/insert 3 (h/hiccup [:div "hi"])))
           (h/insert-in [1 2 3] (h/hiccup [:div "hi"])))))

  (testing "Compose a vdom with a nested vdom update to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:h1 "My title, " "index 0"]
                      [:p "My paragraph, " "index 1"]
                      [:p "Blablabla ... " "something" "My paragraph, " "index 2"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/insert-in [2 0] (h/hiccup "Blablabla ... ") (h/hiccup "something")))))))

(deftest remove-in-test
  (testing "Remove-in is equivalent to a remove nested into update calls."
    (is (= (h/update 1 (h/update 2 (h/remove 3 1)))
           (h/update-in [1 2] (h/remove 3 1))
           (h/remove-in [1 2 3] 1))))

  (testing "Compose a vdom with a nested vdom removal to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:h1 "My title, " "index 0"]
                      [:p "My paragraph, " "index 1"]
                      [:p "index 2"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/remove-in [2 0] 1))))))

(deftest move-test
  (testing "Compose a vdom with a child movement to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:p "My paragraph, " "index 2"]
                      [:h1 "My title, " "index 0"]
                      [:p "My paragraph, " "index 1"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/move 0 2 3))))))

(deftest move-in-test
  (testing "Move-in is equivalent to a move nested into update calls."
    (is (= (h/update 1 (h/update 2 (h/move 3 1 0)))
           (h/update-in [1 2] (h/move 3 1 0))
           (h/move-in [1 2 3] 1 0))))

  (testing "Compose a vdom with a nested child movement to build an updated vdom."
    (is (= (h/hiccup [:main
                      [:h1 "index 0" "My title, "]
                      [:p "My paragraph, " "index 1"]
                      [:p "My paragraph, " "index 2"]
                      [:p "My paragraph, " "index 3"]])
           (vdom/comp (h/hiccup [:main
                                 [:h1 "My title, " "index 0"]
                                 [:p "My paragraph, " "index 1"]
                                 [:p "My paragraph, " "index 2"]
                                 [:p "My paragraph, " "index 3"]])
                      (h/move-in [0 0] 1 2))))))
