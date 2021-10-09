(ns embassy.vdom.core-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [embassy.vdom.core :as vdom]
            [embassy.vdom.helper :as h]))


#_
(deftest index-op-size-test
  (is (= 3 (#'vdom/index-op-size [:no-op 3])))
  (is (= 3 (#'vdom/index-op-size [:update [1 2 3]])))
  (is (= 3 (#'vdom/index-op-size [:remove 3])))
  (is (= 3 (#'vdom/index-op-size [:insert [1 2 3]])))
  (is (= 3 (#'vdom/index-op-size [:take 3 0])))
  (is (= 3 (#'vdom/index-op-size [:put 3 0]))))


#_
(deftest index-op-split-test
  (is (= [[:no-op 2] [:no-op 1]]
         (#'vdom/index-op-split [:no-op 3] 2)))
  (is (= [[:update ['d 'e]] [:update ['f]]]
         (#'vdom/index-op-split [:update ['d 'e 'f]] 2)))
  (is (= [[:remove 2] [:remove 1]]
         (#'vdom/index-op-split [:remove 3] 2)))
  (is (= [[:insert ['x 'y]] [:insert ['z]]]
         (#'vdom/index-op-split [:insert ['x 'y 'z]] 2)))
  (is (= [[:take 2 'id] [:take 1 'id]]
         (#'vdom/index-op-split [:take 3 'id] 2)))
  (is (= [[:put 2 'id] [:put 1 'id]]
         (#'vdom/index-op-split [:put 3 'id] 2))))


#_
(deftest head-split-test
  (is (= [2
          [[:remove 2] [:remove 1] [:no-op 2]]
          [[:no-op 2] [:remove 3]]]
         (#'vdom/head-split [[:remove 3] [:no-op 2]]
                            [[:no-op 2] [:remove 3]])))
  (is (= [2
          [[:remove 2] [:no-op 2]]
          [[:no-op 2] [:no-op 1] [:remove 3]]]
         (#'vdom/head-split [[:remove 2] [:no-op 2]]
                            [[:no-op 3] [:remove 3]])))
  (is (= [2
          [[:remove 2] [:no-op 2]]
          [[:no-op 2] [:remove 3]]]
         (#'vdom/head-split [[:remove 2] [:no-op 2]]
                            [[:no-op 2] [:remove 3]])))
  (is (= [2
          [[:no-op 2] [:insert [:a :b]]]
          [[:take 2 0] [:take 4 0]]]
         (#'vdom/head-split [[:no-op 2] [:insert [:a :b]]]
                            [[:take 6 0]]))))


#_
(deftest get-fragment-test
  (is (= [:no-op 3]
         (#'vdom/get-fragment [[:no-op 6]]
           2 3)))

  (is (= [:update [:c :d :e]]
         (#'vdom/get-fragment [[:update [:a :b :c :d :e :f]]]
           2 3)))

  (is (= [:no-op 2]
         (#'vdom/get-fragment [[:update [:a :b :c :d :e :f]] [:no-op 4]]
           7 2)))

  (is (= [:update [:bb :cc]]
         (#'vdom/get-fragment [[:update [:a :b :c :d :e :f]] [:update [:aa :bb :cc :dd]]]
           7 2)))

  (is (= [:no-op 2]
         (#'vdom/get-fragment [[:update [:a :b :c :d :e :f]] [:no-op 4]]
           8 2)))

  (is (= [:update [:e :f]]
         (#'vdom/get-fragment [[:no-op 4] [:update [:a :b :c :d :e :f]]]
           8 2))))


#_
(deftest update-fragments-test
  (testing "Splits a fragment into 3 pieces, updates the middle piece."
    (is (= [[:no-op 2] [:foobar 3] [:no-op 1]]
           (#'vdom/update-fragments [[:no-op 6]]
                                    2
                                    3
                                    (fn [operation] (assoc operation 0 :foobar))))))

  (testing "Splits into 2 pieces and update the second one."
    (is (= [[:no-op 3] [:foobar 3]]
           (#'vdom/update-fragments [[:no-op 6]]
             3
             3
             (fn [operation] (assoc operation 0 :foobar))))))

  (testing "Updates end of first fragment and beginning of the second one."
    (is (= [[:no-op 4] [:foobar 2] [:foobar 1] [:no-op 3]]
           (#'vdom/update-fragments [[:no-op 6] [:no-op 4]]
             4
             3
             (fn [operation] (assoc operation 0 :foobar))))))

  (testing "Updates end of second fragment and beginning of the third one."
    (is (= [[:no-op 10] [:no-op 2] [:foobar 3] [:no-op 1] [:no-op 4]]
           (#'vdom/update-fragments [[:no-op 10] [:no-op 6] [:no-op 4]]
             12
             3
             (fn [operation] (assoc operation 0 :foobar))))))

  (testing "Updates the second fragment in its entirety."
    (is (= [[:no-op 10] [:foobar 6] [:no-op 4]]
           (#'vdom/update-fragments [[:no-op 10] [:no-op 6] [:no-op 4]]
             10
             6
             (fn [operation] (assoc operation 0 :foobar)))))))


#_
(deftest index-ops-comp-test
  (is (= [{}
          [[:no-op 1] [:remove 1] [:remove 1] [:no-op 1] [:insert [1 2 3]]]]
         (#'vdom/index-ops-comp
           {} [[:no-op 2] [:insert [1 2 3]]]
           {} [[:no-op 1] [:remove 2]])))

  (is (= [{}
          [[:no-op 2] [:insert [1 2]] [:insert [3]] [:remove 1] [:remove 1]]]
         (#'vdom/index-ops-comp
           {} [[:no-op 2] [:insert [1 2 3]]]
           {} [[:no-op 2] [:remove 2]])))

  (is (= [{0 {:size 6
               :fragments []
               :put-index 6
               :take-index 6}}
          [[:take 2 0] [:take 2 0] [:take 2 0] [:no-op 2] [:remove 2] [:put 6 0]]]
         (#'vdom/index-ops-comp
           {}
           [[:no-op 2] [:remove 2]]
           {0 {:size 6
               :fragments []}}
           [[:take 6 0] [:no-op 4] [:put 6 0]])))

  (is (= [{0 {:size       2
              :fragments  [[:update ["x" "y"]]]
              :take-index 2
              :put-index  2}}
          [[:put 2 0] [:no-op 4] [:take 2 0]]]
         (#'vdom/index-ops-comp
           {}
           [[:update ["x" "y"]]]
           {0 {:size 2
               :fragments [[:no-op 2]]}}
           [[:put 2 0] [:no-op 4] [:take 2 0]])))

  (is (= [{0 {:size       2
              :fragments  [[:update ["x" "y"]]]
              :take-index 2
              :put-index  2}}
          [[:take 2 0] [:no-op 4] [:put 2 0]]]
         (#'vdom/index-ops-comp
           {0 {:size 2
               :fragments [[:no-op 2]]}}
           [[:take 2 0] [:no-op 4] [:put 2 0]]
           {}
           [[:update ["x" "y"]]])))

  (is (= [{0 {:size       2
              :fragments  [[:insert ["x" "y"]]]
              :take-index 2
              :put-index  2}}
          [[:take 2 0] [:no-op 4] [:put 2 0]]]
         (#'vdom/index-ops-comp
           {0 {:size 2
               :fragments [[:no-op 2]]}}
           [[:take 2 0] [:no-op 4] [:put 2 0]]
           {}
           [[:insert ["x" "y"]]])))

  (is (= [{0 {:size       2
              :fragments  [[:insert ["xx" "yy"]]]
              :take-index 2
              :put-index  2}}
          [[:take 2 0] [:no-op 4] [:put 2 0]]]
         (#'vdom/index-ops-comp
           {0 {:size 2
               :fragments [[:update ["xx" "yy"]]]}}
           [[:take 2 0] [:no-op 4] [:put 2 0]]
           {}
           [[:insert ["x" "y"]]])))

  ;; TODO: Some information is missing to know how to update the take/put base/new in the next pass.
  (is (= [{0 {:size       2
              :fragments  [[:no-op 2]]
              :take-index 2
              :put-index  2}
           1 {:size 2
              :fragments  [[:do-not-take 2]]
              :take-index 2
              :put-index  2}}
          [[:put 2 1] [:take 2 0] [:no-op 2] [:no-op 2] [:put 2 0] [:take 2 1]]]
         (#'vdom/index-ops-comp
           {1 {:size 2
               :fragments [[:no-op 2]]}}
           [[:put 2 1] [:no-op 4] [:take 2 1]]
           {0 {:size 2
               :fragments [[:no-op 2]]}}
           [[:take 2 0] [:no-op 4] [:put 2 0]]))))


#_
(deftest index-ops-canonical-test
  (are [index-ops expected-result]
    (= expected-result (#'vdom/index-ops-canonical index-ops))

    [[:no-op 1] [:remove 1] [:remove 1] [:no-op 1] [:insert [1 2 3]]]
    [[:no-op 1] [:remove 2] [:no-op 1] [:insert [1 2 3]]]

    [[:no-op 2] [:remove 2] [:insert [1 2]] [:insert [3]]]
    [[:no-op 2] [:remove 2] [:insert [1 2 3]]]

    [[:no-op 2] [:insert [1 2]] [:insert [3]] [:remove 1] [:remove 1]]
    [[:no-op 2] [:remove 2] [:insert [1 2 3]]]

    [[:remove 1] [:insert [1 2]] [:remove 1] [:insert [3]]]
    [[:remove 2] [:insert [1 2 3]]]

    [[:no-op 1] [:no-op 1]]
    [[:no-op 2]]))

(deftest comp-text
  (is (= {:tag "ul",
          :children [{:tag "li", :children ["Item " "0"]}
                     {:tag "li", :children ["Item " "1"]}
                     {:tag "li", :children ["Item " "3"]}
                     {:tag "li", :children ["Foobar"]}
                     {:tag "li", :children ["Item " "2"]}
                     {:tag "li", :children ["Item " "5"]}]}
         (#'vdom/comp {:children-ops [{:type :no-op, :size 2}
                                      {:type :put, :move-id 0}
                                      {:type :no-op, :size 1}
                                      {:type :take
                                       :move-id 0
                                       :operations [{:type :no-op, :size 1}
                                                    {:type :update, :elements [(h/hiccup [:li "Foobar"])]}]}]}
                      (h/hiccup [:ul
                                 (for [i (range 6)]
                                   [:li "Item " i])])))))
