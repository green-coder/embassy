(ns ambassy.vdom.core-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [ambassy.vdom.core :as vdom]
            [ambassy.vdom.helper :as h]))


(deftest preprocess-moves-test
  (is (= [{}
          [[:no-op 2] [:remove 5]]]
         (#'vdom/preprocess-moves (-> (h/remove 2 5) :children-diff) 0)))

  (is (= [{0 {:size      5
              :fragments [[:no-op 5]]}}
          [[:no-op 2] [:take 5 0] [:no-op 3] [:put 5 0]]]
         (#'vdom/preprocess-moves (-> (h/move 2 5 10) :children-diff) 0)))

  (is (= [{10 {:size      5
               :fragments [[:no-op 5]]}}
          [[:no-op 2] [:take 5 10] [:no-op 3] [:put 5 10]]]
         (#'vdom/preprocess-moves (-> (h/move 2 5 10) :children-diff) 10)))

  (is (= [{0 {:size      5
              :fragments [[:update [:a :b :c :d :e]]]}}
          [[:no-op 2] [:take 5 0] [:no-op 3] [:put 5 0]]]
         (#'vdom/preprocess-moves [[:no-op 2]
                                   [:update-take [:a :b :c :d :e] 0]
                                   [:no-op 3]
                                   [:put 0]] 0))))


(deftest index-op-size-test
  (is (= 3 (#'vdom/index-op-size [:no-op 3])))
  (is (= 3 (#'vdom/index-op-size [:update [1 2 3]])))
  (is (= 3 (#'vdom/index-op-size [:remove 3])))
  (is (= 3 (#'vdom/index-op-size [:insert [1 2 3]])))
  (is (= 3 (#'vdom/index-op-size [:take 3 0])))
  (is (= 3 (#'vdom/index-op-size [:put 3 0]))))


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


(deftest index-ops-comp-test
  (are [new-take-id->size new-iops
        base-take-id->size base-iops
        expected-result]
    (= expected-result
       (#'vdom/index-ops-comp new-take-id->size new-iops
                              base-take-id->size base-iops))

    {} [[:no-op 2] [:insert [1 2 3]]]
    {} [[:no-op 1] [:remove 2]]
    [{} [[:no-op 1] [:remove 1] [:remove 1] [:no-op 1] [:insert [1 2 3]]]]

    {} [[:no-op 2] [:insert [1 2 3]]]
    {} [[:no-op 2] [:remove 2]]
    [{} [[:no-op 2] [:insert [1 2]] [:insert [3]] [:remove 1] [:remove 1]]]))

    ;;{} [[:no-op 2] [:remove 2]]
    ;;{} [[:take 6 0] [:no-op 4] [:put 0]]
    ;;[{} [:take 2] ,,,]))


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

