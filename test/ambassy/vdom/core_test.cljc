(ns ambassy.vdom.core-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [ambassy.vdom.core :as vdom]
            [ambassy.vdom.helper :as h]))


(deftest extract-take-id->state-test
  (is (= {}
         (#'vdom/extract-take-id->state (-> (h/remove 2 5) :children-diff))))
  (is (= {0 {:size 5
             :fragments []}}
         (#'vdom/extract-take-id->state (-> (h/move 2 5 10) :children-diff))))
  (is (= {0 {:size 5
             :fragments [[:update [:a :b :c :d :e]]]}}
         (#'vdom/extract-take-id->state [[:no-op 2]
                                         [:update-take [:a :b :c :d :e] 0]
                                         [:no-op 3]
                                         [:put 0]]))))


(deftest index-op-size-test
  (is (= 3 (#'vdom/index-op-size {} [:no-op 3])))
  (is (= 3 (#'vdom/index-op-size {} [:update [1 2 3]])))
  (is (= 3 (#'vdom/index-op-size {} [:remove 3])))
  (is (= 3 (#'vdom/index-op-size {} [:insert [1 2 3]])))
  (is (= 3 (#'vdom/index-op-size {} [:take 3 0])))
  (is (= 3 (#'vdom/index-op-size {} [:update-take [1 2 3] 0])))
  (is (= 3 (#'vdom/index-op-size {0 {:size 3}} [:put 0])))
  (is (= 3 (#'vdom/index-op-size {0 {:updates [1 2 3]}} [:put 0]))))


(deftest index-op-split-test
  (is (= [[:no-op 2] [:no-op 1]]
         (#'vdom/index-op-split {} [:no-op 3] 2)))
  (is (= [[:update ['d 'e]] [:update ['f]]]
         (#'vdom/index-op-split {} [:update ['d 'e 'f]] 2)))
  (is (= [[:remove 2] [:remove 1]]
         (#'vdom/index-op-split {} [:remove 3] 2)))
  (is (= [[:insert ['x 'y]] [:insert ['z]]]
         (#'vdom/index-op-split {} [:insert ['x 'y 'z]] 2)))
  (is (= [[:take 2 'id] [:take 1 'id]]
         (#'vdom/index-op-split {} [:take 3 'id] 2)))
  (is (= [[:update-take ['x 'y] 'id] [:update-take ['z] 'id]]
         (#'vdom/index-op-split {} [:update-take ['x 'y 'z] 'id] 2)))
  (is (= [[:put 0 2] [:put 0 1]]
         (#'vdom/index-op-split {0 {:size 3}} [:put 0] 2))))


(deftest head-split-test
  (is (= [2
          [[:remove 2] [:remove 1] [:no-op 2]]
          [[:no-op 2] [:remove 3]]]
         (#'vdom/head-split {} [[:remove 3] [:no-op 2]]
                            {} [[:no-op 2] [:remove 3]])))
  (is (= [2
          [[:remove 2] [:no-op 2]]
          [[:no-op 2] [:no-op 1] [:remove 3]]]
         (#'vdom/head-split {} [[:remove 2] [:no-op 2]]
                            {} [[:no-op 3] [:remove 3]])))
  (is (= [2
          [[:remove 2] [:no-op 2]]
          [[:no-op 2] [:remove 3]]]
         (#'vdom/head-split {} [[:remove 2] [:no-op 2]]
                            {} [[:no-op 2] [:remove 3]])))
  #_
  (is (= [2
          [[:no-op 2] [:insert [:a :b]]]
          ([:take 2 0] [:take 4 0])]
         (#'vdom/head-split {} [[:no-op 2] [:insert [:a :b]]]
                            {} [[:take 6 0]]))))


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

