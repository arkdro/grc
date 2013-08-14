(ns grc.test.cp
  (:use [clojure.test])
  (:use clojure.tools.trace)
  ;; (:require [grc.cp])
  )

;; (trace-ns 'grc.cp)

(deftest no-more-node-colors-test
  (is (= false (grc.cp/no-more-node-colors #{5} 2 2)))
  (is (= true (grc.cp/no-more-node-colors #{1 5} 2 2)))
  (is (= true (grc.cp/no-more-node-colors #{1 5} 1 3)))
  )

(deftest no-more-free-nodes-test
  (is (= false (grc.cp/no-more-free-nodes 2 [1 2 3])))
  (is (= false (grc.cp/no-more-free-nodes 2 nil)))
  (is (= true (grc.cp/no-more-free-nodes nil nil)))
  )

(deftest make-solution-test
  (is (= 2 (grc.cp/make-solution #{1 3 5} 2)))
  (is (= 3 (grc.cp/make-solution #{1 3 5} 5)))
  )

(deftest check-node-neg-color-test
  (let [neg-colors {1 #{1 3 4}
                    2 #{2 3 4}}]
    (is (= true (grc.cp/check-node-neg-color 2 neg-colors 1)))
    (is (= false (grc.cp/check-node-neg-color 3 neg-colors 2)))
    ))

(deftest filter-nodes-del-test
  (let [neg-colors {1 #{1 3 4}
                    2 #{2 3 4}}
        color 2
        nodes [1 2 3]
        ]
    (is (= [1 3] (grc.cp/filter-nodes-del neg-colors [2 nodes])))
    (is (= [3] (grc.cp/filter-nodes-del neg-colors [3 nodes])))
    (is (= [2 3] (grc.cp/filter-nodes-del neg-colors [1 nodes])))
    ))

(deftest remove-nodes-colors-test
  (let [neg-colors {1 #{1 3 5}
                    2 #{2 3 4}}
        color 5
        h-nodes-del [1 2 5]
        act (grc.cp/remove-nodes-colors neg-colors color h-nodes-del)
        exp {1 #{1 3 5}
             2 #{2 3 4 5}
             5 #{5}}
        ]
    (is (= exp act))
    ))

(deftest is-node-single-colored-test
  (let [neg-colors {1 #{1 3 5}
                    2 #{1 2 3 4}}
        color-limit 5]
    (is (= false (grc.cp/is-node-single-colored color-limit 1 neg-colors)))
    (is (= true (grc.cp/is-node-single-colored color-limit 2 neg-colors)))
    ))

(deftest find-missing-item-test
  (is (= 2 (grc.cp/find-missing-item 3 #{0 1 3})))
  (is (= nil (grc.cp/find-missing-item 3 #{0 1 2 3})))
  )

(deftest make-one-add-item-test
  (let [
        neg-colors {1 #{0 2 3 4}
                    2 #{1 2 3 4}}
        color-limit 5]
    (is (= [1 [1]] (grc.cp/make-one-add-item color-limit 1 neg-colors)))
    (is (= [0 [2]] (grc.cp/make-one-add-item color-limit 2 neg-colors)))
    (is (= [4 [3]] (grc.cp/make-one-add-item color-limit 3 neg-colors)))
    ))

(deftest find-single-colored-items-test
  (let [neg-colors {1 #{1 3 5}
                    2 #{1 2 3 4}}]
    (is (= [[0 [2]]] (grc.cp/find-single-colored-items 5 neg-colors [1 2 3])))
    ))

(deftest get-new-items-on-delete-test-1
  (let [neg-colors {1 #{0 1 3}
                    2 #{0 1 2 3 4}}
        ]
    (is (= [neg-colors []] (grc.cp/get-new-items-on-delete 5 neg-colors nil)))
    ))

(deftest get-new-items-on-delete-test-2
  (let [neg-colors {1 #{0 1 3}
                    2 #{0 1 2 3 4}}
        h-del [3 [1 2 4]]
        exp [{1 #{0 1 3}
              2 #{0 1 2 3 4}
              4 #{3}}
             []]
        ]
    (is (= exp (grc.cp/get-new-items-on-delete 5 neg-colors h-del)))
    ))

(deftest get-new-items-on-delete-test-3
  (let [neg-colors {1 #{0 1 3}
                    2 #{0 1 2 3 4}}
        h-del [2 [1 2 4]]
        exp [{1 #{0 1 2 3}
              2 #{0 1 2 3 4}
              4 #{2}}
             [[4 [1]]]]
        ]
    (is (= exp (grc.cp/get-new-items-on-delete 5 neg-colors h-del)))
    ))

(deftest filled-neg-colors-for-node-test
  (is (= false (grc.cp/filled-neg-colors-for-node 3 [0 #{1}])))
  (is (= true (grc.cp/filled-neg-colors-for-node 3 [0 #{1 5 7 9}])))
  (is (= true (grc.cp/filled-neg-colors-for-node 3 [0 #{1 5 6}])))
  )

(deftest empty-colors-test
  (let [neg-colors1 {1 #{0 1 3}
                     2 #{0 1 2 3 4}}
        neg-colors2 {1 #{0 3}
                     2 #{1 4}}
        ]
    (is (= true (grc.cp/empty-colors 3 neg-colors1)))
    (is (= nil (grc.cp/empty-colors 3 neg-colors2)))
    ))

