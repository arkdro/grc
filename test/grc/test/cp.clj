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

