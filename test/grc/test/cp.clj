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

