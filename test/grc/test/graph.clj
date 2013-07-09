(ns grc.test.graph
  (:use [clojure.test])
  (:use clojure.tools.trace)
  (:use [grc.graph])
  )

;; (trace-ns 'grc.graph)

(deftest store-node-test
  (let [
        acc [[] [3 2] [1 5] [1] [] [2]]
        edge [0 5]
        act (grc.graph/store-node acc edge)
        exp [[5] [3 2] [1 5] [1] [] [0 2]]
        ]
  (is (= exp act))))

