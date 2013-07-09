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

(deftest make-graph-test
  (let [
        edges [[0 1]
               [1 2]
               [0 2]
               [1 3]
               [2 3]]
        data {:n 4 :e 5 :edges edges}
        act (grc.graph/make-graph data)
        exp-nodes [[1 2]
                   [0 2 3]
                   [0 1 3]
                   [1 2]]
        exp-colors [nil nil nil nil]
        exp [exp-nodes exp-colors]
        ]
  (is (= exp act))))

