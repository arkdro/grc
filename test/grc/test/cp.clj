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

(deftest fail-occur-test
  (let [colors #{1 2}]
    (is (= true (grc.cp/fail-occur 3 :fail #{})))
    (is (= true (grc.cp/fail-occur 3 colors #{3 4 5 6})))
    (is (= false (grc.cp/fail-occur 3 colors #{3 4})))
    ))

(deftest get-cur-limit-test
  (is (= 2 (grc.cp/get-cur-limit #{3 4}))))

(deftest choose-solution-test
  (is (= 1 (grc.cp/choose-solution 1 nil)))
  (is (= 2 (grc.cp/choose-solution 2 :fail)))
  (is (= 3 (grc.cp/choose-solution nil 3)))
  (is (= 4 (grc.cp/choose-solution :fail 4)))
  (is (= 5 (grc.cp/choose-solution 5 6)))
  (is (= 6 (grc.cp/choose-solution 7 6)))
  (is (= 9 (grc.cp/choose-solution 9 9)))
  )

(deftest feasible-test
  (let [neg-colors :ignored
        used-colors :ignored
        solution 3
        color-limit 5]
    (is (= false (grc.cp/feasible :fail neg-colors used-colors 3 5)))
    (is (= false (grc.cp/feasible #{1 2 3} neg-colors used-colors 3 5)))
    (is (= false (grc.cp/feasible #{1 2 3 4} neg-colors used-colors 3 5)))
    (is (= true (grc.cp/feasible #{1 4} neg-colors used-colors 3 5)))
    ))

(deftest get-new-items-on-add-aux-test-1
  (let [nodes {1 [3 4], 2 [3 4], 3 [1 2], 4 [1 2]}
        colors {1 0, 2 1}
        used-colors #{0 1}
        color 2
        h-nodes [3 4]
        del-items []
        act (grc.cp/get-new-items-on-add-aux nodes
                                             colors
                                             used-colors
                                             color
                                             []
                                             del-items)
        exp [colors used-colors del-items]]
    (is (= exp act))
    ))

(deftest get-new-items-on-add-aux-test-2
  (let [nodes {1 [3 4], 2 [3 4], 3 [1 2], 4 [1 2]}
        colors {1 0, 2 1}
        used-colors #{0 1}
        color 2
        h-nodes [1]
        del-items []
        act (grc.cp/get-new-items-on-add-aux nodes
                                             colors
                                             used-colors
                                             color
                                             h-nodes
                                             del-items)
        ]
    (is (= [:fail] act))
    ))

(deftest get-new-items-on-add-aux-test-3
  (let [nodes {1 [3 4], 2 [3 4], 3 [1 2], 4 [1 2]}
        colors {1 0, 2 1} ;; node -> color
        used-colors #{0 1}
        color 0
        h-nodes [1]
        del-items [:stub]
        act (grc.cp/get-new-items-on-add-aux nodes
                                             colors
                                             used-colors
                                             color
                                             h-nodes
                                             del-items)
        exp [colors used-colors del-items]]
    (is (= exp act))
    ))

;; new node that does not have color assigned
(deftest get-new-items-on-add-aux-test-4
  (let [nodes {1 [3 4], 2 [3 4], 3 [1 2], 4 [1 2]}
        colors {1 0, 2 1} ;; node -> color
        used-colors #{0 1}
        color 2
        node 3
        h-nodes [node]
        del-items [:stub4]
        act (grc.cp/get-new-items-on-add-aux nodes
                                             colors
                                             used-colors
                                             color
                                             h-nodes
                                             del-items)
        exp-colors {1 0, 2 1, 3 2}
        exp-del-items [[color [1 2]] :stub4]
        exp-used-colors #{0 1 2}
        exp [exp-colors exp-used-colors exp-del-items]]
    (is (= exp act))
    ))

(deftest get-new-items-on-add-test-1
  (let [nodes {1 [3 4], 2 [3 4], 3 [1 2], 4 [1 2]}
        colors {}
        used-colors #{}
        act (grc.cp/get-new-items-on-add nodes
                                         colors
                                         used-colors
                                         [])
        ]
    (is (= [{} #{} []] act))
    ))

(deftest get-new-items-on-add-test-2
  (let [nodes {1 [3 4], 2 [3 4], 3 [1 2], 4 [1 2]}
        colors {}
        used-colors #{}
        h-add [0 [1]]
        act (grc.cp/get-new-items-on-add nodes
                                         colors
                                         used-colors
                                         h-add)
        exp-colors {1 0}
        exp-used-colors #{0}
        exp-h-del [[0 [3 4]]]
        exp [exp-colors exp-used-colors exp-h-del]
        ]
    (is (= exp act))
    ))

(deftest new-neighbour-greater-test
  (let [nodes {1 [3 4], 2 [3 4 5], 3 [1 2], 4 [1 2], 5 [2]}]
    (is (= false (grc.cp/new-neighbour-greater 1 3 nodes)))
    (is (= false (grc.cp/new-neighbour-greater 1 2 nodes)))
    (is (= true (grc.cp/new-neighbour-greater 2 4 nodes)))
    ))

(deftest get-next-node-and-colors-aux-test-1
  (is (= [1 2] (grc.cp/get-next-node-and-colors-aux [] {} [] [1 2])))
  )

(deftest get-next-node-and-colors-aux-test-2
  (let [nodes {1 [3 4], 2 [3 4 5], 3 [1 2], 4 [1 2], 5 [2]}
        neg-colors {1 #{0 1 3}
                    2 #{0 1 2 3 4}
                    5 #{1}
                    }
        act (grc.cp/get-next-node-and-colors-aux [2] nodes neg-colors [5 #{1}])
        exp [2 #{0 1 2 3 4}]
        ]
    (is (= exp act))
    ))

(deftest get-next-node-and-colors-aux-test-3
  (let [nodes {1 [3 4], 2 [3 4 5], 3 [1 2], 4 [1 2], 5 [2]}
        neg-colors {1 #{0 1 3}
                    2 #{0 1 2 3 4}
                    5 #{1}
                    }
        act (grc.cp/get-next-node-and-colors-aux [5] nodes neg-colors
                                                 [1 #{0 1 3}])
        exp [1 #{0 1 3}]
        ]
    (is (= exp act))
    ))

(deftest get-next-node-and-colors-aux-test-4
  (let [nodes {1 [3 4], 2 [3 4 5], 3 [1 2 5], 4 [1 2 6], 5 [2 3], 6 [4]}
        neg-colors {1 #{0 1 3}
                    2 #{0 2}
                    3 #{3 4 5}
                    4 #{0 1 2}
                    5 #{1 2}
                    }
        act1 (grc.cp/get-next-node-and-colors-aux [5] nodes neg-colors
                                                  [2 #{0 2}])
        exp1 [2 #{0 2}]
        act2 (grc.cp/get-next-node-and-colors-aux [3] nodes neg-colors
                                                  [1 #{0 1 3}])
        exp2 [3 #{3 4 5}]
        act3 (grc.cp/get-next-node-and-colors-aux [4] nodes neg-colors
                                                  [3 #{3 4 5}])
        exp3 [3 #{3 4 5}]
        ]
    (is (= exp1 act1))
    (is (= exp2 act2))
    (is (= exp3 act3))
    ))

(deftest get-next-node-and-colors-test
  (let [nodes {1 [3 4], 2 [3 4 5], 3 [1 2 5], 4 [1 2 6], 5 [2 3], 6 [4]}
        neg-colors {1 #{0 1 3}
                    2 #{0 2}
                    3 #{3 4 5}
                    4 #{0 1 2}
                    5 #{1 2}
                    }
        colors :not_used
        act (grc.cp/get-next-node-and-colors nodes colors neg-colors)
        exp-new-nodes {1 [3 4], 2 [3 4 5], 4 [1 2 6], 5 [2 3], 6 [4]}
        exp [3 exp-new-nodes #{3 4 5}]
        ]
    (is (= exp act))
    )
  )

