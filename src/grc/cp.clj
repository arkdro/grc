(ns grc.cp
  (:use overtone.at-at)
  (:use clojure.tools.logging)
  )

(defn get-available-colors [idx colors neg-colors max-used-color max-total]
  
  )

(defn iter-nodes-aux [[h & t] node
                      nodes colors neg-colors max-used-color color-limit flag]
  (if (no-more-node-colors h) :fail
      (let [[new-colors
             new-neg-colors
             new-max-used-color] (set-node-color nodes colors neg-colors
                                                 max-used-color
                                                 color-limit flag)]
        (if (feasible new-colors new-neg-colors new-max-used-color color-limit)
          ;; use h-color
          (let [next-node (inc node)
                avail-colors (get-available-colors
                              next-node new-colors new-neg-colors)
                res (iter-nodes-aux avail-colors next-node
                                    nodes new-colors new-neg-colors
                                    new-max-used-color color-limit flag)]
            (if (= res :fail) (recur t node nodes colors neg-colors
                                     max-used-color color-limit flag)
                res))
          ;; use next avail color
          (recur t node nodes colors neg-colors
                 max-used-color color-limit flag)))))

(defn iter-nodes [[nodes colors] color-limit flag]
  (let [node 0
        neg-colors []
        max-used-color 0
        avail-colors (get-available-colors node colors neg-colors
                                           max-used-color color-limit)
        ]
    (iter-nodes-aux [avail-colors
                     node
                     nodes colors neg-colors max-used-color color-limit flag]
                    )
    )
  )

(defn calc-aux [[nodes colors] flag]
  (iter-nodes nodes colors [] (count nodes) flag)
  )

(defn calc [graph time-limit]
  (let [flag (atom :go)
        my-pool (mk-pool)
        _ (after time-limit #(compare-and-set! flag :go :stop) my-pool)
        res (calc-aux graph flag)
        ]
    res)
  )

