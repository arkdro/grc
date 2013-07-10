(ns grc.cp
  (:use overtone.at-at)
  (:use clojure.tools.logging)
  )

(defn iter-nodes [nodes colors neg-colors flag]
  (let [
        [new-constraints new-nodes new-colors new-neg-colors] (set-one-node
                                                               nodes
                                                               colors
                                                               neg-colors)
        ]
    )
  )

(defn calc-aux [[nodes colors] flag]
  (iter-nodes nodes colors [] flag)
  )

(defn calc [graph time-limit]
  (let [flag (atom :go)
        my-pool (mk-pool)
        _ (after time-limit #(compare-and-set! flag :go :stop) my-pool)
        res (calc-aux graph flag)
        ]
    res)
  )

