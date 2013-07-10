(ns grc.cp)

(defn iter-nodes [nodes colors neg-colors flag]
  (let [
        [new-constraints new-nodes new-colors new-neg-colors] (set-one-node
                                                               nodes
                                                               colors
                                                               neg-colors)
        ]
    )
  )

(defn calc [[nodes colors] flag]
  (iter-nodes nodes colors [] flag)
  )

(defn timed-calc [graph time-limit]
  (let [flag (atom :go)
        my-pool (mk-pool)
        _ (after time-limit #(compare-and-set! flag :go :stop) my-pool)
        res (calc graph flag)
        ]
    res)
  )

