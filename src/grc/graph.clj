(ns grc.graph)

(defn store-node [acc [u v]]
  (let [u-prev (get acc u)
        v-prev (get acc v)
        new-acc (assoc acc
                  v (cons u v-prev)
                  u (cons v u-prev))]
    new-acc))

(defn make-graph [{n-nodes :n
                   n-edges :e
                   edges :edges}]
  (let [empty-nodes (vec (repeat n-nodes []))
        colors (vec (repeat n-nodes nil))
        nodes-lst (reduce store-node empty-nodes edges)
        nodes (vec (map vec nodes-lst))
        ]
    [nodes colors])
  )

