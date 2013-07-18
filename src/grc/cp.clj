(ns grc.cp
  (:use overtone.at-at)
  (:use clojure.tools.logging)
  )

(defn get-next-node-and-colors-aux [[h & t] colors neg-colors
                                    [prev-avail prev-node :as acc]]
  (if (nil? h) acc
      (let [cur-avail (get neg-colors h)]
        (if (< cur-avail prev-avail) (recur t colors neg-colors [h cur-avail])
            (recur t colors neg-colors acc)))))

;; get node with smallest domain. It's better to maintain the smallest N
;; somewhere inside neg-colors on updating by set-node-color,
;; so time to find would be O(1)
(defn get-next-node-and-colors [nodes colors neg-colors]
  (let [[node avail-colors] (get-next-node-and-colors-aux nodes
                                                          colors
                                                          neg-colors
                                                          nil)
        new-nodes (dissoc nodes node)]
    [node new-nodes avail-colors]))

(defn no-more-node-colors [color color-limit]
  (cond (>= color color-limit) 'true
        (>= color current-solution) 'true
        :default false)
  )

(defn iter-nodes-aux [[h & t] free-nodes node
                      nodes colors neg-colors
                      used-colors max-used-color color-limit flag]
  (cond
    (no-more-free-nodes free-nodes) (make-solution)
    (no-more-node-colors h color-limit) (make-solution)
    :default (let [
                   ;; use h-color
                   [new-colors
                    new-neg-colors
                    new-used-colors
                    new-max-used-color] (set-node-color h node nodes colors
                                                        neg-colors
                                                        used-colors
                                                        max-used-color
                                                        color-limit flag)]
               (if (feasible new-colors new-neg-colors
                             new-used-colors
                             new-max-used-color color-limit)
                 (let [[next-node
                        next-free-nodes
                        avail-colors] (get-next-node-and-colors
                                       new-colors new-neg-colors)
                        max-used-color2 (iter-nodes-aux
                                         avail-colors
                                         next-node next-free-nodes
                                         new-colors new-neg-colors
                                         new-used-colors new-max-used-color
                                         color-limit flag)]
                   (recur t free-nodes node nodes colors neg-colors
                          used-colors max-used-color2 color-limit flag)
                   )
                 (recur t free-nodes node nodes colors neg-colors
                        used-colors max-used-color color-limit flag)))))

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

