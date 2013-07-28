(ns grc.cp
  (:use overtone.at-at)
  (:use clojure.tools.logging)
  )

(defn get-next-node-and-colors-aux [[h & t] nodes colors neg-colors
                                    [prev-neg prev-node :as acc]]
  (if (nil? h) acc
      (let [cur-neg (get neg-colors h)
            cur-cnt (count cur-neg)
            prev-cnt (count prev-neg)]
        (cond (nil? acc) (recur t nodes colors neg-colors [cur-neg h])
              (> cur-cnt prev-cnt) (recur t nodes colors neg-colors [cur-neg h])
              (= cur-cnt prev-cnt) (let [cur-nei (count (get nodes h))
                                         prev-nei (count (get nodes prev-node))]
                                     (if (> cur-nei prev-nei)
                                       (recur t nodes colors neg-colors [cur-neg h])
                                       (recur t nodes colors neg-colors acc)))
              :default (recur t nodes colors neg-colors acc)))))

;; get node with smallest domain (in case of equal, use the node that
;; has more neighbours). It's better to maintain the smallest N
;; somewhere inside neg-colors on updating by set-node-color,
;; so time to find would be O(1).
(defn get-next-node-and-colors [nodes colors neg-colors]
  (let [nodes-only (keys nodes)
        [node node-neg-colors] (get-next-node-and-colors-aux
                                nodes-only
                                nodes
                                colors
                                neg-colors
                                nil)
        new-nodes (dissoc nodes node)]
    [node new-nodes node-neg-colors]))

(defn no-more-node-colors [used-colors max-used-colors color-limit]
  (let [cur (count used-colors)]
    (cond (>= cur color-limit) 'true
          (>= cur max-used-colors) 'true
          :default false)))

(defn make-solution [used-colors max-used-colors]
  (let [cur (count used-colors)]
    (if (< cur max-used-colors) cur
        max-used-colors)))

(defn check-node-neg-color [color neg-colors node]
  (let [node-neg-colors (get neg-colors node)]
    (not (contains? node-neg-colors color))))

;; return nodes that still do not have this particular color in neg-colors
(defn filter-nodes-del [neg-colors [color nodes]]
  (map #(check-node-neg-color color neg-colors %) nodes))

;; given an item to del, return updated neg-colors and
;; additional items (of type [color nodes]) for add colors,
;; when after delete there is only one color left
;; TODO: add check for empty node colors and fail early
(defn get-new-items-on-delete [neg-colors h-del]
  (if (nil? h-del) [neg-colors []]
      (let [[color _] h-del
            h-nodes-del (filter-nodes-del neg-colors h-del)
            new-neg-colors (remove-nodes-colors neg-colors color h-nodes-del)
            add-items (find-single-colored-items new-neg-colors h-nodes-del)]
        [new-neg-colors add-items]
        )))

(defn get-new-items-on-add-aux [nodes
                                colors
                                used-colors
                                color [h & t]
                                del-items]
  (if (nil? h) [colors used-colors del-items]
      (let [cur-node-color (get colors h)]
        (if (not (nil? cur-node-color)) (if (= cur-node-color color)
                                          (recur nodes colors
                                                 used-colors color t
                                                 del-items)
                                          :fail)
            (let [item [color (get nodes h)]
                  new-del-items (cons item del-items) ;; or add to the end?
                  new-colors (assoc colors h color)
                  new-used-colors (assoc used-colors color :t)]
              (recur nodes
                     new-colors
                     new-used-colors
                     color
                     t
                     new-del-items))))))

;; given an item to add, return updated *colors and
;; additional items (of type [color nodes]) to delete colors
(defn get-new-items-on-add [nodes
                            colors
                            used-colors
                            h-add]
  (if (nil? h-add) [colors used-colors []]
      (let [[color h-nodes] h-add]
        (get-new-items-on-add-aux nodes
                                  colors
                                  used-colors
                                  color
                                  h-nodes
                                  [])
        )
      )
  )

;; return true if at least one item of neg-colors is filled completely
;; i.e. no available colors for coloring the node left
(defn empty-colors [neg-colors]
  )

;; check:
;; - failed on add
;; - too many colors are used
(defn fail-occur [colors used-colors]
  (cond (= colors :fail) 'true
        (> (count used-colors) color-limit) 'true
        :default 'false))

;; to-add/to-del - list of items: [color nodes]
(defn set-node-color-aux [nodes colors neg-colors used-colors
                          [h-add & rest-add] [h-del & rest-del]]
  (if (and (nil? h-add) (nil? h-del)) [colors neg-colors used-colors]
      (let [[new-neg-colors new-add] (get-new-items-on-delete
                                      neg-colors
                                      h-del)]
        (if (empty-colors new-neg-colors) :fail
            (let [[new-colors new-used-colors new-del] (get-new-items-on-add
                                                        nodes
                                                        colors
                                                        used-colors
                                                        h-add)]
              (if (fail-occur new-colors) :fail
                  (recur nodes new-colors new-neg-colors new-used-colors
                         (concat new-add rest-add)
                         (concat new-del rest-del))))))))

(defn set-node-color [nodes colors neg-colors used-colors]
  (let [item-to-add [color [node]]]
    (set-node-color-aux nodes colors neg-colors used-colors
                        [item-to-add] [])
    )
  )

(defn iter-nodes-aux [[h & t] free-nodes node
                      nodes colors neg-colors
                      used-colors max-used-colors color-limit flag]
  (cond
    (no-more-free-nodes free-nodes) (make-solution)
    (no-more-node-colors used-colors
                         max-used-colors
                         color-limit) (make-solution)
    :default (let [
                   ;; use h-color
                   [new-colors
                    new-neg-colors
                    new-used-colors
                    new-max-used-colors] (set-node-color h node nodes colors
                                                         neg-colors
                                                         used-colors
                                                         max-used-colors
                                                         color-limit flag)]
               (if (feasible new-colors new-neg-colors
                             new-used-colors
                             new-max-used-colors color-limit)
                 (let [[next-node
                        next-free-nodes
                        avail-colors] (get-next-node-and-colors
                                       new-colors new-neg-colors)
                        max-used-colors2 (iter-nodes-aux
                                          avail-colors
                                          next-node next-free-nodes
                                          new-colors new-neg-colors
                                          new-used-colors new-max-used-colors
                                          color-limit flag)]
                   (recur t free-nodes node nodes colors neg-colors
                          used-colors max-used-colors2 color-limit flag)
                   )
                 (recur t free-nodes node nodes colors neg-colors
                        used-colors max-used-colors color-limit flag)))))

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

