(ns grc.cp
  (:use overtone.at-at)
  (:use clojure.tools.logging)
  )

(declare iter-nodes-aux)

(defn new-neighbour-greater [new prev nodes]
  (let [new-nei (count (get nodes new))
        prev-nei (count (get nodes prev))]
    (> new-nei prev-nei)))

(defn get-next-node-and-colors-aux [[h & t] nodes neg-colors
                                    [prev-node prev-neg :as acc]]
  (if (nil? h) acc
      (let [cur-neg (get neg-colors h)
            new-acc [h cur-neg]]
        (if (nil? acc) (recur t nodes neg-colors new-acc)
            (let [cur-cnt (count cur-neg)
                  prev-cnt (count prev-neg)]
              (cond
                (> cur-cnt prev-cnt) (recur t nodes neg-colors new-acc)
                (= cur-cnt prev-cnt) (if (new-neighbour-greater h
                                                                prev-node
                                                                nodes)
                                       (recur t nodes neg-colors new-acc)
                                       (recur t nodes neg-colors acc))
                :default (recur t nodes neg-colors acc)))))))

;; get node with smallest domain (in case of equal, use the node that
;; has more neighbours). It's better to maintain the smallest N
;; somewhere inside neg-colors on updating by set-node-color,
;; so time to find would be O(1).
(defn get-next-node-and-colors [nodes colors neg-colors]
  (let [nodes-only (keys nodes)
        [node node-neg-colors] (get-next-node-and-colors-aux
                                nodes-only
                                nodes
                                neg-colors
                                nil)
        new-nodes (dissoc nodes node)]
    [node new-nodes node-neg-colors]))

(defn no-more-node-colors [used-colors solution color-limit]
  (let [cur (count used-colors)]
    (cond (>= cur color-limit) 'true
          (>= cur solution) 'true
          :default false)))

(defn no-more-free-nodes [node free-nodes]
  (cond
    (nil? node) 'true
    :default false))

(defn time-is-up [flag]
  (= @flag :stop))

(defn make-solution [used-colors solution]
  (let [cur (count used-colors)]
    (if (< cur solution) cur
        solution)))

(defn check-node-neg-color [color neg-colors node]
  (let [node-neg-colors (get neg-colors node)]
    (not (contains? node-neg-colors color))))

;; return nodes that still do not have this particular color in neg-colors
(defn filter-nodes-del [neg-colors [color nodes]]
  (filter #(check-node-neg-color color neg-colors %) nodes))

(defn store-neg-color [color node neg-colors]
  (let [node-neg-colors (get neg-colors node #{})
        new-node-neg-colors (conj node-neg-colors color)]
    (assoc neg-colors node new-node-neg-colors)))

;; for each of h-nodes store color in neg-colors
(defn remove-nodes-colors [neg-colors color h-nodes-del]
  (reduce #(store-neg-color color %2 %1) neg-colors h-nodes-del)
  )

(defn is-node-single-colored [color-limit node neg-colors]
  (let [node-neg-colors (get neg-colors node)]
    (= (count node-neg-colors) (dec color-limit))))

(defn find-missing-item [x coll]
  (cond (< x 0) nil
        (contains? coll x) (recur (dec x) coll)
        :default x))

(defn make-one-add-item [color-limit node neg-colors]
  (let [node-neg-colors (get neg-colors node)
        missing (find-missing-item (dec color-limit) node-neg-colors)]
    [missing [node]]))

;; find nodes that contain only one not active color in neg-colors
(defn find-single-colored-items [color-limit neg-colors h-nodes-del]
  (let [singles (filter
                 #(is-node-single-colored color-limit % neg-colors)
                 h-nodes-del)]
    (map #(make-one-add-item color-limit % neg-colors) singles)))

;; given an item to del, return updated neg-colors and
;; additional items (of type [color nodes]) for add colors,
;; when after delete there is only one color left
;; TODO: add check for empty node colors and fail early
(defn get-new-items-on-delete [color-limit neg-colors h-del]
  (if (nil? h-del) [neg-colors []]
      (let [[color _] h-del
            h-nodes-del (filter-nodes-del neg-colors h-del)
            new-neg-colors (remove-nodes-colors neg-colors color h-nodes-del)
            add-items (find-single-colored-items
                       color-limit new-neg-colors h-nodes-del)]
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
                                          [:fail] ;; caller expects a list
                                          )
            (let [item [color (get nodes h)]
                  new-del-items (cons item del-items) ;; or add to the end?
                  new-colors (assoc colors h color)
                  new-used-colors (conj used-colors color)]
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

(defn filled-neg-colors-for-node [color-limit [_node node-neg-colors]]
  (<= color-limit (count node-neg-colors))
  )

;; return true if at least one item of neg-colors is filled completely
;; i.e. no available colors for coloring the node left
(defn empty-colors [color-limit neg-colors]
  (some #(filled-neg-colors-for-node color-limit %) neg-colors)
  )

;; check:
;; - failed on add
;; - too many colors are used
(defn fail-occur [color-limit colors used-colors]
  (cond (= colors :fail) 'true
        (> (count used-colors) color-limit) true
        :default false))

;; to-add/to-del - list of items: [color nodes]
;; is used-colors really needed here?
(defn set-node-color-aux [nodes colors neg-colors used-colors
                          color-limit
                          [h-add & rest-add] [h-del & rest-del]]
  (if (and (nil? h-add) (nil? h-del)) [colors neg-colors used-colors]
      (let [[new-neg-colors new-add] (get-new-items-on-delete
                                      color-limit
                                      neg-colors
                                      h-del)]
        (if (empty-colors color-limit new-neg-colors) [:fail]
            (let [[new-colors new-used-colors new-del] (get-new-items-on-add
                                                        nodes
                                                        colors
                                                        used-colors
                                                        h-add)]
              (if (fail-occur color-limit new-colors new-used-colors) [:fail]
                  (recur nodes new-colors new-neg-colors new-used-colors
                         color-limit
                         (concat new-add rest-add)
                         (concat new-del rest-del))))))))

(defn set-node-color [color node
                      nodes colors neg-colors used-colors
                      solution color-limit flag]
  (let [item-to-add [color [node]]]
    (set-node-color-aux nodes colors neg-colors used-colors
                        color-limit
                        [item-to-add] [])
    )
  )

;; get new allowed colors: from 0 to N.
;; 0..N-1 - already used, N - new
;; colors are zero based, so [0, 1] gives a new limit of value 2
(defn get-cur-limit [used-colors]
  (count used-colors))

(defn iter-next-node [free-nodes
                      nodes colors neg-colors
                      used-colors solution color-limit flag]
  (let [[next-node next-free-nodes next-node-neg-colors]
        (get-next-node-and-colors free-nodes colors neg-colors)
        cur-limit (get-cur-limit used-colors)]
    #(iter-nodes-aux 0 cur-limit next-node-neg-colors
                     next-node next-free-nodes nodes colors neg-colors
                     used-colors solution color-limit flag)
    ))

(defn choose-solution [cur next]
  (cond (= next nil) cur
        (= next :fail) cur
        (= cur nil) next
        (= cur :fail) next
        (< next cur) next
        :default cur))

;; check if the size of available colors
;; - smaller than current solution
;; - below the limit
(defn feasible [colors neg-colors used-colors solution color-limit]
  (cond (= colors :fail) 'false
        (>= (count colors) solution) 'false
        (>= (count colors) color-limit) 'false
        :default 'true))

(defn iter-next-color [color cur-limit node-neg-colors
               node free-nodes nodes colors neg-colors
               used-colors solution color-limit flag]
  (let [[new-colors
         new-neg-colors
         new-used-colors] (set-node-color color node nodes colors
                                          neg-colors
                                          used-colors
                                          solution
                                          color-limit flag)]
    (if (feasible new-colors new-neg-colors
                  new-used-colors
                  solution
                  color-limit)
      (let [[next-node
             next-free-nodes
             next-node-neg-colors] (get-next-node-and-colors
                                    free-nodes
                                    new-colors
                                    new-neg-colors)
             new-solution (iter-nodes-aux
                           0
                           (get-cur-limit new-used-colors)
                           next-node-neg-colors
                           next-node next-free-nodes
                           nodes
                           new-colors new-neg-colors
                           new-used-colors solution
                           color-limit flag)
             better-solution (choose-solution solution new-solution)]
        #(iter-nodes-aux (inc color) cur-limit node-neg-colors
               node free-nodes nodes colors neg-colors
               used-colors better-solution color-limit flag)
        ) ;; feasible
      #(iter-nodes-aux (inc color) cur-limit node-neg-colors
             node free-nodes nodes colors neg-colors
             used-colors solution color-limit flag)
      )
    )
  )

(defn iter-nodes-aux [color cur-limit node-neg-colors
                      node free-nodes
                      nodes colors neg-colors
                      used-colors solution color-limit flag]
  (cond
    (no-more-free-nodes node free-nodes) (make-solution used-colors solution)
    (no-more-node-colors used-colors
                         solution
                         color-limit) #(iter-next-node
                                        free-nodes
                                        nodes colors neg-colors
                                        used-colors solution color-limit flag)
    (time-is-up flag) (make-solution used-colors solution)
    (contains? node-neg-colors color) (recur
                                      (inc color) cur-limit node-neg-colors
                                      node free-nodes
                                      nodes colors neg-colors
                                      used-colors solution color-limit flag)
    :default #(iter-next-color
               color cur-limit node-neg-colors
               node free-nodes nodes colors neg-colors
               used-colors solution color-limit flag)
    )
  )

(defn iter-nodes [[nodes colors] color-limit flag]
  (let [
        used-colors #{}
        neg-colors {}
        solution 0
        ;; avail-colors (get-available-colors node colors neg-colors
        ;;                                    solution color-limit)
        [node free-nodes node-neg-colors] (get-next-node-and-colors
                                           nodes
                                           colors
                                           neg-colors)
        ]
    (trampoline iter-nodes-aux 0
                0
                node-neg-colors
                node
                free-nodes
                nodes colors neg-colors
                used-colors solution color-limit flag
                )
    )
  )

(defn calc-aux [graph color-limit flag]
  (iter-nodes graph color-limit flag)
  )

(defn calc [[nodes colors :as graph] time-limit]
  (let [flag (atom :go)
        my-pool (mk-pool)
        _ (after time-limit #(compare-and-set! flag :go :stop) my-pool)
        color-limit (count nodes) ;; FIXME: make it real
        res (calc-aux graph color-limit flag)
        ]
    res)
  )

