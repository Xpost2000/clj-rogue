(ns clrogue.graph-search)
;; or other graph related things I guess.

(defn breadth-first-search [graph start end neighbors-fn]
  (loop [frontier (into [] [start])
         visited {}
         origins {}]
    (letfn [(trace-path []
              (vec (reverse (loop [path-total [] cursor end]
                              (if-let [parent (get origins cursor)]
                                (recur (conj path-total cursor) parent)
                                (conj path-total start))))))]
      (let [current-node (peek frontier)]
        (if (= current-node end) (trace-path)
            (when-not (empty? frontier)
              (let [neighbors
                    (filter (fn [neighbor]
                              (and (not (some #(= % neighbor) frontier))
                                   (not (get visited neighbor))))
                            (neighbors-fn graph current-node))]
                (recur (vec (into (drop-last frontier) neighbors))
                       (assoc visited current-node true)
                       (reduce #(assoc %1 %2 current-node)
                               origins neighbors)))))))))
