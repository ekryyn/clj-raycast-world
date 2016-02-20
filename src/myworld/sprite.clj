(ns myworld.sprite (:require [quil.core :as q]))


(defn create-sprite
  ([tex] (create-sprite tex 1 1))
  ([tex nb-cols] (create-sprite tex nb-cols 1))
  ([tex nb-cols nb-rows]
   {:texture tex
    :width (/ (.width tex) nb-cols)
    :height (/ (.height tex) nb-rows)
    :nb-rows nb-rows
    :nb-cols nb-cols
    :row-index 0
    :col-index 0
    :duration 0.1
    :last-move 0
    }))


(defn sprite-update-time
  [{:keys [row-index col-index nb-cols nb-rows duration last-move] :as sprite}
   seconds]
  (let [next-index (mod (inc col-index) nb-cols)]
    (if (> (- seconds last-move) duration)
      (assoc sprite
             :col-index next-index
             :last-move seconds)
      sprite)))


(defn sprite-get-pixel
  [{:keys [texture width height row-index col-index] :as sprite}
   x y]
  (let [x-offset (* col-index width)
        y-offset (* row-index height)]
    (q/get-pixel texture (+ x x-offset) (+ y y-offset))))
