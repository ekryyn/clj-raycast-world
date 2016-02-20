(ns myworld.utils (:require [quil.core :as q]))

(use 'myworld.engine)

(defn get-wall-type [value] 1)

(defn color-to-cell-type [x y color]
  (let [red (int (q/red color))]
    (case red
      0 (get-wall-type red)
      0)))


(defn- read-texture [img pixel-fn]
  (let [w (.width img)
        h (.height img)]
    (map #(apply pixel-fn %)
         (for [y (range h)
               x (range w)]
                 [x y (q/get-pixel img x y)]))))

(defn load-map
  "Build a map from an image"
  [img]
  (let [w (.width img)
        h (.height img)]
    {:width w
     :height h
     :grid (vec (read-texture img color-to-cell-type))}))

(defn get-sprite [sprite-map value]
  (case (int (q/blue value))
    16 (:blue-flower sprite-map)
    32 (:flower sprite-map)
    64 (:bush sprite-map)
    128 (:princess sprite-map)
    nil
    )
  )

(defn load-sprites
  [img sprite-map]
  (remove nil? (read-texture
    img
    (fn [x y color]
      (if-let [sprite (get-sprite sprite-map color)]
        (assoc sprite
               :x (+ (/ UNIT 2) (* x UNIT))
               :y (+ (/ UNIT 2) (* y UNIT)))))))
  )
