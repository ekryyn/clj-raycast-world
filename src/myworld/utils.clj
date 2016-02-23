(ns myworld.utils
  (:require [quil.core :as q]
            [myworld.engine :as engine :refer [UNIT]]
            ))

(defn color-to-wall-type [x y color]
  (let [red (int (q/red color))]
    (case red
      0 :brick
      222 :window-wall
      200 :stone-wall
      :none)))

(defn color-to-floor-type [x y color]
  (let [red (int (q/red color))]
    (case red
      255 :grass
      128 :stone
      64 :carpet
      :grass)))

(defn color-to-ceiling-type [x y color]
  (let [red (int (q/red color))]
    (case red
      0 :planks
      :planks)))

(defn- read-texture [img pixel-fn]
  (let [w (.width img)
        h (.height img)]
    (map #(apply pixel-fn %)
         (for [y (range h)
               x (range w)]
                 [x y (q/get-pixel img x y)]))))

(defn load-map
  "Build a map from an image"
  [img pixel-fn]
  (let [w (.width img)
        h (.height img)]
    {:width w
     :height h
     :length (* w h)
     :grid (vec (read-texture img pixel-fn))}))

(defn load-wall-map [img] (load-map img color-to-wall-type))

(defn load-floor-map [img] (load-map img color-to-floor-type))

(defn load-ceiling-map [img] (load-map img color-to-ceiling-type))

(defn get-sprite [sprite-map value]
  (case (int (q/blue value))
    8 (:chandelier sprite-map)
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

(defn create-dialogue [text]
  {:remaining-text text
   :display-text ""
   :last-frame 0})

(defn dialogue-step-letter
  [{:keys [text last-frame remaining-text] :as dialogue} t delta]
  (if (> (- t last-frame) delta)
    (let [[next-letter & xs] remaining-text]
      (-> dialogue
          (assoc :remaining-text xs)
          (update :display-text str next-letter)
          (assoc :last-frame t)
          ;#(if (empty? (:remaining-text %)) nil %)
        ))
    dialogue
    )
  )
