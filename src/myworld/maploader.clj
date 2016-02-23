(ns myworld.maploader
  (:require [clojure.data.json :as json]
            [quil.core :as q]
            [myworld.sprite :refer :all]
            ))

(def map-location "res/maps/")

(defn tile-filenames
  "Get a list of unique filenames from a world-map"
  [world-map]
  (->> world-map
       (:tilesets)
       (map #(:tiles %))
       (map vals)
       (flatten)
       (map :image)
       (set)))

(defn read-map [filename]
  (let [path (str map-location filename)]
    (json/read-str (slurp path) :key-fn keyword)))

(defn tile-textures
  "Load textures in a map indexed by path"
  [world-map]
  (let [map-entry #(vector % (q/load-image (str map-location %)))]
    (into {} (map map-entry (tile-filenames world-map)))))


(defn tiles-from-tileset [tileset, textures]
  (let [start (:firstgid tileset)]
    (into
      {}
      (for [[k v] (seq (:tiles tileset))
            :let [tile-id (read-string (name k))
                  image (:image v)]
            ]
        [(+ start tile-id) ,
         {:texture (get textures image)}]
        ))))

(defn world-tiles
  "create a vector of tiles"
  [world-map, textures]
  (reduce merge (for [ts (:tilesets world-map)]
                  (tiles-from-tileset ts textures))))

(defn get-world-layer [world-map layer-name]
  (first (filter #(= layer-name (:name %)) (:layers world-map))))

(defn- object-to-sprite [sprite-map, {obj-name :name, :keys [x y] :as object}]
  (let [sprite-key (keyword obj-name)
        sprite (sprite-key sprite-map)]
    (assoc sprite :x x :y y)))

(defn get-world-sprites [world-map sprite-map layer-name]
  (let [layer (get-world-layer world-map layer-name)]
    (map #(object-to-sprite sprite-map %) (:objects layer))))
