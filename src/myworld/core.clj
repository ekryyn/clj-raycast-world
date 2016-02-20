(ns myworld.core
  (:gen-class)
  (:require [quil.core :as q]
            [quil.middleware :as m]
            ))

(use 'myworld.engine
     'myworld.drawing
     'myworld.sprite
     'myworld.utils
     )


(def default-frustum
  "Projection attributes"
  {:fov (q/radians 60) ,
   :width 190,
   :height 120})

(defn sprite-map []
  {:princess (create-sprite (q/load-image "princess.gif") 4)
   :bush (create-sprite (q/load-image "bush.gif"))
   :flower (create-sprite (q/load-image "flower.gif"))
   :blue-flower (create-sprite (q/load-image "blue-flower.gif"))
   })

(defn init-state []
  {; Surface to draw projection :
   ; frustum for projection
   :frustum default-frustum
   :midpoint-ratio 0.5

   :pressed-keys []
   :wall-texture (q/load-image "wall.png")
   :floor-texture (q/load-image "floor.png")
   :sky-texture (q/load-image "moon.png")
   :ceiling-texture (q/load-image "ceiling.png")

   ; set some sprites
   :sprites (load-sprites (q/load-image "world1.png") (sprite-map))
   ; :sprites
   ; [
   ;  (assoc (:bush (sprite-map)) :x 960 :y 96)
   ;  (assoc (:bush (sprite-map)) :x 800 :y 96)
   ;
   ;  ]

   ; set a map
   :world (load-map (q/load-image "world1.png"))

   :last-frame 0

   ; player position :
   :player-height (/ UNIT 2)
   :x 736, :y 1000
   :rot 0
   })

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (init-state))

(defn- move-player! [state direction]
  (let
    [speed (if (= :up direction) (/ UNIT 5) (/ UNIT -5))
     [dx dy] (forward-vector state speed)]
    (-> state (update :x + dx) (update :y + dy))))


(defn move! [{:keys [pressed-keys] :as state}]
  (if-let [dir (first (filter #{:up :down} pressed-keys))]
    (-> state
        (move-player! dir)
        (assoc :walking? true)
        )
    state))

(defn- rotate-and-normalize [state delta]
  (-> state
      (update :rot + delta)
      (update :rot normalize-angle)))

(defn rotate! [{:keys [pressed-keys] :as state}]
  (cond
    (some #{:left} pressed-keys) (rotate-and-normalize state 0.2)
    (some #{:right} pressed-keys) (rotate-and-normalize state -0.2)
    :else state))

(defn lift-player [state offset]
  (-> state
      ; (update :player-height + offset)
      (update :midpoint + offset)
    ))

(defn walk-head! [state]
  (if (:walking? state)
    (let [seconds (q/floor (* 0.015 (q/millis)))
          offset (* 1.7 (q/sin seconds))]
      (lift-player state offset))
    state
    ))

(defn jump-and-crouch! [{:keys [pressed-keys] :as state}]
  (if (some #{:a :z} pressed-keys)
    (let [offset (if (some #{:a} pressed-keys) 50 -50)]
      (lift-player state offset))
    state)
  )

(defn look-up-an-down! [{:keys [pressed-keys] :as state}]
  (if (some #{:w :s} pressed-keys)
    (update state :midpoint * (if (some #{:w} pressed-keys) 1.5 0.5))
    state))

(defn animate-sprites [{:keys [sprites] :as state}]
  (assoc state
         :sprites (map #(sprite-update-time % (* 0.001 (q/millis)))
                       sprites))) 

(defn update-state
  [{:keys [frustum] :as state}]
  (->> state
       ; reset point of view, will move when animated
       ((fn [s] (assoc s
                       :walking? false
                       :midpoint (/ (:height frustum) 2)
                       :player-height (/ UNIT 2))))
       ; process actions
       (look-up-an-down!)
       (jump-and-crouch!)
       (rotate!)
       (move!)
       (walk-head!)
       (animate-sprites)
       ))


(defn mouse-moved [state event]
  state
  )

(defn key-pressed [state event]
  (let [k (:key event)]
    (update state :pressed-keys conj k)))


(defn draw-state
  [{:keys [world frustum rot x y mouse-x mouse-y sprites] :as state}]
  ;(draw-minimap state)
  (q/background 255)
  (q/no-stroke)

  (let [start (q/millis)
        fov (:fov frustum)
        angles (proj-angles rot (:fov frustum) (:width frustum))
        cast-results (map #(ray-cast state %) angles)
        z-buf (map #(:distance %) cast-results)
        visible-pred #(sprite-visible? rot x y %1 %2 fov)
        visible-sprites (filter #(visible-pred (:x %) (:y %)) sprites)]
    (doseq [[index cr] (enumerate cast-results)
            :let [line (line-colors state cr)]]
      (draw-vertical-line! index line))
    (q/no-stroke)
    (doseq [sprite
            (reverse (sort-by #(q/dist x y (:x %1) (:y %1)) visible-sprites))]
      (draw-sprite! state z-buf sprite))
    (q/text-size 24)
    (q/fill (q/color 0 255 255))
    (q/text (str (format "%.2f" (/ 1.0 (* 0.001 (- (q/millis) start)))) " FPS")
            10 30)
    ; (q/text (str "pos: " x ", " y) 10 60)
    )
  (draw-hud)
  ; (draw-minimap state)
)

(defn key-released [{:keys [pressed-keys] :as state}]
  (assoc state :pressed-keys (vec (remove (fn [k] (= k (q/key-as-keyword))) pressed-keys))))

(defn -main [& args] (q/sketch
  :title "You spin my circle right round"
  :size [760 700]
  :renderer :java2d
  ; setup function called only once, during sketch initialization.
  :setup setup
  :host "canvas-id"
  :mouse-moved mouse-moved
  :key-pressed key-pressed
  :key-released key-released
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]))
