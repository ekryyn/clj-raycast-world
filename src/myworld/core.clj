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
  {:princess (create-sprite (q/load-image "res/sprites/princess.gif") 4)
   :bush (create-sprite (q/load-image "res/sprites/bush.gif"))
   :flower (create-sprite (q/load-image "res/sprites/flower.gif"))
   :blue-flower (create-sprite (q/load-image "res/sprites/blue-flower.gif"))
   :chandelier (create-sprite (q/load-image "res/sprites/chandelier.gif") 4)
   })

(defn init-state []
  {; Surface to draw projection :
   ; frustum for projection
   :frustum default-frustum
   :midpoint-ratio 0.5

   :pressed-keys []

   :font (q/create-font "res/fonts/Volter__28Goldfish_29.ttf" 12 false)
   :textures {:grass (q/load-image "res/tiles/floor.png")
              :stone (q/load-image "res/tiles/stone1.png")
              :brick (q/load-image "res/tiles/wall.png")
              :carpet (q/load-image "res/tiles/carpet.png")
              :planks (q/load-image "res/tiles/ceiling.png")
              }

   :sky-texture (q/load-image "res/domes/moon.png")

   ; set some sprites
   :sprites (load-sprites (q/load-image "world1.png") (sprite-map))
   ; :sprites
   ; [
   ;  (assoc (:bush (sprite-map)) :x 960 :y 96)
   ;  (assoc (:bush (sprite-map)) :x 800 :y 96)
   ;
   ;  ]

   :world (load-wall-map (q/load-image "world1.png"))
   :floors (load-floor-map (q/load-image "world1-floor.png"))
   :ceilings (load-ceiling-map (q/load-image "world1-ceiling.png"))

   :last-frame 0

   :dialogue (create-dialogue "Salut les connards !")

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

(defn play-dialogue
  [{:keys [dialogue] :as state}]
  (if (nil? dialogue)
    state
    (assoc state :dialogue (dialogue-step-letter dialogue (q/millis) 200))
    )
  )

(defn player-action
  "handle main (space bar) action"
  [state]
  (assoc state :dialogue (create-dialogue "Bonjour, je suis vraiment heureux !"))
  )

(defn update-state
  [{:keys [frustum] :as state}]
  (->> state
       ; reset point of view, will move when animated
       ((fn [s] (assoc s
                       :walking? false
                       :midpoint (/ (:height frustum) 2)
                       :player-height (/ UNIT 2))))
       ; process actions
       (play-dialogue)
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
    ;(println k)
    (if (= (name k) " ")
      (player-action state)
      (update state :pressed-keys conj k))
    ))


(defn draw-state
  [{:keys [world frustum rot x y mouse-x mouse-y sprites] :as state}]
  ;(draw-minimap state)
  (q/background 255)
  (q/no-stroke)
  (q/text-font (:font state))

  (let [start (q/millis)
        fov (:fov frustum)
        angles (proj-angles rot (:fov frustum) (:width frustum))
        cast-results (map #(ray-cast state %) angles)
        z-buf (map #(:distance %) cast-results)
        visible-pred #(sprite-visible? rot x y %1 %2 fov)
        visible-sprites (filter #(visible-pred (:x %) (:y %)) sprites)]
    (q/push-matrix)
    (q/scale 4)
    (doseq [[index cr] (enumerate cast-results)
            ;:let [line (line-colors state cr)]
            ]
      ;(draw-vertical-line! index line)
      (draw-stripe! state cr index)
      )
    (q/no-stroke)
    (doseq [sprite
            (reverse (sort-by #(q/dist x y (:x %1) (:y %1)) visible-sprites))]
      (draw-sprite! state z-buf sprite))
    (if (:dialogue state)
      (do
        (q/text-size 8)
        (q/fill 255)
        (q/text (-> state (:dialogue) (:display-text)) 10 100)))
    (q/pop-matrix)
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
  :settings #(q/smooth 0)
  :size [760 700]
  ; setup function called only once, during sketch initialization.
  :setup setup
  :renderer :java2d
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
