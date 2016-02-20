(ns myworld.drawing (:require [quil.core :as q]))

(use 'myworld.engine)
(use 'myworld.sprite)


(defn- distance-blend [color distance]
  (let [v (q/map-range distance 0 500 255 0)]
    (if (> (q/alpha color) 254)
      (q/blend-color color (q/color v v v 240) :multiply)
      color)))


(defn sprite-columns
  "List of stripes indices to draw"
  [img ratio]
  (let [width (:width img) ]
  (map #(q/map-range % 0 (* width ratio) 0 width)
       (range (* ratio width)))))

(defn sprite-stripe
  "Colors of ratio-resized stripe"
  [img x-offset ratio distance]
  (let [height (:height img)
        y-mapper (fn [y] (q/map-range y 0 (* height ratio) 0 height))]
    (map #(-> %
              (y-mapper)
              ((fn [y] (sprite-get-pixel img x-offset y)))
              (distance-blend distance)
              )
         (range (* ratio height)))))

(defn get-wall-endpoints
  "top and bottom Y coordinate of the wall, centered in available space"
  [max-height wall-height midpoint]
  (let [start (- midpoint (/ wall-height 2))
        end (+ start wall-height)
        offset (/ (- max-height wall-height) 2)
        ]
    [start end]))


(defn coord-mapper [size]
  (fn [x y] [(* size x) (* size y)]))


(defn- tex-mapper [tex-height wall-height]
  (fn [wall-y] (/ (* wall-y tex-height) wall-height)))



(defn get-floor-color [tex playerx playery fx fy]
  (let [dist (q/dist fx fy playerx playery)
        cmap #(mod % 64)
        texcolor (q/get-pixel tex (cmap fx) (cmap fy))]
    (distance-blend texcolor dist)
    ; texcolor
    ))

(defn get-dome-line [tex midpoint height angle]
  (map #(q/get-pixel tex (* 2 angle) (- (+ % 120) midpoint)) (range height)))

(defn line-colors
  "return the line (list of colors) to draw from a cast result"
  [{:keys [rot wall-texture floor-texture sky-texture ceiling-texture
           player-height] :as state}
   {:keys [distance frustum hit-direction x y angle] :as cast-result}]
  (let
    [focal (focal-dist frustum)
     height (:height frustum)
     wall-height UNIT ;; const ?
     real-height (* (/ wall-height distance) focal)
     midpoint (:midpoint state (/ height 2))
     [start end] (get-wall-endpoints height real-height midpoint)
     top (max start 0)
     bottom (min end height)
     projected-height (- bottom top)
     x-tex (if (= hit-direction :horizontal) (mod x 64) (mod y 64))
     y-mapper (fn [y] (q/map-range y start end 0 64))
     ;y-mapper (tex-mapper 64 real-height)
     wall-colors (map #(q/get-pixel wall-texture x-tex (y-mapper %))
                      (filter #(< -1 % height) (range start end)))
     wall (let [otd (q/floor (/ (- real-height projected-height) 2))
                dmp (- midpoint (/ height 2))
                top-drop (- otd dmp)
                bottom-drop (- real-height projected-height otd)]
            (->> wall-colors
                 (map (fn [color] (-> color
                                      (distance-blend distance)
                                      )))))

     ; ceiling (take top (repeat (q/color 0 0 30)))
     ; ceiling (get-dome-line sky-texture midpoint top (+ 180 angle))
     ceiling-cast (floor-caster focal (- UNIT player-height) (- angle rot))
     ceiling-points (map #(->> %1
                               (- midpoint)
                               (ceiling-cast)
                               (rotate-vector (- rot))
                               (add-vector [(:x state) (:y state)]))
                         (range 1 (inc top)))
     ceiling (map #(apply get-floor-color ceiling-texture (:x state) (:y state) %)
                 ceiling-points)

     ; prepare values for floor casting
     floor-cast (floor-caster focal player-height (- angle rot))
     floor-points (map #(->> %1
                             (+ (- midpoint))
                             (floor-cast)
                             (rotate-vector (- rot))
                             (add-vector [(:x state) (:y state)]))
                       (range bottom height))
     floor (map #(apply get-floor-color floor-texture (:x state) (:y state) %)
                floor-points)
     ; ceiling (map #(apply get-floor-color ceiling-texture (:x state) (:y state) %)
     ;            floor-points)
     ]
    (concat ceiling wall floor)))


(defn draw-vertical-line!
  "from a list of colors, draw vertical line at x on screen"
  ([x colors] (draw-vertical-line! x colors 0))
  ([x colors y-offset]
   (let [size 4
         to-coord (coord-mapper size)
         draw-rect (fn [x y] (q/rect x y (inc size) (inc size)))
         indexed-clors (map vector (iterate inc 0) colors)]
     (doseq [[y color] indexed-clors]
       (q/fill color)
       (apply draw-rect (to-coord x (+ y y-offset)))))))

(defn draw-square
  [line, column, color]
  (let [S UNIT
        m #(* S %1)]
    (q/stroke (q/color 0 0 0))
    (q/fill color)
    (q/rect (m column) (m line) S S)
    ))

(defn draw-sprite!
  [{:keys [x y frustum rot midpoint] :as state}
   z-buf
   sprite]
  (let [fov (:fov frustum)
        focal (focal-dist frustum)
        angle (sprite-angle rot x y (:x sprite) (:y sprite))
        pos-x (sprite-screen-x frustum angle)
        sp-dist (q/dist x y (:x sprite) (:y sprite))
        ratio (/ focal sp-dist)
        sp-height (:height sprite)
        off-floor (/ (- UNIT sp-height) 2)
        pos-y (+ (* ratio off-floor) (- midpoint (/ (* ratio sp-height) 2)))]
    ; (q/text-size 20)
    ; (q/text (str "z-buf size " (count z-buf)) 10 25)
    ; (q/text (str "Alpha: " (format "%.2f" angle)) 10 50)
    ; (q/text (str "Pos-x: " (format "%.2f" pos-x)) 10 75)
    ; (q/text (str "Dist :" (format "%.2f" sp-dist)) 10 100)
    ; (q/text (str "Ratio :" (format "%.2f" ratio)) 10 125)
    (doseq [[i col] (enumerate (sprite-columns sprite ratio))]
      (if (< sp-dist (nth z-buf (+ pos-x i) 0))
        (draw-vertical-line!
          (+ pos-x i)
          (sprite-stripe sprite col ratio sp-dist)
          (- pos-y (* ratio (:z sprite 0))))))))



(defn draw-minimap
  [{:keys [world frustum rot x y] :as state}]
  (q/background 120)

  (doseq [l (range (/ (world-height world) UNIT))
          c (range (/ (world-width world) UNIT))
          :let
          [color (if (wall? world (* UNIT c) (* UNIT l))
                   (q/color 150)
                   (q/color 255)
                   )]]
      (draw-square l c color))
  (q/fill 0 255 255)
  (let [r 10, o (/ r 2)] (q/rect (- x o) (- y o) r r))
  (let [[dx dy] (forward-vector state 55)]
    (q/line x y (+ dx x) (+ dy y))
    )
  (let [cr (ray-cast state rot)
        r 10, o (/ r 2)]
    (q/rect (- (:x cr) 0) (- (:y cr) o) r r)
    )
  )

(defn draw-hud []
  (q/fill 70)
  (q/rect 0 480 760 160)
  (q/fill 100)
  (q/rect 6 486 760 160)
  (q/fill 130)
  (q/rect 12 492 760 160)

  (q/fill 100)
  (q/rect 25 504 174 120)
  (q/fill 70)
  (q/rect 25 504 168 114)
  )
