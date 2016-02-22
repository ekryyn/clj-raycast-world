(ns myworld.drawing (:require [quil.core :as q]))

(use 'myworld.utils)
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

;(defn draw-sprite-stripe!
;  [img x-offset ratio distance column-index]
;  (let [height (:height img)
;        line (sprite-get-stripe img column-index)
;        ]
;    (q/resize line PIXEL (* ratio height PIXEL))
;    )
;  )

(defn get-wall-endpoints
  "top and bottom Y coordinate of the wall, centered in available space"
  [max-height wall-height midpoint]
  (let [start (- midpoint (/ wall-height 2.0))
        end (+ start wall-height)
        offset (/ (- max-height wall-height) 2.0)
        ]
    [start end]))



(defn- tex-mapper [tex-height wall-height]
  (fn [wall-y] (/ (* wall-y tex-height) wall-height)))


(defn get-floor-color [floor-map textures playerx playery fx fy]
  (let [dist (q/dist fx fy playerx playery)
        c (q/floor  (/ fx UNIT))
        r (q/floor  (/ fy UNIT))
        tex-type (world-cell floor-map r c)
        tex (tex-type textures)
        cmap #(mod % 64)
        texcolor (q/get-pixel tex (cmap fx) (cmap fy))
        ]
    (distance-blend texcolor dist)
    ;texcolor
    ))

; (get-dome-line sky-texture midpoint top (+ Math/PI angle))
(defn draw-dome-stripe! [tex midpoint height angle column-index]
  (let [x-offset (mod (* 120 angle) 720)
        y-offset (- 120 midpoint)
        line (q/get-pixel tex x-offset y-offset 1 (inc height))]
    ;(q/resize line 0 height)
    (q/image line column-index 0)))

(defn draw-wall-stripe! [tex x-offset x y height distance]
  (let [line (q/get-pixel tex x-offset 0 1 64)
        ratio (/ (inc height) 64.0)
        xpos x
        ypos y
        ysize (inc height)
        dark (q/map-range distance 0 500 0 255)
        ]
    ;(q/resize line 1 ysize)
    (q/push-matrix)
    (q/scale 1 ratio)
    (q/image line xpos (/ ypos ratio))
    (q/pop-matrix)
    (q/fill (q/color 0 0 0 dark))
    (q/rect xpos (dec ypos) 1 (inc ysize))
    ))

(defn draw-vertical-line!
  "from a list of colors, draw vertical line at x on screen"
  ([x colors] (draw-vertical-line! x colors 0))
  ([x colors y-offset]
   (let [
         draw-rect (fn [x y] (q/rect x y 2 1))
         indexed-clors (map vector (iterate inc 0) colors)]
     (doseq [[y color] indexed-clors]
       (q/fill color)
       (draw-rect x (+ y y-offset))))))

(defn draw-ceiling!
  [{:keys [frustum player-height rot midpoint ceilings textures] :as state}
   column-index angle y-start y-end
   ]
  (let [
    focal (focal-dist frustum)
    floor-cast (floor-caster focal (- UNIT player-height) (- angle rot))
    floor-points (map #(->> %1
                            (- midpoint)
                            (floor-cast)
                            (rotate-vector (- rot))
                            (add-vector [(:x state) (:y state)]))
                      (range y-start y-end))
    floor (map #(apply get-floor-color ceilings textures (:x state) (:y state) %)
              floor-points)]
    (draw-vertical-line! column-index floor y-start)
    ))

(defn draw-floor!
  [{:keys [frustum player-height rot midpoint floors textures] :as state}
   column-index angle y-start y-end
   ]
  (let [
    focal (focal-dist frustum)
    floor-cast (floor-caster focal player-height (- angle rot))
    floor-points (map #(->> %1
                            (+ (- midpoint))
                            (floor-cast)
                            (rotate-vector (- rot))
                            (add-vector [(:x state) (:y state)]))
                      (range y-start y-end))
    floor (map #(apply get-floor-color floors textures (:x state) (:y state) %)
              floor-points)]
    (draw-vertical-line! column-index floor y-start)
    ))

(defn draw-stripe!
  [{:keys [rot textures sky-texture floors ceilings
           player-height world] :as state}
   {:keys [distance frustum hit-direction x y angle] :as cast-result}
   column-index]
  (let
    [focal (focal-dist frustum)
     wall-height (* (/ UNIT distance) focal)
     height (:height frustum)
     x-tex (if (= hit-direction :horizontal) (mod x 64) (mod y 64))
     midpoint (:midpoint state (/ height 2))
     [start end] (get-wall-endpoints height wall-height midpoint)
     top (max start 0)
     bottom (min end height)
     wall-texture ((:wall-type cast-result) textures)
     ]
    (draw-floor! state column-index angle (dec bottom) height)
    ;(draw-ceiling! state column-index angle 0 (inc top))
    (draw-dome-stripe! sky-texture midpoint top angle column-index)
    (draw-wall-stripe! wall-texture
                       x-tex column-index start wall-height distance)
    ))

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
        sp-dist (q/dist x y (:x sprite) (:y sprite))
        ratio (/ focal sp-dist)
        sp-height (:height sprite)
        sp-width (:width sprite)
        pos-x (- (sprite-screen-x frustum angle) (/ (* sp-width ratio) 2.0))
        off-floor (/ (- UNIT sp-height) 2.0)
        pos-y (+ (* ratio off-floor) (- midpoint (/ (* ratio sp-height) 2.0)))
        frame (sprite-get-frame sprite)]
    (q/push-matrix)
    (q/scale 1 ratio)
    (q/tint (q/map-range sp-dist 0 500 255 0))
    (doseq [i (range (* ratio sp-width))
            :let [stripe (q/get-pixel frame (/ i ratio) 0 1 sp-height)]
            :when (< sp-dist (nth z-buf (+ pos-x i) 0))
            ]
      (q/image stripe (+ pos-x (* i 1)) (/ pos-y ratio))
      )
    (q/tint 255)
    (q/pop-matrix)))



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
