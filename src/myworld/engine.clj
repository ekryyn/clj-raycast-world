(ns myworld.engine (:require [quil.core :as q]))

(def UNIT 64)

(defn world-width [{:keys [width]}] (* UNIT width))
(defn world-height [{:keys [height]}] (* UNIT height))

(defn enumerate [coll]
  (map vector (iterate inc 0) coll)  )

(defn normalize-angle [angle]
  (let [pipi (* 2 Math/PI)
        angle (mod angle pipi)]
    (if (> angle Math/PI)
      (- angle pipi)
      angle)))

(defn is-door? [{:keys [door?] :as tile-types} wall-type-id]
  (true? door?))

(defn world-cell [{:keys [width data height]} l c]
  (let [length (* width height)
        l (max l 0)
        c (max c 0)
        i (+ c (* width l))
        i (max 0 i)
        i (min i (dec length))] (nth data i)))

(defn focal-dist
  [{:keys [fov width height]}]
  (let [angle (/ fov 2.0)]
    (/ (/ width 2.0) (q/tan angle))))

(defn in-world? [world x y]
  "is (x,y) in world boundaries ?"
  (and
    (< 0 x (world-width world))
    (< 0 y (world-height world))))

(defn wall-type
  "is (x,y) a wall ?"
  [world x y]
  (let [c (q/floor (/ x UNIT))
        r (q/floor (/ y UNIT))]
    (if (in-world? world x y)
      (world-cell world r c)
      1)
    ))

(defn wall?
  "is (x,y) a wall ?"
  [world x y]
  (let [c (q/floor (/ x UNIT))
        r (q/floor (/ y UNIT))]
    (or (not (in-world? world x y))
        (> (world-cell world r c) 0))))

(defn- next-horiz-hit
  [x y angle world]
  (let
    [a angle
     tan-a (q/tan a)
     Ya (if (pos? a) (- UNIT) UNIT)
     Xa (if (zero? tan-a)
          Ya
          (/ Ya (q/tan (- a))))
     Ay (if (pos? a)
          (- (* (q/floor (/ y UNIT)) UNIT) 1)
          (+ (* (q/floor (/ y UNIT)) UNIT) UNIT))
     Ax (+ x (/ (- y Ay) tan-a))]
    (loop
      [Ax Ax
       Ay Ay]
      (if (wall? world Ax Ay)
          [Ax Ay :horizontal (wall-type world Ax Ay)]
          (recur (+ Ax Xa) (+ Ay Ya)))
      )))

(defn- next-vert-hit
  [x y angle world]
  (let
    [a angle
     half-pi (/ Math/PI 2.0)
     tan-a (q/tan a)
     Xa (if (< (- half-pi) angle half-pi) UNIT (- UNIT))
     Ya (* Xa (q/tan (- a)))]
    (loop
      [Bx (if (< (- half-pi) angle half-pi)
            (+ (* (q/floor (/ x UNIT)) UNIT) UNIT)
            (- (* (q/floor (/ x UNIT)) UNIT) 1))
       By (+ y (* (- x Bx) (q/tan a)))]
      (if (wall? world Bx By)
          [Bx By :vertical (wall-type world Bx By)]
          (recur (+ Bx Xa) (+ By Ya)))
      )))

(defn next-wall
  [x y angle world]
  (let [a angle
        check-horiz? (not (zero? (q/sin a)))
        check-vert? (not (zero? (q/cos a)))
        ]
    (cond
      (and check-horiz? check-vert?)
      (let [hit-h (next-horiz-hit x y angle world)
            hit-v (next-vert-hit x y angle world)]
        (min-key (fn [[px py _ _]]  (q/dist x y px py)) hit-v hit-h))
      check-horiz? (next-horiz-hit x y angle world)
      check-vert? (next-vert-hit x y angle world)
      )))


(defn proj-angles
  "Get angles (in radians) of rays to cast from current view"
  [rot fov width]
  (let [step (/ fov (float width))]
    (loop [column 0
           angle (- rot (/ fov 2.0))
           res []]
      (if (< column width)
          (recur (inc column)
                 (+ angle step)
                 (conj res (normalize-angle angle) ))
          (reverse res)
        ))))

(defn ray-cast
  [{:keys [frustum world x y rot]} angle]
  (let  ;; as a toy, we assume wall at 200 everywhere
    [[wx wy hit-dir wtype] (next-wall x y angle world)
     dist (q/dist x y wx wy)]
    {
     :distance (* dist (q/cos (- angle rot))); we'll use above later
     :angle angle
     :x wx, :y wy
     :hit-direction hit-dir
     :wall-type wtype
     :frustum frustum ; keep the frustum used to case
     }))

(defn add-vector [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn rotate-vector [angle [vx vy]]
  (let [a angle
        cs (q/cos a)
        sn (q/sin a) ]
    [(- (* vx cs) (* vy sn))
     (+ (* vx sn) (* vy cs))]))

(defn floor-caster
  "cast of the floor, returns vector [dx, dy]"
  [focal player-height beta]
  (fn [py]
    (let [b beta
          straight (/ (* player-height focal) py)
          distance (/ straight (q/cos b))
          dx (* straight (q/tan b))
          dy straight]
       [dy (- dx)])))

(defn sprite-screen-x
  [{:keys [fov width] :as frustum} angle]
  (let [a angle
        focal (focal-dist frustum)
        distance-to-half-screen (* focal (q/tan a))
        distance (- (/ width 2.0) distance-to-half-screen)]
    distance))

(defn sprite-angle [rot player-x player-y sprite-x sprite-y]
  (let [dx (- sprite-x player-x )
        dy (- sprite-y player-y )]
    (normalize-angle (- (q/atan2 (- dy) dx) rot))))


(defn sprite-visible? [rot player-x player-y sprite-x sprite-y fov]
  (let [angle (sprite-angle rot player-x player-y sprite-x sprite-y)
        extra (+ fov 0.35)
        ]
    (<= (/ extra -2.0) angle (/ extra 2.0))))


(defn forward-vector
  [{:keys [world rot x y] :as state} speed]
  (let [a rot
        dx (* speed (q/cos a))
        dy (* (- speed) (q/sin a))]
    [(if (wall? world (+ x dx) y) 0 dx)
     (if (wall? world x (+ y dy)) 0 dy)]))

