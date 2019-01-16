(ns checkers.dynamic
  (:require [quil.core :as q])
  (:require [genartlib.util :as u])
  (:require [genartlib.algebra :refer :all])
  (:require [checkers.perlin :refer [noise-generator]]))

(def sketch-width "" 500)
(def sketch-height "" 500)

(def perlin #((noise-generator {:dimensions 1 :octaves 3}) [%]))

(def t0 (/ (System/currentTimeMillis) 1000.0))

(defn t [] (- (/ (System/currentTimeMillis) 1000.0) t0))
(defn qt [] (/ (q/millis) 1000.0))

(defn grid
  ([w h r c x0 y0]
   (map (fn [y]
          (map (fn [x] [(float x) (float y)])
               (range x0 (inc w) (/ w c))))
        (range y0 (inc h) (/ h r)))) 
  ([w h r c]
   (grid w h r c 0 0)))

(defn with-next [coll]
  (map vector coll (drop 1 coll)))

(defn checkers [grid]
  (map (fn [[line0 line1]] 
         (map (fn [[p0 p1] [p3 p2]] [p0 p1 p2 p3]) 
              line0 line1))
       (with-next (map with-next grid))))

(defn norm-quad [quad]
  (let [[[x0 y0]] quad]
    (map (fn [[x y]] [(- x x0) (- y y0)]) quad)))

(defn odds [coll]
  (keep-indexed #(if (odd? %1) %2) coll))

(defn evens [coll]
  (keep-indexed #(if (even? %1) %2) coll))

(defn initial-state []
  (let [w sketch-width
        h sketch-height
        num-lines 15
        pts-per-line 12
        g (grid (+ w 100) (+ h 100) num-lines pts-per-line -100.0 -100.0)]
    {:grid (grid w h num-lines pts-per-line)
     :checkers (checkers )}))

(defonce play-state (atom {:paused false}))

(defn play-pause [k] 
  (swap! play-state #(update-in % [k] not)))

(defonce my-state (atom (initial-state)))

(defn reset-state []
  (swap! my-state (fn [_] (initial-state))))

(defn plot [f xs]
  (doseq [x xs]
    (q/point x (f x))))

(defn plot-points [ps]
  (doseq [[x y] ps]
    (q/point x y)))

(defn draw-line [points]
  (loop [ps points]
    (when (> (count ps) 1)
      (apply q/line (concat (nth ps 0) (nth ps 1)))
      (recur (rest ps)))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/smooth)
  (swap! my-state (fn [_] (initial-state))))

(defn update-state [s] {:grid (:grid s) :checkers (:checkers s)}) 

(defn draw-state [state]
  (let [reversed? (even? (int (/ (t) q/PI)))
        rot (t)]
    (if reversed?
      (do 
        (q/background 0 0 60)
        (q/stroke 130 255 255 255)
        (q/fill 130 255 255 255))
      (do 
        (q/background 130 255 255)
        (q/stroke 0 0 60 255)
        (q/fill 0 0 60 255)))
    
    (doseq [quad-row (evens (:checkers @my-state))]
      (doseq [quad ((if reversed? odds evens) quad-row)]
        (let [[[x0 y0]] quad]
          (q/with-translation [x0 y0]
            (q/with-rotation [rot]
              (apply q/quad ;(map #(+ % (* 10 (q/sin rot))) 
                                 (flatten (norm-quad quad))))))));)
    (doseq [quad-row (odds (:checkers @my-state))]
      (doseq [quad ((if reversed? evens odds) quad-row)]
        (let [[p0 p1 p2 p3] quad
              [[x0 y0]] quad
              rot-quad [p2 p3 p0 p1]
              [[x2 y2]] rot-quad]
          (q/with-translation [x0 y0]
            (q/with-rotation [rot]
              (apply q/quad ;(map #(+ % (* 10 (q/sin rot))) 
                                 (flatten (norm-quad quad))))))))));)
