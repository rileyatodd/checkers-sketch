(ns checkers.perlin)
; Based on eevee's implementation
; https://gist.github.com/eevee/26f547457522755cb1fb8739d0ea89a1
; and blog post
; https://eev.ee/blog/2016/05/29/perlin-noise/

(def sqrt #(Math/sqrt %))

(def floor #(Math/floor %))

(def ceil #(Math/ceil %))

(def log #(Math/log %))

(def cos #(Math/cos %))

(def π Math/PI)

(def pow #(Math/pow %1 %2))

;; math/vector stuff

(defn gaussian
  "Normal-distributed random number via Box-Muller transform"
  []
  (let [u (- 1 (rand 1))
        v (- 1 (rand 1))]
    (* (sqrt (* -2 (log u)))
       (cos (* 2 π v)))))

(defn normalize
  "Normalize a vector"
  [v]
  (let [scale (/ (sqrt (reduce + 0 (map #(* % %) v))))]
    (mapv (partial * scale) v)))

(defn v-
  [v w]
  (mapv - v w))

(defn dot
  [& vs]
  (reduce + 0 (apply map * vs)))

(defn product
  "Generate the cartesian product of vector"
  [colls]
  (if (empty? colls)
    (list '())
    (->> (for [x (first colls)
               xs (product (rest colls))]
           (cons x xs))
         (map vec))))

;; perlin stuff

(defn generate-gradient
  [dims]
  (normalize (vec (repeatedly dims gaussian))))

(defn smoothstep
  "Smooth curve for interpolating (zero derative @ 0 , 1)"
  [t]
  (* t t (- 3 (* 2 t))))

(defn lerp
  "Linear interpolation between a & b, at point t between them"
  [t a b]
  (+ a (* t (- b a))))

(defn noise-generator
  [{:keys [dimensions octaves tile unbias]
    :or {octaves 1 tile [] unbias false}}]
  (let [gradients (atom {})
        dims dimensions
        tile (into tile (repeat dims 0))
        scale-factor (/ 2 (sqrt dims))

        plain-noise-at
        (fn [point]
          (let [; list of (min,max) bounds in each direction
                grid-coords (mapv (juxt floor (comp inc floor)) point)
                ; Dot products of each gradient vector &
                ; distance from grid point gives gradient's
                ; influence on the point
                dots (for [grid-point (product grid-coords)]
                       (do
                         (when-not (get @gradients grid-point)
                           (swap! gradients assoc grid-point
                                  (generate-gradient dims)))
                         (dot (get @gradients grid-point)
                              (v- point grid-point))))]
            ; Now interpolate dot products together
            (loop [dim (dec dims)
                   dots dots]
              (if (> (count dots) 1)
                (let [s (smoothstep
                          (- (get point dim)
                             (get-in grid-coords [dim 0])))]
                  (recur (dec dim)
                         (map (fn [[a b]] (lerp s a b))
                              (partition 2 dots))))
                ; for n dimensions, range of noise is ±sqrt (n)/2
                ; multiply to scale to ±1
                (* scale-factor (first dots))))))

        add-octaves
        (fn [point]
          (reduce
            (fn [r o]
              (let [o2 (bit-shift-left 1 o)
                    new-point (vec (map-indexed
                                     (fn [i coord]
                                       (let [c (* coord o2)
                                             t (get tile i)]
                                         (if-not (zero? t)
                                           (mod c (* t o2))
                                           c)))
                                     point))]
                (+ r (/ (plain-noise-at new-point) o2))))
            0
            (range octaves)))

        scale (fn [v] (/ v (- 2 (pow 2 (- 1 octaves)))))

        unbias (fn [v]
                 (if-not unbias
                   v
                   (-> (reduce (fn [r _] (smoothstep r))
                               (/ (inc v) 2)
                               (range (ceil (/ octaves 2))))
                       (* 2)
                       dec)))

        generate
        (fn [point]
          (-> point
              add-octaves
              scale
              unbias))]
    generate))