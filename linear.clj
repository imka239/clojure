(defn equals-len?
  ([] true)
  ([vector1 & vectors]
   (let [cnt (count vector1)] (every? (fn [x] (== (count x) cnt)) vectors))))

(defn scalar-vec? [vec] (and (vector? vec) (every? number? vec)))

(defn shape [a]
  (if (number? a)
    ()
    (cons (count a) (shape (first a)))))

(defn operate-f [func]
  (fn
    ([v]
     (if (number? v)
       (func v)
       (mapv (operate-f func) v)))
    ([v1 v2]
     (if (number? v1)
       (func v1 v2)
       (mapv (operate-f func) v1 v2)))
    ))

(defn broad? [t1 shapeT2] (= (nthrest shapeT2 (- (count shapeT2) (count (shape t1)))) (shape t1)))

(defn broadcast [t1 shapeT2]
  (if (equals-len? (shape t1) shapeT2)
    t1
    (vec (repeat (nth shapeT2 0) (broadcast t1 (rest shapeT2)))))
  )

;(m+ [[1 2 3] [4 5 6]] [[1 2 3][4 5 6]])

(defn matrix? [m]
  (and (vector? m) (apply equals-len? m) (every? scalar-vec? m)))

(defn tensor? [t] (or (number? t)
                      (scalar-vec? t)
                      (if (and (every? vector? t) (equals-len? t)) (tensor? ((fn [tens] (reduce (partial reduce conj) tens)) t)))))

(defn check-matrix? [m & ms]
  (and (apply equals-len? m ms) (every? matrix? (apply vector m ms))))

(defn checked-mat-op [op] (fn [m & ms]
                            {:pre [(and (apply check-matrix? m ms) (every? (fn [x] (equals-len? (first m) (first x))) ms))]}
                            (apply op m ms)))
(defn checked-vec-op [op] (fn [v & vcs]
                            {:pre [(and (apply equals-len? v vcs) (scalar-vec? v) (every? scalar-vec? vcs))]}
                            (apply op v vcs)))
(defn checked-tens-op [f] (fn
                            ([t1] {:pre [(tensor? t1)]} (f t1))
                            ([t1 t2]
                             {:pre [(and (tensor? t1) (tensor? t2))]}
                             (let [max-shape (max-key count (shape t1) (shape t2))]
                               {:pre (and (broad? t1 max-shape) (broad? t2 max-shape))}
                               (f (broadcast t1 max-shape) (broadcast t2 max-shape))))
                            ([t1 t2 & t]
                             {:pre (apply every? tensor? t1 t2 t)}
                             (reduce (checked-tens-op f) ((checked-tens-op f) t1 t2) t))))

(defn apply_func [f] (partial mapv f))

(defn apply_v1_v2 [f] (checked-vec-op (apply_func f)))
(defn apply_m1_m2 [f] (checked-mat-op (apply_func f)))
(defn apply_t1_t2 [f] (checked-tens-op (operate-f f)))
(defn apply_v1_scalar [f] (fn
                            ([arg & vals] {:pre [(scalar-vec? arg) (every? number? vals)]} (mapv (fn [layer] (apply f layer vals)) arg))))
(defn apply_m1_scalar [f] (fn
                            ([arg & vals] {:pre [(matrix? arg) (every? number? vals)]} (mapv (fn [layer] (apply f layer vals)) arg))))

(def v+ (apply_v1_v2 +))
(def m+ (apply_m1_m2 v+))
(def v- (apply_v1_v2 -))
(def m- (apply_m1_m2 v-))
(def v* (apply_v1_v2 *))
(def m* (apply_m1_m2 v*))
(def v*s (apply_v1_scalar *))
(def m*s (apply_m1_scalar v*s))

(defn scalar [& args] (apply + (apply v* args)))

(defn vect
  ([v1 v2]
   {:pre [(scalar-vec? v1) (scalar-vec? v2) (== (count v1) 3) (== (count v2) 3)]}
   (vector (- (* (nth v1 1) (nth v2 2)) (* (nth v1 2) (nth v2 1)))
           (- (* (nth v1 2) (nth v2 0)) (* (nth v1 0) (nth v2 2)))
           (- (* (nth v1 0) (nth v2 1)) (* (nth v1 1) (nth v2 0)))))
  ([v1] {:pre [(scalar-vec? v1)]} v1)
  ([v1 v2 & v]
   {:pre [(apply equals-len? v1 v2 v)]}
   (reduce vect (vect v1 v2) v))
  )

(def m*v (fn [m v] {:pre [(matrix? m)]} (mapv (fn [row] (scalar row v)) m)))
(defn transpose [m] {:pre [(matrix? m)]} (apply mapv vector m))
(def m*m (fn
           ([m1] {:pre [(matrix? m1)]} m1)
           ([m1 m2] {:pre [(and (matrix? m1) (matrix? m2)) (== (count (first m1)) (count (first (transpose m2))))]}
            (mapv (fn [ra] (mapv (fn [cb] (scalar ra cb)) (transpose m2))) m1))
           ([m1 m2 & m] (reduce m*m (m*m m1 m2) m))))

(def b+ (apply_t1_t2 +))
(def b* (apply_t1_t2 *))
(def b- (apply_t1_t2 -))



