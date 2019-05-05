(defn equals-len?
  ([] true)
  ([v] true)
  ([vector1 & vectors]
   (let [cnt (count vector1)] (every? (fn [x] (== (count x) cnt)) vectors)))
  )

(defn scalar-vec? [& vals] (every? (fn [v] (number? v)) vals))

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


(defn broadcast [t1 t2]
  {:pre (= (nthrest t2 (- (count t2) (count (shape t1)))) (shape t1))}
  (if (equals-len? (shape t1) t2)
    t1
    (vec (repeat (nth t2 0) (broadcast t1 (rest t2)))))
  )

;(m+ [[1 2 3] [4 5 6]] [[1 2 3][4 5 6]])
(defn matrix? [m]
  (and (vector? m) (apply equals-len? m)))
;(every? (fn [x] (equals-len? (first m)  (first x))) mts)
(defn checked-mat-op [op] (fn [m & mts]
                            {:pre [(apply equals-len? m mts) (every? matrix? (apply vector m mts)) ]}
                            (apply op m mts)))

(defn checked-vec-op [op] (fn [v & vcs]
                            {:pre [(apply equals-len? v vcs)]}
                            (apply op v vcs)))

(defn apply_v1_v2 [f] (checked-vec-op (fn [& vcs] (apply mapv f vcs))))
(defn apply_m1_m2 [f] (checked-mat-op (fn [& vcs] (apply mapv f vcs))))
(defn apply_t1_t2 [f]
  (fn
    ([t1] ((operate-f f) t1))
    ([t1 t2]
     (let [max-shape (max-key count (shape t1) (shape t2)) t1 (broadcast t1 max-shape) t2 (broadcast t2 max-shape)]
       ((operate-f f) t1 t2)))
    ([t1 t2 & t]
     (reduce (apply_t1_t2 f) ((apply_t1_t2 f) t1 t2) t))))

(defn apply_v1_scalar [f] (fn
                            ([arg] arg)
                            ([arg & vals] (mapv (fn [layer] (apply f layer vals)) arg))))

(def v+ (apply_v1_v2 +))
(def m+ (apply_m1_m2 v+))
(def v- (apply_v1_v2 -))
(def m- (apply_m1_m2 v-))
(def v* (apply_v1_v2 *))
(def m* (apply_m1_m2 v*))
(def v*s (apply_v1_scalar *))
(def m*s (apply_v1_scalar v*s))

(defn scalar [& args] (apply + (apply v* args)))

(defn vect
  ([v1 v2]
   {:pre [(vector? v1) (vector? v2) (== (count v1) 3) (== (count v2) 3)]}
   (vector (- (* (nth v1 1) (nth v2 2)) (* (nth v1 2) (nth v2 1)))
           (- (* (nth v1 2) (nth v2 0)) (* (nth v1 0) (nth v2 2)))
           (- (* (nth v1 0) (nth v2 1)) (* (nth v1 1) (nth v2 0)))))
  ([v1] {:pre [(vector? v1)]} v1)
  ([v1 v2 & v]
   {:pre [(apply equals-len? v1 v2 v)]}
   (reduce vect (vect v1 v2) v))
  )

(def m*v (fn [m v] (mapv(fn [row] (scalar row v)) m)))
(defn transpose [m] (apply mapv vector m))
(def m*m (fn
           ([m1] m1)
           ([m1 m2] (mapv (fn [ra] (mapv (fn [cb] (scalar ra cb)) (transpose m2))) m1))
           ([m1 m2 & m] (reduce m*m (m*m m1 m2) m))
           ))

(def b+ (apply_t1_t2 +))
(def b* (apply_t1_t2 *))
(def b- (apply_t1_t2 -))