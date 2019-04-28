(def constant constantly)
(defn variable [name] (fn [vars] (vars name)))

(defn abstractOperation [func]
  (fn [& expr] (fn [vars] (apply func (mapv (fn [x] (x vars)) expr)))))
(defn specOperation [func] (fn [& expr] (reduce func expr)))

(def add (abstractOperation +))
(def subtract (abstractOperation -))
(def multiply (abstractOperation *))
(def negate (abstractOperation -))
(def divide (abstractOperation (fn [a & b] (/ (double a)  (apply * b)))))
(def min (abstractOperation (specOperation clojure.core/min)))
(def max (abstractOperation (specOperation clojure.core/max)))

(def operation
  {
   '+ add
   '- subtract
   '* multiply
   '/ divide
   'negate negate
   'min min
   'max max
  })

(defn parseFunc [expr]
  (cond
    (seq? expr) (apply (operation (first expr))(mapv parseFunc (rest expr)))
    (number? expr) (constant expr)
    :else (variable (str expr))))

(def parseFunction
  (comp parseFunc read-string))

