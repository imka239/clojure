(defn constant [value] (constantly value) )
(defn variable [name] (fn [vars] (vars name)))

(defn abstractOperation [func]
  (fn [expr] (fn [vars] (func (expr vars))))
  (fn [& expr] (fn [vars] (apply func (mapv (fn [x] (x vars)) expr)))))
(defn division [& expr]
  (reduce (fn [a b] (/ a  (double b))) expr))

(def add (abstractOperation +))
(def subtract (abstractOperation -))
(def multiply (abstractOperation *))
(def negate (abstractOperation -))
(def divide (abstractOperation division))

(def operation
  {
   '+ add
   '- subtract
   '* multiply
   '/ divide
   'negate negate
  })

(defn parseFunc [expr] ( cond
         (seq? expr) (apply (operation (first expr))(mapv parseFunc (rest expr)))
         (number? expr) (constant expr)
         :else (variable (str expr))
     ))
(def parseFunction
  (comp parseFunc read-string))