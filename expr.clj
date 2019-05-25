(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (proto-get (obj :prototype) key)))

(defn proto-call [this key & args]
  (apply (proto-get this key) (cons this args)))

(defn field [key]
  (fn [this] (proto-get this key)))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(def toString (method :toString))
(def evaluate (method :evaluate))
(def diff (method :diff))
(def diffF (field :diffF))
(def operands (field :operands))
(def symbol (field :symbol))
(def operation (field :operation))

(def ConstProto (let [number (field :value)] {
                                              :toString (fn [this] (format "%.1f" (number this)))
                                              :evaluate (fn [this _] (number this))}))

(defn Constant [num] {:prototype ConstProto :value num})
(let [zero (Constant 0)]
  (def ConstProto (assoc ConstProto :diff (fn [& _] zero))))

(def VariableProto (let [name (field :value)
                         zero (Constant 0)
                         one (Constant 1)] {
                                            :evaluate (fn [this vars] (vars (name this)))
                                            :toString (fn [this] (name this))
                                            :diff     (fn [this var] (if (= var (name this)) one zero))
                                            }))

(defn Variable [name] {:prototype VariableProto :value name})

(def OperationProto
  {:evaluate (fn [this var] (apply (operation this) (mapv (fn [operand] (evaluate operand var)) (operands this))))
   :toString (fn [this] (str "(" (symbol this) " " (clojure.string/join " " (mapv toString (operands this))) ")"))
   :diff     (fn [this key] ((diffF this) (operands this) (mapv (fn [x] (diff x key)) (operands this)) key))})

(defn OperationFactory [action symbol diff]
  (let [proto {:prototype OperationProto
               :operation action
               :diffF     diff
               :symbol    symbol}]
    (fn [& args] {:prototype proto
                  :operands  (vec args)})))

(def Add (OperationFactory + '+ (fn [[& _] [& da] var] (apply Add da))))
(def Subtract (OperationFactory - '- (fn [[& a] [& da] var] (apply Subtract da))))
(def Multiply (OperationFactory * '* (fn [[fia seca & a] [fida secda & da] var] (cond
                                                                                  (== (count a) 0) (Add (Multiply fida seca)
                                                                                                        (Multiply secda fia))
                                                                                  (> (count a) 0) (diff (Multiply fia (apply Multiply (rest a) (rest da))) var)))))
(def Square (OperationFactory (fn [x] (* x x)) 'square (fn [[& a] [& da] var] (diff Multiply a da var))))
(def Negate (OperationFactory - 'negate (fn [[_] [da] var] (Negate da))))
(def Divide (OperationFactory (fn [a & b] (/ (double a) (apply * b))) '/ (fn [[& a] [& da] var] (cond
                                                                                                  (== (count a) 2) (Divide (Subtract (Multiply (first da) (second a))
                                                                                                                                     (Multiply (first a) (second da)))
                                                                                                                           (Square (second a)))))))
(def Sqrt (OperationFactory (fn [x] (Math/sqrt (Math/abs x))) 'sqrt (fn [[a] [da] var]
                                                                      (Divide (Multiply (diff a da) (Sqrt a))
                                                                              a (Constant 2)))))


(def objectOperations
  {
   '+      Add
   '-      Subtract
   '*      Multiply
   '/      Divide
   'negate Negate
   'square Square
   'sqrt   Sqrt
   })

(defn parseObj [expr]
  (cond
    (seq? expr) (apply (objectOperations (first expr)) (mapv parseObj (rest expr)))
    (number? expr) (Constant expr)
    (symbol? expr) (Variable (str expr))))

(def parseObject
  (comp parseObj read-string))