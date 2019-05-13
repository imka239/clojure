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
(def operands (field :operands))
(def symbol (field :symbol))
(def operation (field :operation))
(def diffF (field :diffF))

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
                                            :diff     (fn [this var] (if (= var (name this)) one zero))}))

(defn Variable [name] {:prototype VariableProto :value name})

(def OperationPrototype {
                         :toString (fn [this] (str "(" (symbol this) (clojure.string/join " " (map toString (operands this))) ")"))
                         :evaluate (fn [this vars] (apply (operation this) (map (fn [x] (evaluate x vars)) (operands this))))
                         :diff (fn [this var] ((diffF this) (operands this) var))
                         })

(defn OperationFactory [oper, sym, dif]
  (fn [& args] (
                 :operation oper
                 :symbol sym
                 :diffF dif
                 :operands (apply vector args))))


(def Add (OperationFactory + '+ (fn [_ _ da db] (Add da db))))
(def Subtract (OperationFactory - '- (fn [_ _ da db] (Subtract da db))))
(def Multiply (OperationFactory * '* (fn [a b da db] (Add (Multiply a db) (Multiply b da)))))
(def Square (OperationFactory (fn [x] (* x x)) 'square (fn [a da] (Multiply (Constant 2) da a))))
(def Negate (OperationFactory - 'negate (fn [_ da] (Negate da))))
(def Divide (OperationFactory (fn [a & b] (/ (double a) (apply * b))) '/ (fn [a b da db] (Divide (Subtract (Multiply da b) (Multiply a db)) (Square b)))))
(def Sqrt (OperationFactory (fn [x] (Math/sqrt (Math/abs x))) 'sqrt (fn [a da] (Divide (Multiply a da) (Multiply (Constant 2) (Sqrt (Multiply (Square a) a)))))))


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
    :else (Variable (str expr))
    ))

(def parseObject
  (comp parseObj read-string))


