(def constant constantly)
(defn variable [name] (fn [vars] (vars name)))


(defn abstractOperation [func]
  (fn [& expr] (fn [vars] (apply func (mapv (fn [x] (x vars)) expr)))))
(defn reduce_op [func] (abstractOperation (fn [& expr] (reduce func expr))))

(def add (abstractOperation +))
(def subtract (abstractOperation -))
(def multiply (abstractOperation *))
(def negate (abstractOperation -))
(def divide (abstractOperation (fn [a & b] (/ (double a)  (apply * b)))))
(def min (reduce_op (fn [x y] (Math/min x y))))
(def max (reduce_op (fn [x y] (Math/max x y))))

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

(def parseFunction (comp parseFunc read-string))


; hw8

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

(def ConstProto (let [number (field :value)] {
                   :toString (fn [this] (let [value (number this)] (if (integer? value) (str value) (format "%.1f" value))))
                   :evaluate (fn [this _] (number this))}))

(defn Constant [num] {:prototype ConstProto :value num} )
(let [zero (Constant 0)]
  (def ConstProto (assoc ConstProto :diff (fn [_ _] zero))))

(def VariableProto (let [name (field :value)
                         zero (Constant 0)
                         one (Constant 1)] {
                   :evaluate (fn [this vars] (vars (name this)))
                   :toString (fn [this] (name this))
                   :diff (fn [this var] (if (= var (name this)) one zero))
                   }))

(defn Variable [name] {:prototype VariableProto :value name})

;let [[evaluate operation symbol diff] (methods :evaluate :operation)]
(def OperationProto (let [evaluate (method :evaluate) operation (field :operation) symbol (field :symbol) diff (method :diff)] {
                    :evaluate (fn [this var] (apply (operation this) (mapv (fn [operand] (evaluate operand var)) (operands this))))
                    :toString (fn [this] (str "(" (symbol this) " " (clojure.string/join " " (mapv toString (operands this))) ")"))
                    :diff (fn [this var] (diff this var))
                                                                                                                }))

(defn OperationFactory [action symbol diff] (fn [& operands] {
                    :prototype OperationProto
                    :operation action
                    :operands (vec operands)
                    :diff diff
                    :symbol symbol}))

(let [diff_impl (fn [this var] (map (fn [operand] (diff operand var)) (operands this)))
      operation_id (fn [this id] ((operands this) id))
      operation_rest (fn [this func] (apply func (rest (operands this))))
      diff_oper (fn [this var id] (diff (operation_id this id) var))
      ]
  (def Add (OperationFactory + '+ (fn [this var] (apply Add (diff_impl this var)))))
  (def Subtract (OperationFactory - '- (fn [this var] (apply Subtract (diff_impl this var)))))
  (def Multiply (OperationFactory * '* (fn [this var] (Add (Multiply  (operation_id this 0) (diff_oper this var 1))
                                                           (Multiply (operation_rest this Multiply) (diff_oper this var 0))))))
  (def Negate (OperationFactory - 'negate (fn [this var] (apply Negate (diff_impl this var)))))
  (def Divide (OperationFactory (fn [a & b] (/ (double a)  (apply * b))) '/
                                (fn [this var] (Divide(Subtract (Multiply (operation_id this 1) (diff_oper this var 0))
                                                           (Multiply (operation_id this 0) (diff_oper this var 1)))
                                                      (Multiply (operation_id this 1) (operation_id this 1))))))
  (def Sign (OperationFactory (fn [x] (Math/signum x)) 'sign (Constant 0)))
  (def Sqrt (OperationFactory (fn [x] (Math/sqrt (Math/abs x))) 'sqrt (fn [this var] (Divide (diff_oper this var 0)
                                                           (Multiply (Constant 2) (Sqrt (operation_id this 0)) (Sign (operation_id this 0)))))))
  (def Square (OperationFactory (fn [x] (* x x)) 'square (fn [this var] (Multiply (Constant 2) (diff_oper this var 0) (operation_id this 0)))))
  )

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
    (seq? expr) (apply (objectOperations (first expr))(mapv parseObj (rest expr)))
    (number? expr) (Constant expr)
    :else (Variable (str expr))
    ))

(def parseObject
  (comp parseObj read-string))