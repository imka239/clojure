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
     :diff     (fn [this key] ((diffF this) (operands this)  key))})

(defn OperationFactory [action symbol diff]
  (let [proto {:prototype OperationProto
              :operation action
              :diffF     diff
              :symbol    symbol}]
    (fn [& args] {:prototype proto
                  :operands  (vec args)})))

(defn Mapv [f a var] (apply f (mapv (fn [x] (diff x var)) a)))

(def Add (OperationFactory + '+ (fn [a var] (Mapv Add a var))))
(def Subtract (OperationFactory - '- (fn [a var] (Mapv Subtract a var))))
(def Multiply (OperationFactory * '* (fn [[fi sec & a] var]
                                       (let [difffi (diff fi var), diffsec (diff sec var)]
                                          (cond
                                            (== (count a) 0) (Add (Multiply difffi sec)
                                                                  (Multiply diffsec fi))
                                            (> (count a) 0) (diff (Multiply fi (apply Multiply [sec a])) var))))))
(def Square (OperationFactory (fn [x] (* x x)) 'square (fn [[fi] var] (diff (Multiply fi fi) var))))
(def Negate (OperationFactory - 'negate (fn [a var] (Negate (Mapv Add a var)))))
(def Divide (OperationFactory (fn [a & b] (/ (double a) (apply * b))) '/ (fn [[fi sec & a] var]
                                                             (let [difffi (diff fi var), diffsec (diff sec var)]
                                                              (cond
                                                                (== (count a) 0) (Divide (Subtract (Multiply difffi sec)
                                                                                                   (Multiply fi diffsec))
                                                                                         (Square sec))
                                                                (> (count a) 0) (diff (Divide fi (apply Multiply [sec a])) var))))))
(def Sqrt (OperationFactory (fn [x] (Math/sqrt (Math/abs x))) 'sqrt (fn [[fi] var]
                                                                      (let [diffi (diff fi var)]
                                                                        (Divide (Multiply diffi (Sqrt fi))
                                                                                fi (Constant 2))))))


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