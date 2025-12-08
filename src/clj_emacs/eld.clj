(ns clj-emacs.eld
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defrecord Cons [car cdr])
(defrecord Quote [data])
(defrecord BackQuote [data])

(defn ->cons [car cdr] (->Cons car cdr))
(defn ->quote [data] (->Quote data))
(defn ->backquote [data] (->BackQuote data))

(defprotocol ElispData
  (clj->eld [_]))

(defn cons->inner-eld
  [cons]
  (when (some? cons)
    (let [{:keys [car cdr]} cons]
      (cond (nil? cdr) (clj->eld car)
            (instance? Cons cdr) (str (clj->eld car) \space (cons->inner-eld cdr))
            :else (str (clj->eld car) " . " (clj->eld cdr))))))

(extend-protocol ElispData
  nil
  (clj->eld [_data]
    "nil")

  java.lang.Boolean
  (clj->eld [data]
    (if data "t" "nil"))

  java.lang.Long
  (clj->eld [data]
    (pr-str data))

  java.lang.Double
  (clj->edn [data]
    (pr-str data))

  java.lang.String
  (clj->eld [data]
    (pr-str data))

  clojure.lang.Symbol
  (clj->eld [data]
    (pr-str data))

  clojure.lang.Keyword
  (clj->eld [data]
    (pr-str data))

  clojure.lang.PersistentList
  (clj->eld [data]
    (str \( (->> data (map clj->eld) (str/join \space)) \)))

  clojure.lang.PersistentVector
  (clj->eld [data]
    (str \[ (->> data (map clj->eld) (str/join \space)) \]))

  Cons
  (clj->eld [data]
    (str \( (cons->inner-eld data) \)))

  Quote
  (clj->eld [data]
    (str \' (clj->eld data)))

  BackQuote
  (clj->eld [data]
    (str \` (clj->eld data))))

(comment
  (clj->eld ['(1 2 hello) :world (->cons 1 (->cons 2 (->cons 3 'hello))) (->cons (->cons 'a 1) (->cons (->cons 'b 2) nil))])
  ;; => "[(1 2 hello) :world (1 2 3 . hello) ((a . 1) (b . 2))]"
  )

(defmulti read
  (fn [s] (first s)))

(defmethod read nil [s]
  (throw (ex-info "" {:reason ::end-of-data})))

(def whitespace-chars
  #{\space \tab \formfeed \return \newline})

(def newline-chars
  #{\return \newline})

(defn skip-whitespace
  [s]
  (if-not (contains? whitespace-chars (first s))
    s
    (recur (rest s))))

(defn skip-comment
  [s]
  (if (contains? newline-chars (first s))
    (rest s)
    (recur (rest s))))

(comment
  (apply str (skip-whitespace "  hello")) ; => "hello"
  (apply str (skip-comment "; hello\nworld")) ; => "world"
  )

(doseq [char whitespace-chars]
  (defmethod read char [s]
    (read (skip-whitespace s))))

(defmethod read \; [s]
  (read (skip-comment s)))
