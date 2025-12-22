(ns clj-emacs.eld
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn third
  [s]
  (nth s 2 nil))

(defn join-chars
  [s]
  (let [sb (StringBuilder.)]
    (doseq [c s]
      (.append sb (char c)))
    (str sb)))

(defrecord Cons [car cdr])
(defrecord Quote [data])
(defrecord BackQuote [data])

(defn ->cons [car cdr] (->Cons car cdr))
(defn ->quote [data] (->Quote data))
(defn ->backquote [data] (->BackQuote data))

(defn seq->cons
  [s & [last]]
  (when (seq s)
    (let [s (reverse s)]
      (->> (rest s)
           (reduce
            (fn [cdr car]
              (->cons car cdr))
            (->cons (first s) last))))))

(defn cons->seq
  [cons]
  (when (some? cons)
    (lazy-seq
     (clojure.core/cons
      (:car cons) (cons->seq (:cdr cons))))))

(comment
  (seq->cons []) ; => nil
  (seq->cons [1 2 3]) ; => {:car 1 :cdr {:car 2 :cdr {:car 3 :cdr nil}}}
  (seq->cons [1 2 3] 4) ; => {:car 1 :cdr {:car 2 :cdr {:car 3 :cdr 4}}}
  (cons->seq (seq->cons [1 2 3])) ; => [1 2 3]
  )

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
    (str \' (clj->eld (:data data))))

  BackQuote
  (clj->eld [data]
    (str \` (clj->eld (:data data)))))

(comment
  (clj->eld ['(1 2 hello) :world (->cons 1 (->cons 2 (->cons 3 'hello))) (->cons (->cons 'a 1) (->cons (->cons 'b 2) nil))])
  ;; => "[(1 2 hello) :world (1 2 3 . hello) ((a . 1) (b . 2))]"
  )

(defmulti read
  (fn [s] (first s)))

(defmethod read :default [s]
  (throw (ex-info "ELD 语法错误：未知语法" {:reason ::unknown-syntax :char (first s)})))

(defmethod read nil [s]
  (throw (ex-info "空字符串" {:reason ::end-of-data})))

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

(defn skip-whitespace-and-comment
  [s]
  (let [c (first s)]
    (cond (contains? whitespace-chars c) (recur (skip-whitespace s))
          (= \; c) (recur (skip-comment s))
          :else s)))

(comment
  (apply str (skip-whitespace "  hello")) ; => "hello"
  (apply str (skip-comment "; hello\nworld")) ; => "world"
  (apply str (skip-whitespace-and-comment " ; hello\r\nworld")) ; => "world"
  )

(doseq [c whitespace-chars]
  (defmethod read c [s]
    (read (skip-whitespace s))))

(defmethod read \; [s]
  (read (skip-comment s)))

(defn read-open-string
  [s]
  (loop [acc [] s s]
    (let [c (first s)]
      (cond (nil? c) (throw (ex-info "ELD 语法错误：未关闭字符串" {:reason ::unclosed-string}))
            (= c \") [(join-chars acc) (rest s)]
            (= c \\) (let [s (rest s)
                           c (first s)]
                       (cond (nil? c) (throw (ex-info "ELD 语法错误：未关闭字符串" {:reason ::unclosed-string}))
                             (= c \") (recur (conj acc \") (rest s))
                             (= c \\) (recur (conj acc \\) (rest s))
                             (= c \b) (recur (conj acc \backspace) (rest s))
                             (= c \t) (recur (conj acc \tab) (rest s))
                             (= c \f) (recur (conj acc \formfeed) (rest s))
                             (= c \n) (recur (conj acc \newline) (rest s))
                             (= c \r) (recur (conj acc \return) (rest s))
                             :else (recur (conj acc c) (rest s))))
            :else (recur (conj acc c) (rest s))))))

(defmethod read \" [s]
  (read-open-string (rest s)))

(comment
  (read "\"hello\r\nworld\"1") ; => ["hello\r\nworld" [\1]]
  )

(def symbol-continue-chars
  (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@$%^&*-_=+/?.|<>"))

(def symbol-start-chars
  (set/difference symbol-continue-chars (set "-?.")))

(def num-map
  {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})

(defn read-symbol
  [s]
  (loop [acc [] s s]
    (let [c (first s)]
      (if-not (contains? symbol-continue-chars c)
        [(join-chars acc) s]
        (recur (conj acc c) (rest s))))))

(doseq [c symbol-start-chars]
  (defmethod read c [s]
    (let [[sym s] (read-symbol s)]
      [(symbol sym) s])))

(defmethod read \: [s]
  (let [[sym s] (read-symbol (rest s))]
    (when (empty? sym)
      (throw (ex-info "ELD 语法错误：空关键字" {:reason ::empty-keyword})))
    [(keyword sym) s]))

(defn read-number
  [s]
  (let [[i s] (loop [i 0 s s]
                (let [c (first s)]
                  (if-let [n (num-map c)]
                    (recur (+ (* 10 i) n) (rest s))
                    (if (or (= \. c) (not (contains? symbol-continue-chars c)))
                      [i s]
                      (throw (ex-info "ELD 语法错误：未结束数字" {:reason ::unfinished-number}))))))]
    (if-not (= \. (first s))
      [i s]
      (let [s (rest s)
            [f s] (loop [f 0.0 base 0.1 s s]
                    (let [c (first s)]
                      (if-let [n (num-map c)]
                        (recur (+ f (* base n)) (* 0.1 base) (rest s))
                        (if-not (contains? symbol-continue-chars c)
                          [f s]
                          (throw (ex-info "ELD 语法错误：未结束数字" {:reason ::unfinished-number}))))))]
        [(+ i f) s]))))

(doseq [c (keys num-map)]
  (defmethod read c [s]
    (read-number s)))

(defmethod read \. [s]
  (when-not (contains? num-map (second s))
    (throw (ex-info "ELD 语法错误：单独的点" {:reason ::single-dot})))
  (read-number s))

(defmethod read \- [s]
  (let [c (second s)]
    (if (or (and (= c \.) (contains? num-map (third s)))
            (contains? num-map c))
      (let [[n s] (read-number (rest s))]
        [(- n) s])
      (let [[sym s] (read-symbol s)]
        [(symbol sym) s]))))

(comment
  (read ".123 a") ; => [0.12300000000000001 [\space \a]]
  (read "1.1 a") ; => [1.1 [\space \a]]

  (read "hello a") ; => ['hello [\space \a]]
  (read ":hello a") ; => [:hello [\space \a]]
  (read "-123 a") ; => [-123 [\space \a]]
  (read "-.1 a") ; => [-0.1 [\space \a]]

  (read "-hello a") ; => ['-hello [\space \a]]
  (read "--> a") ; => ['--> [\space \a]]

  (read "1.(a)") ; => [1.0 [\( \a \)]]
  (read "1(a)") ; => [1 (\[ \a \])]
  (read "-(a)") ; => ['- [\( \a \)]]
  (read "-.(a)") ; => ['-. [\( \a \)]]
  )

(defmethod read \' [s]
  (let [[data s] (read (rest s))]
    [(->quote data) s]))

(defmethod read \` [s]
  (let [[data s] (read (rest s))]
    [(->backquote data) s]))

(comment
  (read "'hello a") ; => [{:data 'hello} [\space \a]]
  )

(defmethod read \[ [s]
  (loop [acc [] s (skip-whitespace-and-comment (rest s))]
    (let [c (first s)]
      (if (nil? s)
        (throw (ex-info "ELD 语法错误：未关闭向量" {:reason ::unclosed-vector}))
        (if (= \] c)
          [acc (rest s)]
          (let [[data s] (read s)]
            (recur (conj acc data) (skip-whitespace-and-comment s))))))))

(defmethod read \( [s]
  (loop [acc [] s (skip-whitespace-and-comment (rest s))]
    (let [c (first s)]
      (if (nil? s)
        (throw (ex-info "ELD 语法错误：未关闭列表" {:reason ::unclosed-list}))
        (if (= \) c)
          [(seq->cons acc) (rest s)]
          (if (and (= \. c) (not (contains? symbol-continue-chars (second s))))
            (let [[last s] (read (rest s))
                  s (skip-whitespace-and-comment s)]
              (when-not (= \) (first s))
                (throw (ex-info "ELD 语法错误：未关闭列表" {:reason ::unclosed-list})))
              [(seq->cons acc last) (rest s)])
            (let [[data s] (read s)]
              (recur (conj acc data) (skip-whitespace-and-comment s)))))))))

(comment
  (read "[1 2 3] a") ; => [[1 2 3] [\space \a]]
  (read "(1 2 3) a") ; => [{:car 1 :cdr {:car 2 :cdr {:car 3 :cdr nil}}} [\space \a]]
  (read "(1 2 . 3) a") ; => [{:car 1 :cdr {:car 2 :cdr 3}} [\space \a]]
  (read "(1 2 .;3\n 3) a") ; => [{:car 1 :cdr {:car 2 :cdr 3}} [\space \a]]
  )

(defn eld->clj
  [s]
  (let [[data s] (read s)
        s (skip-whitespace-and-comment s)]
    (when (seq s)
      (throw (ex-info "ELD 数据错误：未完全读取" {:reason ::unfinished-parse})))
    data))
