(ns clj-emacs.elpa
  (:require [clojure.string :as str]
            [clj-emacs.eld :as eld]))

(def first-line-re
  ;; name desc local-vars
  #"^;;; ([^ ]*)\.el ---[ \t]*(.*?)[ \t]*(-\*-.*-\*-[ \t]*)?$")

(def header-line-re
  ;; key value
  #"^;; ([^ \t]+)[ \t]*:[ \t]*(.*?)[ \t]*$")

(def header-continue-line-re
  ;; tab value
  #"^;;(  |\t)[ \t]*(.*?)[ \t]*$")

(defn parse-headers
  [lines]
  (loop [acc [] lines lines]
    (if (empty? lines)
      (->> acc
           (map
            (fn [[key values]]
              [(str/lower-case key) (->> values (remove str/blank?) vec)]))
           reverse
           (into {}))
      (if-let [[_ key value] (re-matches header-line-re (first lines))]
        (let [[values lines] (loop [values [value] lines (rest lines)]
                               (if (empty? lines)
                                 [values lines]
                                 (if-let [[_ _ value] (re-matches header-continue-line-re (first lines))]
                                   (recur (conj values value) (rest lines))
                                   [values lines])))]
          (recur (conj acc [key values]) lines))
        (recur acc (rest lines))))))

(def comment-line-re
  #"^;;;[ \t]*[Cc]ommentary:[ \t]*$")

(def code-line-re
  #"^;;;[ \t]*[Cc]ode:[ \t]*$")

(defn join-comment
  [lines]
  (->> lines
       (map #(cond-> % (str/starts-with? % ";;") (subs 2)))
       (map str/trim)
       (str/join \newline)
       str/trim))

(defn parse-package
  [lines]
  (if-let [[_ name desc local-vars] (some->> (first lines) (re-matches first-line-re))]
    (let [lines (rest lines)
          [header-lines lines] (->> lines (split-with #(nil? (re-matches comment-line-re %))))]
      (if (seq lines)
        (let [headers (parse-headers header-lines)
              lines (rest lines)
              [comment-lines lines] (->> lines (split-with #(nil? (re-matches code-line-re %))))]
          (if (seq lines)
            (let [comment (join-comment comment-lines)]
              {:name name :desc desc :local-vars local-vars :headers headers :comment comment})
            (throw (ex-info "未找到代码行" {:reason ::code-line-not-found}))))
        (throw (ex-info "未找到注释行" {:reason ::comment-line-not-found}))))
    (throw (ex-info "首行错误" {:reason ::first-line-not-match}))))

(defn parse-simple-text
  [lines]
  (str/trim (apply str lines)))

(defn parse-requires
  [lines]
  (->> (eld/eld->clj (str/join \space lines))
       eld/cons->seq
       (mapv
        (fn [cons]
          (let [[name version] (eld/cons->seq cons)]
            {:name (clojure.core/name name) :version version})))))

(defn parse-keywords
  [lines]
  (str/split (str/join \space lines) #"[ \t]+"))

(def person-line-re
  #"^[ \t]*(.*?)[ \t]*<(.*)>$")

(defn parse-persons
  [lines]
  (->> lines
       (keep
        (fn [line]
          (when-let [[_ name address] (re-matches person-line-re line)]
            {:name name :address address})))
       vec))

(defn expand-package-info
  [{:keys [headers] :as info}]
  (merge
   info
   (when-let [version (or (get headers "version") (get headers "package-version"))]
     {:version (parse-simple-text version)})
   (when-let [requires (or (get headers "requires") (get headers "package-requires"))]
     {:requires (parse-requires requires)})
   (when-let [keywords (get headers "keywords")]
     {:keywords (parse-keywords keywords)})
   (when-let [url (or (get headers "url") (get headers "homepage"))]
     {:url (parse-simple-text url)})
   (when-let [authors (get headers "author")]
     {:authors (parse-persons authors)})
   (when-let [maintainers (get headers "maintainer")]
     {:maintainers (parse-persons maintainers)})))

(defn requires->define-data
  [requires]
  (->> requires
       (map
        (fn [{:keys [name version]}]
          (eld/seq->cons [(symbol name) version])))
       eld/seq->cons
       eld/->quote))

(defn keywords->define-data
  [keywords]
  (->> keywords eld/seq->cons eld/->quote))

(defn persons->define-data
  [persons]
  (->> persons
       (map
        (fn [{:keys [name address]}]
          (eld/->cons name address)))
       eld/seq->cons
       eld/->quote))

(defn package-define-data
  [{:keys [name version desc requires url keywords authors maintainers]}]
  (eld/seq->cons
   (concat
    ['define-package
     name version desc (requires->define-data requires)]
    (when (some? url)
      [:url url])
    (when (some? keywords)
      [:keywords (keywords->define-data keywords)])
    (when (some? authors)
      [:authors (persons->define-data authors)])
    (when-let [maintainers (or maintainers authors)]
      [:maintainers (persons->define-data maintainers)]))))
