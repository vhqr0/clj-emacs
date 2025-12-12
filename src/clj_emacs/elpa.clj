(ns clj-emacs.elpa
  (:require [clojure.string :as str]))

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
