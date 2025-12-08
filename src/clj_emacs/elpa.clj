(ns clj-emacs.elpa
  (:require [clojure.string :as str]))

(def package-first-line-re
  #"^;;; ([^ ]*)\.el ---[ \t]*(.*?)[ \t]*(-\*-.*-\*-[ \t]*)?$")

(comment
  (def test-first-line ";;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-")
  (-> (re-matches package-first-line-re test-first-line) (subvec 1))
  ;; =>
  ["package"
   "Simple package system for Emacs"
   "-*- lexical-binding:t -*-"])

(defn package-first-line-info
  [line]
  (when-let [[_ name desc _] (re-matches package-first-line-re line)]
    {:name name :desc desc}))
