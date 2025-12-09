(ns clj-emacs.elpa
  (:require [clojure.string :as str]))

(def first-line-re
  #"^;;; ([^ ]*)\.el ---[ \t]*(.*?)[ \t]*(-\*-.*-\*-[ \t]*)?$")

(defn first-line-info
  [line]
  (when-let [[_ name desc local-vars] (re-matches first-line-re line)]
    {:name name :desc desc :local-vars local-vars}))

(comment
  (def test-first-line ";;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-")
  (-> (re-matches first-line-re test-first-line) (subvec 1))
  ;; =>
  ["package"
   "Simple package system for Emacs"
   "-*- lexical-binding:t -*-"])

(def commentary-line-re
  #"^;;;[ \t]*[Cc]ommentary:[ \t]*$")

(defn commentary-line?
  [line]
  (some? (re-matches commentary-line-re line)))

(comment
  (def test-commentary-line ";;; Commentary:")
  (re-matches commentary-line-re test-commentary-line) ; => ";;; Commentary:"
  )

(def header-line-re
  #"^;; ([^ \t]+)[ \t]*:[ \t]*(.*?)[ \t]*$")

(def header-continue-line-re
  #"^;;  [ \t]*(.*?)[ \t]*$")

(comment
  (def test-header-line ";; Author: Tom Tromey <tromey@redhat.com>")
  (def test-header-continue-line ";;         Daniel Hackney <dan@haxney.org>")
  (-> (re-matches header-line-re test-header-line) (subvec 1))
  ;; => ["Author" "Tom Tromey <tromey@redhat.com>"]
  (-> (re-matches header-continue-line-re test-header-continue-line) (subvec 1))
  ;; => ["Daniel Hackney <dan@haxney.org>"]
  )

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
                                 (if-let [[_ value] (re-matches header-continue-line-re (first lines))]
                                   (recur (conj values value) (rest lines))
                                   [values lines])))]
          (recur (conj acc [key values]) lines))
        (recur acc (rest lines))))))

(comment
  (def test-header-lines
    (str/split-lines "
;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;;         Daniel Hackney <dan@haxney.org>
;; Created: 10 Mar 2007
;; Version: 1.1.0
;; Keywords: tools
;; Package-Requires: ((tabulated-list \" 1.0 \"))

;; This file is part of GNU Emacs."))

  (parse-headers test-header-lines)
  ;; =>
  {"package-requires" ["((tabulated-list \" 1.0 \"))"]
   "keywords"         ["tools"]
   "version"          ["1.1.0"]
   "created"          ["10 Mar 2007"]
   "author"           ["Tom Tromey <tromey@redhat.com>" "Daniel Hackney <dan@haxney.org>"]})

(comment

  (def test-header-lines-magit
    (str/split-lines "
;; Copyright (C) 2008-2025 The Magit Project Contributors

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;;     Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;;     Kyle Meyer <kyle@kyleam.com>
;; Former-Maintainers:
;;     Nicolas Dudebout <nicolas.dudebout@gatech.edu>
;;     Noam Postavsky <npostavs@users.sourceforge.net>
;;     Peter J. Weisberg <pj@irregularexpressions.net>
;;     Phil Jackson <phil@shellarchive.co.uk>
;;     Rémi Vanicat <vanicat@debian.org>
;;     Yann Hodique <yann.hodique@gmail.com>

;; Homepage: https://github.com/magit/magit
;; Keywords: git tools vc

;; Package-Version: 20251202.2209
;; Package-Revision: 3fe795e51af3
;; Package-Requires: (
;;     (emacs        \"28.1\")
;;     (compat       \"30.1\")
;;     (cond-let      \"0.1\")
;;     (llama         \"1.0\")
;;     (magit-section \"4.4\")
;;     (seq           \"2.24\")
;;     (transient     \"0.10\")
;;     (with-editor   \"3.4\"))

;; SPDX-License-Identifier: GPL-3.0-or-later ")

    (parse-headers test-header-lines-magit)
    ;; =>
    {"author"                  ["Marius Vollmer <marius.vollmer@gmail.com>" "Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>"],
     "package-revision"        ["3fe795e51af3"],
     "homepage"                ["https://github.com/magit/magit"],
     "package-version"         ["20251202.2209"],
     "package-requires"        ["("
                                "(emacs        \"28.1\")"
                                "(compat       \"30.1\")"
                                "(cond-let      \"0.1\")"
                                "(llama         \"1.0\")"
                                "(magit-section \"4.4\")"
                                "(seq           \"2.24\")"
                                "(transient     \"0.10\")"
                                "(with-editor   \"3.4\"))"],
     "keywords"                ["git tools vc"],
     "maintainer"              ["Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>" "Kyle Meyer <kyle@kyleam.com>"],
     "spdx-license-identifier" ["GPL-3.0-or-later"],
     "former-maintainers"      ["Nicolas Dudebout <nicolas.dudebout@gatech.edu>"
                                "Noam Postavsky <npostavs@users.sourceforge.net>"
                                "Peter J. Weisberg <pj@irregularexpressions.net>"
                                "Phil Jackson <phil@shellarchive.co.uk>"
                                "Rémi Vanicat <vanicat@debian.org>"
                                "Yann Hodique <yann.hodique@gmail.com>"]}))
