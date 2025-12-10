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

;; SPDX-License-Identifier: GPL-3.0-or-later")

    (parse-headers test-header-lines-magit)
    ;; =>
    {"author"                  ["Marius Vollmer <marius.vollmer@gmail.com>" "Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>"]
     "package-revision"        ["3fe795e51af3"]
     "homepage"                ["https://github.com/magit/magit"]
     "package-version"         ["20251202.2209"]
     "package-requires"        ["("
                                "(emacs        \"28.1\")"
                                "(compat       \"30.1\")"
                                "(cond-let      \"0.1\")"
                                "(llama         \"1.0\")"
                                "(magit-section \"4.4\")"
                                "(seq           \"2.24\")"
                                "(transient     \"0.10\")"
                                "(with-editor   \"3.4\"))"]
     "keywords"                ["git tools vc"]
     "maintainer"              ["Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>" "Kyle Meyer <kyle@kyleam.com>"]
     "spdx-license-identifier" ["GPL-3.0-or-later"]
     "former-maintainers"      ["Nicolas Dudebout <nicolas.dudebout@gatech.edu>"
                                "Noam Postavsky <npostavs@users.sourceforge.net>"
                                "Peter J. Weisberg <pj@irregularexpressions.net>"
                                "Phil Jackson <phil@shellarchive.co.uk>"
                                "Rémi Vanicat <vanicat@debian.org>"
                                "Yann Hodique <yann.hodique@gmail.com>"]}))

(def comment-line-re
  #"^;;;[ \t]*[Cc]ommentary:[ \t]*$")

(def code-line-re
  #"^;;;[ \t]*[Cc]ode:[ \t]*$")

(defn package-raw-info
  [text]
  (let [lines (str/split-lines text)]
    (if-let [{:keys [name desc local-vars]} (some-> (first lines) first-line-info)]
      (let [lines (rest lines)
            [header-lines lines] (->> lines (split-with #(nil? (re-matches comment-line-re %))))]
        (if (seq lines)
          (let [headers (parse-headers header-lines)
                lines (rest lines)
                [comment-lines lines] (->> lines (split-with #(nil? (re-matches code-line-re %))))]
            (if (seq lines)
              (let [comment (->> comment-lines
                                 (map #(cond-> % (str/starts-with? % ";;") (subs 2)))
                                 (map str/trim)
                                 (str/join \newline)
                                 str/trim)]
                {:name name :desc desc :local-vars local-vars :headers headers :comment comment})
              (throw (ex-info "未找到代码行" {:reason ::code-line-not-found}))))
          (throw (ex-info "未找到注释行" {:reason ::comment-line-not-found}))))
      (throw (ex-info "首行错误" {:reason ::first-line-not-match})))))

(comment
  (def test-package
    ";;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;;         Daniel Hackney <dan@haxney.org>
;; Created: 10 Mar 2007
;; Version: 1.1.0
;; Keywords: tools
;; Package-Requires: ((tabulated-list \"1.0\"))

;; This file is part of GNU Emacs.

;;; Commentary:

;; The idea behind package.el is to be able to download packages and
;; install them.  Packages are versioned and have versioned
;; dependencies.  Furthermore, this supports built-in packages which
;; may or may not be newer than user-specified packages.  This makes
;; it possible to upgrade Emacs and automatically disable packages
;; which have moved from external to core.  (Note though that we don't
;; currently register any of these, so this feature does not actually
;; work.)

;;; Code:
")

  (package-raw-info test-package)
  ;; =>
  {:name "package"
   :desc "Simple package system for Emacs"
   :local-vars "-*- lexical-binding:t -*-"
   :headers {"package-requires" ["((tabulated-list \"1.0\"))"]
             "keywords" ["tools"]
             "version" ["1.1.0"]
             "created" ["10 Mar 2007"]
             "author" ["Tom Tromey <tromey@redhat.com>" "Daniel Hackney <dan@haxney.org>"]}
   :comment "The idea behind package.el is to be able to download packages and\ninstall them.  Packages are versioned and have versioned\ndependencies.  Furthermore this supports built-in packages which\nmay or may not be newer than user-specified packages.  This makes\nit possible to upgrade Emacs and automatically disable packages\nwhich have moved from external to core.  (Note though that we don't\ncurrently register any of these so this feature does not actually\nwork.)"})
