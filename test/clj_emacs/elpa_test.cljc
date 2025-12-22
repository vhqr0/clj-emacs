(ns clj-emacs.elpa-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clj-emacs.eld :as eld]
            [clj-emacs.elpa :as elpa]))

(def test-first-line
  ";;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-")

(def test-header-line
  ";; Author: Tom Tromey <tromey@redhat.com>")

(def test-header-continue-line
  ";;         Daniel Hackney <dan@haxney.org>")

(def test-header-text-1
  "
;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;;         Daniel Hackney <dan@haxney.org>
;; Created: 10 Mar 2007
;; Version: 1.1.0
;; Keywords: tools
;; Package-Requires: ((tabulated-list \" 1.0 \"))

;; This file is part of GNU Emacs.")

(def test-header-text-2
  "
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

(def test-package-text
  "
;;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-

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

(deftest elpa-test
  (is (= [test-first-line
          "package"
          "Simple package system for Emacs"
          "-*- lexical-binding:t -*-"]
         (re-matches elpa/first-line-re test-first-line)))
  (is (= [test-header-line
          "Author"
          "Tom Tromey <tromey@redhat.com>"]
         (re-matches elpa/header-line-re test-header-line)))
  (is (= [test-header-continue-line
          "  "
          "Daniel Hackney <dan@haxney.org>"]
         (re-matches elpa/header-continue-line-re test-header-continue-line)))
  (is (= {"package-requires" ["((tabulated-list \" 1.0 \"))"]
          "keywords"         ["tools"]
          "version"          ["1.1.0"]
          "created"          ["10 Mar 2007"]
          "author"           ["Tom Tromey <tromey@redhat.com>" "Daniel Hackney <dan@haxney.org>"]}
         (elpa/parse-headers (str/split-lines test-header-text-1))))
  (is (= {"author"                  ["Marius Vollmer <marius.vollmer@gmail.com>" "Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>"]
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
                                     "Yann Hodique <yann.hodique@gmail.com>"]}
         (elpa/parse-headers (str/split-lines test-header-text-2))))
  (is (= {:name "package"
          :desc "Simple package system for Emacs"
          :local-vars "-*- lexical-binding:t -*-"
          :headers {"package-requires" ["((tabulated-list \"1.0\"))"]
                    "keywords" ["tools"]
                    "version" ["1.1.0"]
                    "created" ["10 Mar 2007"]
                    "author" ["Tom Tromey <tromey@redhat.com>" "Daniel Hackney <dan@haxney.org>"]}
          :comment "The idea behind package.el is to be able to download packages and\ninstall them.  Packages are versioned and have versioned\ndependencies.  Furthermore, this supports built-in packages which\nmay or may not be newer than user-specified packages.  This makes\nit possible to upgrade Emacs and automatically disable packages\nwhich have moved from external to core.  (Note though that we don't\ncurrently register any of these, so this feature does not actually\nwork.)"}
         (elpa/parse-package (rest (str/split-lines test-package-text)))))
  (is (= [{:name "Tom Tromey" :address "tromey@redhat.com"} {:name "Daniel Hackney" :address "dan@haxney.org"}]
         (elpa/parse-persons ["Tom Tromey <tromey@redhat.com>" "Daniel Hackney <dan@haxney.org>"])))
  (is (= ["git" "tools" "vc"]
         (elpa/parse-keywords ["git tools" "vc"])))
  (is (= {:version "1.1.0"
          :requires [{:name "tabulated-list" :version "1.0"}]
          :keywords ["tools"]
          :authors [{:name "Tom Tromey" :address "tromey@redhat.com"}
                    {:name "Daniel Hackney" :address "dan@haxney.org"}]}
         (-> (rest (str/split-lines test-package-text))
             elpa/parse-package
             elpa/expand-package-info
             (select-keys [:version :requires :keywords :url :authors :maintainers]))))
  (is (= (str "(define-package \"package\" \"1.1.0\" \"Simple package system for Emacs\" '((tabulated-list \"1.0\")) :keywords '(\"tools\") "
              ":authors '((\"Tom Tromey\" . \"tromey@redhat.com\") (\"Daniel Hackney\" . \"dan@haxney.org\")) "
              ":maintainers '((\"Tom Tromey\" . \"tromey@redhat.com\") (\"Daniel Hackney\" . \"dan@haxney.org\")))")
         (-> (rest (str/split-lines test-package-text))
             elpa/parse-package
             elpa/expand-package-info
             elpa/package-define-data
             eld/clj->eld)))
  (is (= (str "(package . [(1 1 0) ((tabulated-list (1 0))) \"Simple package system for Emacs\" tar ((:keywords \"tools\") "
              "(:authors (\"Tom Tromey\" . \"tromey@redhat.com\") (\"Daniel Hackney\" . \"dan@haxney.org\")) "
              "(:maintainer (\"Tom Tromey\" . \"tromey@redhat.com\") (\"Daniel Hackney\" . \"dan@haxney.org\")))])")
         (-> (rest (str/split-lines test-package-text))
             elpa/parse-package
             elpa/expand-package-info
             elpa/package-archive-data
             eld/clj->eld))))
