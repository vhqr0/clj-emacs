(ns clj-emacs.elpa.tool
  (:require [clj-emacs.elpa.api :as api]))

(defn archive-package
  [opts]
  (api/archive-package (api/create-basis opts) (:package opts)))

(defn delete-package
  [opts]
  (api/delete-package (api/create-basis opts) (:package opts)))

(defn generate-index
  [opts]
  (api/generate-index (api/create-basis opts)))

(defn update-package
  [opts]
  (api/update-package (api/create-basis opts) (:package opts)))

(defn update-packages
  [opts]
  (api/update-packages (api/create-basis opts)))
