(ns clj-emacs.elpa.tool
  (:refer-clojure :exclude [update])
  (:require [clj-emacs.elpa.api :as api]))

(defn basis
  [opts]
  (prn (-> (api/create-basis opts) (dissoc :project-dir))))

(defn create-package
  [opts]
  (api/create-package (api/create-basis opts) (:package opts)))

(defn delete-package
  [opts]
  (api/delete-package (api/create-basis opts) (:package opts)))

(defn create-index
  [opts]
  (api/create-index (api/create-basis opts)))

(defn update-package
  [opts]
  (api/update-package (api/create-basis opts) (:package opts)))

(defn update-packages
  [opts]
  (api/update-packages (api/create-basis opts)))

(defn update
  [opts]
  (if (contains? opts :package)
    (update-package opts)
    (update-packages opts)))
