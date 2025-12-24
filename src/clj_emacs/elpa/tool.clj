(ns clj-emacs.elpa.tool
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clj-emacs.eld :as eld]
            [clj-emacs.elpa :as elpa])
  (:import [java.io File]
           [org.apache.commons.compress.archivers.tar TarArchiveOutputStream TarArchiveEntry]))

(set! clojure.core/*warn-on-reflection* true)

(comment
  :elpa.deps.edn
  {:deps {;; path: relative path of file or dir from basis
          :package-1 {:path "./package-1.el"}
          :package-2 {:path "./package-2/src"}}})

(def default-opts
  {:project-path "."
   :deps-path "elpa.deps.edn"
   :archive-path "archive"})

(defn file
  (^File [^String path] (File. path))
  (^File [^File dir ^String path] (File. dir path)))

(defn file-exists?
  [^File file]
  (.exists file))

(defn file-dir?
  [^File file]
  (.isDirectory file))

(defn create-basis
  [opts]
  (let [{:keys [project-path deps-path] :as opts} (merge default-opts opts)
        project-dir (.getCanonicalFile (file project-path))
        deps-file (file project-dir deps-path)]
    (merge
     opts
     (when (file-exists? deps-file) (-> deps-file slurp edn/read-string))
     {:project-dir project-dir})))

(defn archive-dir
  ^File [basis]
  (let [{:keys [project-dir archive-path]} basis]
    (file project-dir archive-path)))

(defn package-file
  [basis package]
  (when-let [{:keys [path]} (get-in basis [:deps package])]
    (let [{:keys [project-dir]} basis]
      (file project-dir path))))

(defn archive-package
  ([opts]
   (archive-package (create-basis opts) (:package opts)))
  ([basis package]
   (when-let [^File package-file (package-file basis package)]
     (when (file-exists? package-file)
       (let [package-dir? (file-dir? package-file)
             package-name (name package)
             package-main-file (if-not package-dir?
                                 package-file
                                 (file package-file (str package-name ".el")))
             package-info (->> package-main-file
                               slurp
                               str/split-lines
                               elpa/parse-package
                               elpa/expand-package-info)
             package-version (:version package-info)
             package-name-version (str package-name \- package-version)
             package-define-text (str (elpa/package-define-text package-info))
             package-archive-text (str (elpa/package-archive-text package-info))
             archive-dir (archive-dir basis)
             archive-package-tar-file (file archive-dir (str package-name-version ".tar"))
             archive-package-meta-file (file archive-dir (str package-name-version ".eld"))]
         (.mkdirs archive-dir)
         (spit archive-package-meta-file package-archive-text)
         (with-open [fos (io/output-stream archive-package-tar-file)
                     tos (TarArchiveOutputStream. fos)]
           (if-not package-dir?
             (let [entry (.createArchiveEntry tos package-file (str package-name-version \/ package-name ".el"))]
               (.putArchiveEntry tos entry)
               (io/copy package-file tos)
               (.closeArchiveEntry tos))
             (doseq [^File package-sub-file (.listFiles package-file)]
               (let [package-sub-file-name (.getName package-sub-file)]
                 (when (str/ends-with? package-sub-file-name ".el")
                   (let [entry (.createArchiveEntry tos package-sub-file (str package-name-version \/ package-sub-file-name))]
                     (.putArchiveEntry tos entry)
                     (io/copy package-sub-file tos)
                     (.closeArchiveEntry tos))))))
           (let [data (.getBytes package-define-text)
                 entry (TarArchiveEntry. (str package-name-version \/ package-name "-pkg.el"))]
             (.setSize entry (alength data))
             (.putArchiveEntry tos entry)
             (.write tos data)
             (.closeArchiveEntry tos))
           (.finish tos)))))))
