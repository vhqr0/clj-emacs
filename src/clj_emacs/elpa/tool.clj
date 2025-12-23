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
     (when (.exists deps-file) (-> deps-file slurp edn/read-string))
     {:project-dir project-dir :uuid (random-uuid)})))

(defn tmp-dir
  ^File [basis]
  (let [{:keys [uuid]} basis]
    (File. (System/getProperty "java.io.tmpdir") (str "elpa-" uuid))))

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
             package-define-data (str ";; -*- no-byte-compile: t; lexical-binding: nil -*-\n"
                                      (-> package-info elpa/package-define-data eld/clj->eld))
             package-archive-data (-> package-info elpa/package-archive-data eld/clj->eld)
             tmp-dir (tmp-dir basis)
             archive-dir (archive-dir basis)
             tmp-package-dir (file tmp-dir (str package-name \- package-version))
             tmp-package-meta-file (file tmp-package-dir (str package-name "-pkg.el"))
             archive-package-file (file archive-dir (str package-name \- package-version ".tar"))
             archive-package-meta-file (file archive-dir (str package-name \- package-version ".eld"))]
         ;; make sure dirs were created
         (.mkdirs tmp-package-dir)
         (.mkdirs archive-dir)
         ;; write meta data first
         (spit archive-package-meta-file package-archive-data)
         (spit tmp-package-meta-file package-define-data)
         ;; copy package files to tmp dir
         (if-not package-dir?
           (io/copy package-file (file tmp-package-dir (str package-name ".el")))
           (doseq [^File package-sub-file (.listFiles package-file)]
             (let [package-sub-path (.getName package-sub-file)]
               (when (str/ends-with? package-sub-path ".el")
                 (io/copy package-sub-file (file tmp-package-dir package-sub-path))))))
         ;; tar tmp dir to archive file
         (with-open [fos (io/output-stream archive-package-file)
                     tos (TarArchiveOutputStream. fos)]
           (let [prefix (str package-name \- package-version \/)]
             (doseq [^File archive-sub-file (.listFiles tmp-package-dir)]
               (let [entry (.createArchiveEntry tos archive-sub-file (str prefix (.getName archive-sub-file)))]
                 (.putArchiveEntry tos entry)
                 (with-open [fis (io/input-stream archive-sub-file)]
                   (io/copy fis tos))
                 (.closeArchiveEntry tos)))
             (.finish tos))))))))
