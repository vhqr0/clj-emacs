(ns clj-emacs.elpa.api
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clj-emacs.elpa :as elpa])
  (:import [java.io File]
           [org.apache.commons.compress.archivers.tar TarArchiveOutputStream TarArchiveEntry]))

(set! clojure.core/*warn-on-reflection* true)

(comment
  :elpa.deps.edn
  {:deps {;; path: relative path of file or dir from basis
          :package-1 {:path "./package-1.el"}
          :package-2 {:path "./package-2/src"}}})

(def ^:private default-opts
  {:project-path "."
   :deps-path "elpa.deps.edn"
   :archive-path "archive"})

(def ^:private archive-name-re
  #"^(.+)-([0-9.]+)\.(eld|tar)$")

(comment
  (re-matches archive-name-re "package-1.0.0.tar") ; => ["package-1.0.0.tar" "package" "1.0.0" "tar"]
  )

(defn- ->file
  (^File [^String path] (File. path))
  (^File [^File dir ^String path] (File. dir path)))

(defn ^:api create-basis
  [opts]
  (let [{:keys [project-path deps-path] :as opts} (merge default-opts opts)
        project-dir (.getCanonicalFile (->file project-path))
        deps (when (some? deps-path)
               (let [deps-file (->file project-dir deps-path)]
                 (when (.isFile deps-file)
                   (-> deps-file slurp edn/read-string))))]
    (merge opts deps {:project-dir project-dir})))

(defn- get-archive-dir
  ^File [basis]
  (let [{:keys [project-dir archive-path]} basis]
    (->file project-dir archive-path)))

(defn- get-archive-files
  [basis]
  (let [archive-dir (get-archive-dir basis)]
    (when (.isDirectory archive-dir)
      (->> (.listFiles archive-dir)
           (keep
            (fn [^File file]
              (when-let [[_ name version ext] (re-matches archive-name-re (.getName file))]
                {:file file :name name :version version :ext ext})))))))

(defn- get-tar-files
  [basis]
  (->> (get-archive-files basis) (filter #(= "tar" (:ext %)))))

(defn get-meta-files
  [basis]
  (->> (get-archive-files basis) (filter #(= "eld" (:ext %)))))

(defn- get-package-file
  ^File [basis package]
  (if-let [{:keys [path]} (get-in basis [:deps package])]
    (->file (:project-dir basis) path)
    (throw (ex-info "no such package" {:reason ::no-such-package}))))

(defn- slurp-package-file
  [^File file package]
  (-> (cond-> file
        (.isDirectory file)
        (->file (str (name package) ".el")))
      slurp
      str/split-lines
      elpa/parse-package
      elpa/expand-package-info))

(defn- get-package-info
  [basis package]
  (let [file (get-package-file basis package)
        info (merge (slurp-package-file file package) {:file file})]
    (if (= (name package) (:name info))
      info
      (throw (ex-info "invalid package info" {:reason ::invalid-package-info})))))

(defn- spit-package-meta
  [^File archive-dir info name version]
  (spit
   (->file archive-dir (str name \- version ".eld"))
   (elpa/package-archive-text info)))

(defn- tar-archive-spit-file
  [^TarArchiveOutputStream tos ^File file ^String name]
  (let [entry (.createArchiveEntry tos file name)]
    (.putArchiveEntry tos entry)
    (io/copy file tos)
    (.closeArchiveEntry tos)))

(defn- tar-archive-spit-data
  [^TarArchiveOutputStream tos ^bytes data ^String name]
  (let [entry (TarArchiveEntry. name)]
    (.setSize entry (alength data))
    (.putArchiveEntry tos entry)
    (.write tos data)
    (.closeArchiveEntry tos)))

(defn- tar-archive-spit-pacakge-files
  [^TarArchiveOutputStream tos ^File file name version]
  (if-not (.isDirectory file)
    (let [entry (str name \- version \/ name ".el")]
      (tar-archive-spit-file tos file entry))
    (doseq [^File sub-file (.listFiles file)]
      (let [sub-file-name (.getName sub-file)]
        (when (str/ends-with? sub-file-name ".el")
          (let [entry (str name \- version \/ sub-file-name)]
            (tar-archive-spit-file tos sub-file entry)))))))

(defn- tar-archive-spit-package-meta-file
  [^TarArchiveOutputStream tos info name version]
  (let [data (.getBytes ^String (elpa/package-define-text info))
        entry (str name \- version \/ name "-pkg.el")]
    (tar-archive-spit-data tos data entry)))

(defn- spit-package-tar
  [^File archive-dir ^File file info name version]
  (with-open [fos (io/output-stream (->file archive-dir (str name \- version ".tar")))
              tos (TarArchiveOutputStream. fos)]
    (tar-archive-spit-pacakge-files tos file name version)
    (tar-archive-spit-package-meta-file tos info name version)
    (.finish tos)))

(defn- spit-archive-files
  [^File archive-dir {:keys [file name version] :as info}]
  (.mkdirs archive-dir)
  (spit-package-meta archive-dir info name version)
  (spit-package-tar archive-dir file info name version))

(defn ^:api create-package
  [basis package]
  (let [info (get-package-info basis package)]
    (spit-archive-files (get-archive-dir basis) info)))

(defn ^:api delete-package
  [basis package]
  (let [target-name (name package)]
    (doseq [{:keys [name file]} (get-archive-files basis)]
      (when (= name target-name)
        (.delete ^File file)))))

(defn ^:api create-index
  [basis]
  (let [archive-dir (get-archive-dir basis)]
    (.mkdirs archive-dir)
    (with-open [writer (io/writer (->file archive-dir "archive-contents"))]
      (.write writer "(1")
      (doseq [{:keys [file ext]} (get-meta-files basis)]
        (.write writer "\n")
        (io/copy file writer))
      (.write writer ")"))))

(defn ^:api update-package
  [basis package]
  (delete-package basis package)
  (create-package basis package)
  (create-index basis))

(defn ^:api update-packages
  [basis]
  (let [packages (keys (:deps basis))]
    (doseq [package packages]
      (delete-package basis package)
      (create-package basis package))
    (create-index basis)))
