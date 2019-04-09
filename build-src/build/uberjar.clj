(ns build.uberjar
  (:require [clojure.edn :as edn]
            [clojure.java.io :as jio]
            [clojure.java.io :as io])
  
  (:import [java.io InputStream OutputStream PushbackReader]
           [java.nio.file CopyOption LinkOption OpenOption
            StandardCopyOption StandardOpenOption
            FileSystem FileSystems Files
            FileVisitResult FileVisitor
            Path]
           [java.nio.file.attribute BasicFileAttributes FileAttribute]
           [java.util.jar JarInputStream JarOutputStream JarEntry Manifest Attributes]))

;; Stolen from https://github.com/seancorfield/depstar/blob/master/src/hf/depstar/uberjar.clj
;; Amended to have some more useful features

(defonce ^FileSystem FS (FileSystems/getDefault))

(defn md5 [input]
  (with-open [input (io/input-stream input)]
    (let [md (java.security.MessageDigest/getInstance "MD5")
          is (java.security.DigestInputStream. input md)]

      (while (not= -1 (.read is)))
      (javax.xml.bind.DatatypeConverter/printHexBinary (.digest md)))))

(defn path
  ^Path [s]
  (.getPath FS s (make-array String 0)))

(defmulti map-classpath
  (fn [f src]
    (let [p (path src)
          symlink-opts (make-array LinkOption 0)]
      (if (Files/exists p symlink-opts)
        (cond
          (Files/isDirectory p symlink-opts)
          :directory

          (and (Files/isRegularFile p symlink-opts)
               (re-find #"\.jar$" (.toString p)))
          :jar

          :else :unknown)
        :not-found))))

(defmethod map-classpath :jar [f src]
  (with-open [is (-> src
                     (path)
                     (Files/newInputStream (make-array OpenOption 0))
                     java.io.BufferedInputStream.
                     JarInputStream.)]
    (loop []
      (when-let [entry (try (.getNextJarEntry is) (catch Exception _))]
        (let [name (.getName entry)
              last-modified (.getLastModifiedTime entry)]
          (when-not (.isDirectory entry)
            (f name is last-modified)))
        (recur)))))

(defmethod map-classpath :directory [f src]
  (let [src (path src)
        vis (reify FileVisitor
              (visitFile [_ p attrs]
                (let [rel (.relativize src p)]
                  (with-open [is (Files/newInputStream p (make-array OpenOption 0))]
                    (f (.toString rel) is (.lastModifiedTime attrs))))
                FileVisitResult/CONTINUE)
              (preVisitDirectory [_ p attrs] FileVisitResult/CONTINUE)
              (postVisitDirectory [_ p ioexc]
                (if ioexc (throw ioexc) FileVisitResult/CONTINUE))
              (visitFileFailed [_ p ioexc] (throw (ex-info "Visit File Failed" {:p p} ioexc))))]
    (Files/walkFileTree src vis)))

(defmethod map-classpath :not-found [_ src]
  (println "Not able to get contents for" src))

(defn write-jar
  [^Path src ^Path target]
  (with-open [os (-> target
                     (Files/newOutputStream (make-array OpenOption 0))
                     JarOutputStream.)]
    (let [walker (reify FileVisitor
                   (visitFile [_ p attrs]
                     (let [t (.lastModifiedTime attrs)
                           e (JarEntry. (.toString (.relativize src p)))]
                       (.putNextEntry os (.setLastModifiedTime e t)))
                     (Files/copy p os)
                     FileVisitResult/CONTINUE)
                   (preVisitDirectory [_ p attrs]
                     (when (not= src p) ;; don't insert "/" to zip
                       (.putNextEntry os (JarEntry. (str (.relativize src p) "/")))) ;; directories must end in /
                     FileVisitResult/CONTINUE)
                   (postVisitDirectory [_ p ioexc]
                     (if ioexc (throw ioexc) FileVisitResult/CONTINUE))
                   (visitFileFailed [_ p ioexc] (throw ioexc)))]
      (Files/walkFileTree src walker)))
  :ok)

(def default-skip-files #{#"^project.clj$"
                          #"^LICENSE$"
                          #"^COPYRIGHT$"
                          #"(?i)META-INF/.*\.(?:MF|SF|RSA|DSA)"
                          #"(?i)META-INF/(?:INDEX\.LIST|DEPENDENCIES|NOTICE|LICENSE)(?:\.txt)?"})

(def default-skip-jars #{#"depstar"})

(defn merge-edn [target in]
  (let [er #(with-open [r (PushbackReader. %)] (edn/read r))
        f1 (er (jio/reader in))
        f2 (er (Files/newBufferedReader target))]
    (with-open [w (Files/newBufferedWriter target (make-array OpenOption 0))]
      (binding [*out* w]
        (prn (merge f1 f2))))))

(defn concat-lines [target in]
  (let [f1 (line-seq (jio/reader in))
        f2 (Files/readAllLines target)]
    (with-open [w (Files/newBufferedWriter target (make-array OpenOption 0))]
      (binding [*out* w]
        (run! println (-> (vec f1)
                          (conj "\n")
                          (into f2)))))))

(defn default-merge-strategy [name]
  (cond
    (= "data_readers.clj" name)
    merge-edn
    
    (or (= "META-INF/registryFile.jaiext" name)
        (re-find #"^META-INF/services/" name))
    concat-lines
    
    :else nil))

(defn some-re [res]
  (let [res (filter identity res)]
    (fn [s]
      (loop [[re & res] res]
        (cond (and re (re-find re s)) true
              (seq res) (recur res)
              :else false)))))

(defn create-uberjar [target & {:keys [classpath skip-jars skip-files merge-strategy manifest]
                                :or {classpath (System/getProperty "java.class.path")
                                     skip-jars default-skip-jars
                                     skip-files default-skip-files
                                     merge-strategy default-merge-strategy}}]
  (let [classpath
        (cond-> classpath
          (string? classpath)
          (-> (.split (System/getProperty "path.separator"))
              (vec)))

        skip-jars  (some-re skip-jars)
        skip-files (some-re skip-files)
        
        tmp (Files/createTempDirectory "uberjar" (make-array FileAttribute 0))

        owners (atom {})
        ]

    (doseq [classpath-entry classpath]
      (when-not (skip-jars classpath-entry)
        (map-classpath
         (fn [name is last-mod]
           (when-not (skip-files name)
             (let [target (.resolve tmp name)]
               (if (Files/exists target (make-array LinkOption 0))
                 ;; we must resolve a clash
                 (let [merger (merge-strategy name)]
                   (if merger
                     (merger target is)
                     (let [f1 classpath-entry
                           f2 (get @owners name)

                           m1 (md5 f1)
                           m2 (md5 f2)]
                       
                       (when-not (= m1 m2)
                         (println "Conflict:" name)
                         (println "✗" classpath-entry m1)
                         (println "✔" (get @owners name) m2)))))
                 (do
                   (swap! owners assoc name classpath-entry)
                   (Files/createDirectories (.getParent target) (make-array FileAttribute 0))
                   (Files/copy is target ^"[Ljava.nio.file.CopyOption;" (make-array CopyOption 0))
                   (when last-mod
                     (Files/setLastModifiedTime target last-mod)))))))
         
         classpath-entry)))

    (when manifest
      (println "Creating manifest")
      (let [m (Manifest.)
            a (.getMainAttributes m)]
        (.put a java.util.jar.Attributes$Name/MANIFEST_VERSION "1.0.0")
        (doseq [[k v] manifest]
          (let [v (if (and (= k :Main-Class)
                           (symbol? v))
                    (-> (name v)
                        (.replaceAll "-" "_"))
                    (str v))
                k (if (keyword? k) (name k) (str k))
                ]
            (println " " k v)
            (.putValue a k v)))
        (let [manifest-file (.toFile (.resolve tmp "META-INF/MANIFEST.MF"))]
          (with-open [o (jio/output-stream manifest-file)]
            (.write m o)))))
        
    (println "Writing" target)
    (write-jar tmp (path target))))


