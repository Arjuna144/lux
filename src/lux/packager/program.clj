;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.packager.program
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail*]]
                 [host :as &host])
            (lux.compiler [base :as &&]))
  (:import (java.io InputStream
                    File
                    FileInputStream
                    FileOutputStream
                    BufferedInputStream
                    ByteArrayOutputStream)
           (java.util.jar Manifest
                          Attributes$Name
                          JarEntry
                          JarInputStream
                          JarOutputStream
                          )))

;; [Utils]
(def ^:private kilobyte 1024)

(defn ^:private manifest [^String module]
  "(-> Text Manifest)"
  (doto (new Manifest)
    (-> .getMainAttributes (doto (.put Attributes$Name/MAIN_CLASS (str (&host/->module-class module) "._"))
                             (.put Attributes$Name/MANIFEST_VERSION "1.0")))))

(defn ^:private write-class! [^String path ^File file ^JarOutputStream out]
  "(-> Text File JarOutputStream Unit)"
  (with-open [in (new BufferedInputStream (new FileInputStream file))]
    (let [buffer (byte-array (* 10 kilobyte))]
      (doto out
        (.putNextEntry (new JarEntry (str path "/" (.getName file))))
        (-> (.write buffer 0 bytes-read)
            (->> (when (not= -1 bytes-read))
                 (loop [bytes-read (.read in buffer)])))
        (.flush)
        (.closeEntry)
        ))
    ))

(let [output-dir-size (.length &&/output-dir)]
  (defn ^:private write-module! [^File file ^JarOutputStream out]
    "(-> File JarOutputStream Unit)"
    (let [module-name (.substring (.getPath file) output-dir-size)
          inner-files (.listFiles file)
          inner-modules (filter #(.isDirectory ^File %) inner-files)
          inner-classes (filter #(not (.isDirectory ^File %)) inner-files)]
      (doseq [$class inner-classes]
        (write-class! module-name $class out))
      (doseq [$module inner-modules]
        (write-module! $module out)))))

(defn ^:private fetch-available-jars []
  (->> ^java.net.URLClassLoader (ClassLoader/getSystemClassLoader)
       (.getURLs)
       (map #(.getFile ^java.net.URL %))
       (filter #(.endsWith ^String % ".jar"))))

(let [init-capacity (* 100 1024)
      buffer-size 1024]
  (defn ^:private ^"[B" read-stream [^InputStream is]
    (let [buffer (byte-array buffer-size)]
      (with-open [os (new ByteArrayOutputStream init-capacity)]
        (loop [bytes-read (.read is buffer 0 buffer-size)]
          (when (not= -1 bytes-read)
            (do (.write os buffer 0 bytes-read)
              (recur (.read is buffer 0 buffer-size)))))
        (.toByteArray os)))))

(defn ^:private add-jar! [^File jar-file ^JarOutputStream out]
  (with-open [is (->> jar-file (new FileInputStream) (new JarInputStream))]
    (loop [^JarEntry entry (.getNextJarEntry is)]
      (when entry
        (when (and (not (.isDirectory entry))
                   (not (.startsWith (.getName entry) "META-INF/")))
          (let [entry-data (read-stream is)]
            (doto out
              (.putNextEntry entry)
              (.write entry-data 0 (alength entry-data))
              (.flush)
              (.closeEntry))))
        (recur (.getNextJarEntry is))))))

;; [Resources]
(defn package [module]
  "(-> Text (,))"
  (with-open [out (new JarOutputStream (->> &&/output-package (new File) (new FileOutputStream)) (manifest module))]
    (doseq [$group (.listFiles (new File &&/output-dir))]
      (write-module! $group out))
    (doseq [^String jar-file (fetch-available-jars)]
      (add-jar! (new File jar-file) out))
    ))
