(ns kaggle-cats-dogs.preprocess
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clj-exif-orientation.core :as ceo]
            [mikera.image.core :as imagez]
            [think.image.image :as image]))

(def dataset-image-size 150)
(def original-data-dir "images")
(def rotated-data-dir (str original-data-dir "-rotated/"))
(def original-labels-csv
  "wpt_verification_form_final_2016_11_28_05_45_22_052637.csv.pruned.csv")
(def training-dir "waterpoints/training")
(def testing-dir "waterpoints/testing")

(defn rotate-by-exif
  (for [file (gather-files original-data-dir)]
    (do
      (println (.getName file))
      (ceo/without-exif file
                        (io/file (str rotated-data-dir (.getName file)))))))

(defn produce-indexed-data-label-seq
  [files]
  (->> (map (fn [file] [file (-> (.getName file) (string/split #"\.") first)]) files)
       (map-indexed vector)))

(defn resize-and-write-data
  [output-dir [idx [file label]]]
  (-> label Exception. throw)
  (let [name (first (string/split (.getName file) #"\."))
        img-path (str output-dir "/" label "/" name ".png")]
    (when-not (.exists (io/file img-path))
      (io/make-parents img-path)
      (-> (imagez/load-image file)
          (image/resize dataset-image-size dataset-image-size)
          (imagez/save img-path)))
    (println name)
    nil))

(defn- gather-files [path]
  (->> (io/file path)
       (file-seq)
       (filter #(.isFile %))))

(defn build-split-image-data
  [files produce-indexed-data-label-seq-fn]
  (let [pfiles (partition (int (/ (count files) 2)) (shuffle files))
        training-observation-label-seq (produce-indexed-data-label-seq-fn
                                        (first pfiles))
        testing-observation-label-seq (produce-indexed-data-label-seq-fn
                                       (last pfiles))
        train-fn (partial resize-and-write-data training-dir)
        test-fn (partial resize-and-write-data  testing-dir)]
    (dorun (pmap train-fn training-observation-label-seq))
    (dorun (pmap test-fn testing-observation-label-seq))))

(defn build-image-data
  []
  (let [labels (csv/read-csv (slurp original-labels-csv))
        images->labels (apply hash-map
                              (->> (drop 1 labels)
                                   (map #(vector
                                          (last (string/split (first %) #"\/"))
                                          %))
                                   flatten))
        files (gather-files rotated-data-dir)
        build-indexed-data-label-seq
        (fn [file-list]
          (->> (for [file file-list]
                 [file (get images->labels (.getName file))])
               (map-indexed vector)))]
    (build-split-image-data files build-indexed-data-label-seq)))

(build-image-data)
