(ns bank-ocr.core
  (:require [bank-ocr.parse :refer [parse-file write-file]])
  (:import (java.io BufferedReader BufferedWriter FileReader FileWriter))
  (:gen-class))

(defn -main
  "Accepts an input file name and an output file name"
  [& args]
  (if (not= 2 (count args))
    (println "This procedure accepts two arguments: an intput file and an output file")
    (with-open [in (->> args
                        first
                        FileReader.
                        BufferedReader.)
                out (->> args
                         second
                         FileWriter.
                         BufferedWriter.)]
      (-> (parse-file in)
          (write-file out)))))
