(ns woody.core
  (:require [clojure.contrib.lazy-xml :as lxml])
  (:import [com.ctc.wstx.stax WstxInputFactory]
           [javax.xml.stream XMLStreamConstants]))

(def *default-file* "/home/vedang/Work/woody/hn.xml")

(defn main
  "Things start here"
  [file]
  (let [w (WstxInputFactory.)]
    (.configureForLowMemUsage w)
    (let [x (.createXMLStreamReader w (java.io.File. file))]
      (loop [e (.next x)
             depth 0]
        (cond
         (= e XMLStreamConstants/END_DOCUMENT) (do
						 (println "Done")
						 (.close x))
         (= e XMLStreamConstants/START_ELEMENT) (do
                                                  (println "d: " (inc depth) " start of name: " (.getName x))
						  (recur (.next x) (inc depth)))
         (= e XMLStreamConstants/END_ELEMENT) (recur (.next x) (dec  depth))
	 :else (recur (.next x) depth))))))
