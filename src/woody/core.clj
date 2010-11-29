(ns woody.core
  (:require [somnium.congomongo :as c])
  (:use [clojure.walk :only [keywordize-keys]])
  (:import [com.ctc.wstx.stax WstxInputFactory]
           [javax.xml.stream XMLStreamConstants]))

(def *default-file* "/home/vedang/Work/woody/hn.xml")

(def *default-db* "hackernews")

(def *wstx-factory* (WstxInputFactory.))
(.configureForLowMemUsage *wstx-factory*)

(c/mongo! :db *default-db*)

(declare extract-element)

(defn get-row
  "Extract all relevant data from one row entry in the file"
  [x state]
  (loop [m {}
         e state]
    (cond
     (= e XMLStreamConstants/START_ELEMENT) (recur (merge m (extract-element x))
                                                   (.next x))
     (and (= e XMLStreamConstants/END_ELEMENT) (not (= (.getLocalName x) "row"))) (recur m
                                                                                         (.next x))
     (and (= e XMLStreamConstants/END_ELEMENT) (= (.getLocalName x) "row")) (keywordize-keys m)
     (= e XMLStreamConstants/END_DOCUMENT) (keywordize-keys m)
     :else (recur m
                  (.next x)))))

(defn extract-element
  "takes a XMLStreamReader pointing at a START_ELEMENT, and returns
it as a map with it's text value. Use only on inner elements of a row"
  [x]
  (let [key (.getLocalName x)
        e (.next x)]
    (if (= e XMLStreamConstants/CHARACTERS)
      (hash-map key (.getText x))
      {key " "})))

(defn main
  "Things start here"
  [file]
  (let [x (.createXMLStreamReader *wstx-factory*
                                  (java.io.File. file))]
    (loop [m {}
           e (.next x)
           count 0]
      (cond
       (= e XMLStreamConstants/END_DOCUMENT) (do
                                               (println "Done")
                                               (.close x))
       (= e XMLStreamConstants/START_ELEMENT) (cond
                                               ;; this is a row and we should extract it
                                               ;; and push it to mongo
                                               (= (.getLocalName x) "row") (recur (c/insert! :row (get-row x (.next x)))
                                                                                  XMLStreamConstants/END_ELEMENT
                                                                                  (inc count))
                                               ;; We should just ignore this
                                               :else (recur m
                                                            (.next x)
                                                            count))
       (= e XMLStreamConstants/END_ELEMENT) (recur m
                                                   (.next x)
                                                   count)
       :else (recur m
                    (.next x)
                    count)))))

;;; HN structure
;; <HackerNews>
;;  <row>
;;   <ID>1</ID>
;;   <Url>http://ycombinator.com</Url>
;;   <Title>Y Combinator</Title>
;;   <Text />
;;   <Username>pg</Username>
;;   <Points>39</Points>
;;   <Type>1</Type>
;;   <Timestamp>2006-10-09T20:33:12.700</Timestamp>
;;   <CommentCount>15</CommentCount>
;;   <ParentID />
;;  </row>
;; </HackerNews>

;; javax.xml.stream.XMLStreamConstants
;; public static final int      ATTRIBUTE       10
;; public static final int      CDATA   12
;; public static final int      CHARACTERS      4
;; public static final int      COMMENT         5
;; public static final int      DTD     11
;; public static final int      END_DOCUMENT    8
;; public static final int      END_ELEMENT     2
;; public static final int      ENTITY_DECLARATION      15
;; public static final int      ENTITY_REFERENCE        9
;; public static final int      NAMESPACE       13
;; public static final int      NOTATION_DECLARATION    14
;; public static final int      PROCESSING_INSTRUCTION  3
;; public static final int      SPACE   6
