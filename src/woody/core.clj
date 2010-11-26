(ns woody.core
  (:use [clojure.walk :only [keywordize-keys]])
  (:import [com.ctc.wstx.stax WstxInputFactory]
           [javax.xml.stream XMLStreamConstants]))

(def *default-file* "/home/vedang/Work/woody/hn.xml")

(def *wstx-factory* (WstxInputFactory.))
(.configureForLowMemUsage *wstx-factory*)

(defn get-row
  "Extract all relevant data from one row entry in the file"
  [x]
  (loop [m {}
         e 1
         d 0]
    (cond
     (= e XMLStreamConstants/START_ELEMENT) (recur (merge m (extract-element x))
                                                   (.next x)
                                                   (inc d))
     (and (= e XMLStreamConstants/END_ELEMENT) (not (= d 0))) (recur m
                                                                     (.next x)
                                                                     (dec d))
     (= e XMLStreamConstants/END_DOCUMENT) (keywordize-keys m)
     (= d 0) (keywordize-keys m)
     :else (recur m
                  (.next x)
                  d))))

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
           count 0
           depth 0]
      (cond
       (= e XMLStreamConstants/END_DOCUMENT) (do
                                               (println "Done")
                                               (.close x))
       (= e XMLStreamConstants/START_ELEMENT) (cond
                                               ;; this is a row and we should extract it
                                               ;; and push it to mongo
                                               (= depth 1) (recur (get-row x)
                                                                  XMLStreamConstants/END_ELEMENT
                                                                  (inc count)
                                                                  (inc depth))
                                               ;; We should just ignore this
                                               :else (recur m
                                                            (.next x)
                                                            count
                                                            (inc depth)))
       (= e XMLStreamConstants/END_ELEMENT) (recur m
                                                   (.next x)
                                                   count
                                                   (dec depth))
       :else (recur m
                    (.next x)
                    count
                    depth)))))

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
