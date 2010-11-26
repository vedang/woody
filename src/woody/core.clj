(ns woody.core
  (:use [clojure.walk :only [keywordize-keys]])
  (:import [com.ctc.wstx.stax WstxInputFactory]
           [javax.xml.stream XMLStreamConstants]))

(def *default-file* "/home/vedang/Work/woody/hn.xml")

(def *wstx-factory* (WstxInputFactory.))
(.configureForLowMemUsage *wstx-factory*)

(defn get-row-and-push
  "Extract all relevant data from one row entry in the file"
  [x]
  (loop [m {}
	 e (.next x)
	 d 1]
    (cond
     (= d 0) m
     (= e XMLStreamConstants/START_ELEMENT) (recur (merge m (extract-element x))
						   (.next x)
						   (inc d))
     (= e XMLStreamConstants/END_ELEMENT) (recur m
						 (.next x)
						 (dec d))
     :else (recur m
		  (.next x)
		  (d)))))

(defn extract-element
  "takes a XMLStreamReader pointing at a START_ELEMENT, and returns
it as a map with it's text value. Use only on inner elements of a row"
  [x]
  (let [key (.getLocalName x)]
    (.next x)
    (hash-map key (.getText x))))

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
                                               (= depth 1) (recur (get-row-and-push x)
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
