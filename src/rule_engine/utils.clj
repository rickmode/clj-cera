(ns rule-engine.utils
  (:use [clojure.pprint :only [pprint]]))

(defmacro dbg
  "dbg macro with suggested improvements as described at
http://learnclojure.blogspot.com/2010/09/clojure-macro-tutorial-part-i-getting.html"
  ([x] `(let [x# ~x] (println '~x "=" x#) x#))
  ([x & more] `(let [x# (~x ~@more)] (print "(" '~x "") (apply print '~more) (println " ) =" x#) x#)))

(defmacro pdbg
  "dbg macro with pretty printing."
  ([x] `(let [x# ~x] (println '~x "=" x#) (pprint x#) (println) x#))
  ([x & more] `(let [x# (~x ~@more)] (print "(" '~x "") (apply print '~more) (println " ) =" x#) (pprint x#) (newline) x#)))

(defn date
  ([] (System/currentTimeMillis))
  ([ms] ms))

(defn date-sec
  [s] (date (* s 1000)))

(defn date-min
  [m] (date (* m 60 1000)))
