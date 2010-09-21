(ns rule-engine.utils)

(defmacro dbg
  "dbg macro described at
  http://learnclojure.blogspot.com/2010/09/clojure-macro-tutorial-part-i-getting.html"
  ([x] `(let [x# ~x] (println '~x "=" x#) x#))
  ([x & more] `(let [x# (~x ~@more)] (println "(" '~x ~@more ") =" x#) x#)))

