(ns rule-engine.cera
  (:use [rule-engine.utils :only [dbg]])
  (:import [java.util Date]))

(comment
  (def-recognizer safing-complete
    (pattern
     '(all
       (safing (system foo) (status on))
       (safing (system bar) (status on))))
    (on-complete [st end]
                 (signal-event '(all-safed) st end))))

; TODO: not sure if status-values is needed
(def status-values {:complete 3 :active 2 :ignore 1 :futile 0})

(defrecord Status [value start finish])

(defn make-status
  "Create a Status."
  ([]
     (make-status nil nil nil))
  ([value]
     (make-status value nil nil))
  ([value start]
     (make-status value start start))
  ([value start finish]
     (Status. value start finish)))

(def empty-status (make-status nil nil nil))

(defn active? [status]
  (= (:value status) :active))

(defn complete? [status]
  (= (:value status) :complete))

(defn not-ended? [status]
  (let [v (:value status)] (and (not= v :complete) (not= v :futile))))


(defrecord Signal [tag data start finish])

(defn make-signal
  "Create a Signal."
  ([signal-tag data]
     (make-signal signal-tag data nil nil))
  ([signal-tag data start]
     (make-signal signal-tag data start start))
  ([signal-tag data start finish]
     (Signal. signal-tag data start finish)))

(defn signal-eq? [sig1 sig2]
  "Two signals are equal if everything except their start and finish times match."
  (and (= (:tag sig1) (:tag sig2))
       (= (:data sig1) (:data sig2))))

(defn status-from-signal
  "Create a Status using the times from a signal"
  [value signal] (make-status value (:start signal) (:finish signal)))


(defprotocol Recognizer
  "A recognizer tracks the state of detecting a pattern."
  (handle-signal [this probe]
                 "Returns a recognizer in its next state based on the probe."))

;; In BaseRecognizer, target is a Signal.
;; In all other Recognizers, target, targets, seen and remainder refer to other recognizers.

(defrecord BaseRecognizer [target status callback]
  Recognizer
  (handle-signal
   [this probe]
   {:pre [(not-ended? status)]}
   (if (signal-eq? target probe)
     (assoc this :status (status-from-signal :complete probe))
     (assoc this :status (status-from-signal :ignore probe)))))

(defrecord OneRecognizer [target status callback]
  Recognizer
  (handle-signal
   [this probe]
   {:pre [(not-ended? status)]}
   (let [new-target (handle-signal target probe)]
     (assoc this :target new-target :status (:status new-target)))))

(defrecord InOrderRecognizer [seen remainder status callback]
  Recognizer
  (handle-signal
   [this probe]
   {:pre [(not-ended? status)]}
   this)) ; TODO

(defrecord AllRecognizer [targets status callback]
  Recognizer
  (handle-signal
   [this probe]
   {:pre [(not-ended? status)]}
   this)) ; TODO

(defrecord OneOfRecognizer [target status callback]
  Recognizer
  (handle-signal
   [this probe]
   {:pre [(not-ended? status)]}
   this)) ; TODO

(defrecord WithinRecognizer [target status callback]
  Recognizer
  (handle-signal
   [this probe]
   {:pre [(not-ended? status)]}
   this)) ; TODO

(defrecord WithoutRecognizer [target status callback]
  Recognizer
  (handle-signal
   [this probe]
   {:pre [(not-ended? status)]}
   this)) ; TODO


(defprotocol Pattern
  (make-recognizer [this callback] "Make recognizer for pattern."))

(defn recognizer [pattern]
  "Creates a recognizer without a callback.
   This function aids mapping patterns to recognizers.

  This is intended for use when building recognizers within
  nested patterns. Within a nest pattern, only the callback
  used is from the outtermost recognizer."
  (make-recognizer pattern nil))

(defrecord BasePattern [element contravened?]
  Pattern
  (make-recognizer [this callback] (BaseRecognizer. element empty-status callback)))

(defn base-pattern
  ([element] (base-pattern element false))
  ([element contravened?]
     {:pre [(instance? Signal element)]}
     (BasePattern. element contravened?)))

(defn contravene
  "Build a contravened signal pattern. To use simply wrap a signal
  within a pattern with this call, e.g.
  (contravene (make-signal ::Foo {:a 1 :x 300}))."
  [element] (base-pattern element true))

(defn to-pattern
  "Ensures element is a Pattern. Passes element back if it is a Pattern.
  If element is a Signal, it is wrapped in a BasePattern."
  [element]
  (cond (satisfies? Pattern element) element
        (instance? Signal element) (base-pattern element)
        true (throw (IllegalArgumentException. (str "Element is neither a signal nor pattern: " element)))))

(defn to-patterns
  "Transforms a sequence of patterns and signals to all patterns.
  Takes a sequence of elements that can be patterns or signals
  and wraps all signals with BasePattern."
  [elements]
  (map to-pattern elements))

(defrecord OnePattern [pattern]
  Pattern
  (make-recognizer [this callback] (OneRecognizer. (make-recognizer pattern nil) empty-status callback)))

(defn one-pattern
  [element]
  (OnePattern. (to-pattern element)))

(defrecord InOrderPattern [patterns]
  Pattern
  (make-recognizer [this callback]
                   (InOrderRecognizer.
                    '()
                    (map make-recognizer patterns)
                    empty-status
                    callback)))

(defn in-order-pattern
  [& elements]
  (InOrderPattern. (to-patterns elements)))

(defrecord AllPattern [patterns]
  Pattern
  (make-recognizer [this callback] (AllRecognizer. patterns empty-status callback)))

(defn all-pattern
  [& elements]
  (AllPattern. (to-patterns elements)))

(defrecord OneOfPattern [patterns]
  Pattern
  (make-recognizer [this callback] (OneOfRecognizer. patterns empty-status callback)))

(defn one-of-pattern
  [& elements]
  (OneOfPattern. (to-patterns elements)))

(defrecord WithinPattern [pattern duration]
  Pattern
  (make-recognizer [this callback] (WithinRecognizer. pattern empty-status callback)))

(defn within-pattern
  [element duration]
  (WithinPattern. (to-pattern element) duration))

(defrecord WithoutPattern [pattern start finish]
  Pattern
  (make-recognizer [this callback] (WithoutRecognizer. pattern empty-status callback)))

(defn without-pattern
  [element start finish]
  (WithoutPattern. (to-pattern element) start finish))


;; tests / samples...

(defn safing
  "Build a safing signal."
  ([system status] (safing system status nil nil))
  ([system status start] (safing system status start start))
  ([system status start finish]
     (make-signal ::Safing {:system system :status status} start finish)))

