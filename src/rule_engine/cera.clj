(ns rule-engine.cera
  (:use [rule-engine.utils :only [dbg pdbg]]))

(comment
  (def-recognizer safing-complete
    (pattern
     '(all
       (safing (system foo) (status on))
       (safing (system bar) (status on))))
    (on-complete [st end]
                 (signal-event '(all-safed) st end))))

;; status values can be :complete :active :ignore :futile
(defrecord Status [value start finish])

(defn make-status
  "Create a Status."
  ([]
     (make-status nil nil nil))
  ([value]
     (make-status value nil nil))
  ([value start]
     (Status. value start nil))
  ([value start finish]
     (Status. value start finish)))

;; empty statuses can always share a single constant since would all be identical
(def empty-status (make-status))
;; BaseRecognizer's use of ignore status doesn't need times, so it can use a constant
(def ignore-status (make-status :ignore))

(defn active? [status]
  (= (:value status) :active))

(defn ignore? [status]
  (= (:value status) :ignore))

(defn complete? [status]
  (= (:value status) :complete))

(defn futile? [status]
  (= (:value status) :futile))

(defn not-ended? [status]
  (let [v (:value status)] (and (not= v :complete) (not= v :futile))))


(defprotocol Contravenable
  (contravened? [this other] "Returns true if this is contravened by other."))

(defn signal-eq? [sig1 sig2]
  "Two signals are equal when their tag (type) and data are equal.
  Contravention and the start/finish times are ignored as these are
  not meant for patterns."
  (and (= (:tag sig1) (:tag sig2))
       (= (:data sig1) (:data sig2))))

(defrecord SimpleSignal [tag data start finish]
  Contravenable
  (contravened? [this other] false))

;; Contravenable signals only happen in patterns
;; so start and finish aren't needed
(defrecord SimpleContraSignal [tag data]
  Contravenable
  (contravened? [this other] (signal-eq? this other)))

(defn make-signal
  "Create a SimpleSignal."
  ([tag data]
     (make-signal tag data nil nil))
  ([tag data start finish]
     (SimpleSignal. tag data start finish)))

(defn make-contra-signal
  "Create a SimpleContraSignal."
  [tag data]
  (SimpleContraSignal. tag data))

(defn contravene
     "Create a SimpleContraSignal from another signal."
     [{:keys [tag data]}]
     (SimpleContraSignal. tag data))

(defprotocol Pattern
  (make-recognizer [this callback]
                   "Make recognizer for pattern.
                   Callback should be nil or a function taking recognizer as an argument.
                   E.g.: (make-recognizer
                           (to-pattern (safing :mach1 :on))
                           (fn [recognizer] (println \"Completed recognizer: \" recognizer)))"))

(defprotocol Recognizer
  "A recognizer tracks the state of detecting a pattern."
  (handle-signal [this probe]
                 "Returns a recognizer in its next state based on the probe signal.")
  ; (contravened? [this probe]
  ; "Returns true if this recognizer is contravened by the probe signal.")
  (recognized [this]
              "Returns all matched signals recognized.
              This is meant to be called after a recognizer completes
              (has a status value of :complete). In other states
              the returned signals will be implementation dependent.

              The returned value can be a single signal or a list of signals."))

(defn recognizer
  "Creates a recognizer.

  This single arity version aids mapping patterns to recognizers and is
  used in the Pattern protocol make-recognizer implementations. (This
  works within a nested pattern, as only the topmost callback used.)"
  ([pattern] (make-recognizer pattern nil))
  ([pattern callback] (make-recognizer pattern callback)))


(defrecord BaseRecognizer [target seen status callback]
  Recognizer
  (handle-signal [this probe]
                 (do
                   (assert (not-ended? status))
                   (if (signal-eq? target probe)
                     (assoc this :status (make-status :complete (:start probe) (:finish probe))
                            :seen probe)
                     (if (ignore? status)
                       this ; still ignoring signals - no need to change state
                       (assoc this :status ignore-status))))) ; don't need times - we'll set both on complete
  (recognized [this] seen)
  Contravenable
  (contravened? [this probe] (contravened? target probe)))

(defrecord BasePattern [element]
  Pattern
  (make-recognizer [this callback] (BaseRecognizer. element nil empty-status callback)))

(defn base-pattern [element] (BasePattern. element))

(defn to-pattern
  "Ensures element is a Pattern. Passes element back if it is a Pattern,
  otherwise it is wrapped in a BasePattern."
  [element]
  (if (satisfies? Pattern element) element (BasePattern. element)))

(defn to-patterns
  "Transforms a sequence of patterns and signals to all patterns.
  Takes a sequence of elements that can be patterns or signals
  and wraps all signals with BasePattern."
  [elements] (map to-pattern elements))


(defrecord OneRecognizer [target status callback]
  Recognizer
  (handle-signal [this probe]
                 (do
                   (assert (not-ended? status))
                   (let [new-target (handle-signal target probe)]
                     (assoc this :target new-target :status (:status new-target)))))
  (recognized [this] (recognized target))
  Contravenable
  (contravened? [this probe] (contravened? target probe)))

(defrecord OnePattern [pattern]
  Pattern
  (make-recognizer [this callback] (OneRecognizer. (make-recognizer pattern nil) empty-status callback)))

(defn one-pattern [element] (OnePattern. (to-pattern element)))

(defn- next-status
  "Create a new status based on the start and finish times of current status, new status and probe."
  [value status new-status probe]
  (make-status value
               (or (:start status) (:start new-status)) ; maintain start if already set, otherwise pull it from new target
               (:finish probe)))

(defrecord InOrderRecognizer [seen remainder status callback]
  Recognizer
  (handle-signal [this probe]
                 (do
                   (assert (not-ended? status))
                   (let [new-target (handle-signal (first remainder) probe)
                         new-status (:status new-target)
                         next-remainder (next remainder)]
                     (case (:value new-status)
                           :complete (InOrderRecognizer. (conj seen new-target)
                                                         next-remainder
                                                         (next-status (if next-remainder :active :complete)
                                                                      status new-status probe)
                                                         callback)
                           :active (InOrderRecognizer. seen
                                                       (conj next-remainder new-target) ; replacing target with new
                                                       (assoc status :value :active)
                                                       callback)
                           :ignore (if (or (contravened? new-target probe) (contravened? this probe))
                                     (assoc this :status (next-status :futile status new-status probe))
                                     (if (ignore? status)
                                       this ; keep ignoring (no need to update state)
                                       (assoc-in this [:status :value] :ignore)))
                           (assoc this :status (next-status :futile status new-status probe))))))
  (recognized [this] (reverse (map recognized seen)))
  Contravenable
  (contravened? [this probe] (some #(contravened? % probe) (pdbg seen))))

(defrecord InOrderPattern [patterns]
  Pattern
  (make-recognizer [this callback] (InOrderRecognizer. '()
                                                       (map recognizer patterns)
                                                       empty-status
                                                       callback)))

(defn in-order-pattern
  [& elements]
  (InOrderPattern. (to-patterns elements)))


;; TODO
(defrecord AllRecognizer [targets status callback]
  Recognizer
  (handle-signal [this probe]
                 this)
  (recognized [this] nil)
  Contravenable
  (contravened? [this probe] false))

(defrecord AllPattern [patterns]
  Pattern
  (make-recognizer [this callback] (AllRecognizer. patterns empty-status callback)))

(defn all-pattern
  [& elements]
  (AllPattern. (to-patterns elements)))


;; TODO
(defrecord OneOfRecognizer [targets status callback]
  Recognizer
  (handle-signal [this probe]
                 this)
  (recognized [this] nil)
  Contravenable
  (contravened? [this probe] false))

(defrecord OneOfPattern [patterns]
  Pattern
  (make-recognizer [this callback] (OneOfRecognizer. patterns empty-status callback)))

(defn one-of-pattern
  [& elements]
  (OneOfPattern. (to-patterns elements)))


;; TODO
(defrecord WithinRecognizer [target status callback]
  Recognizer
  (handle-signal [this probe]
                 this)
  (recognized [this] nil)
  Contravenable
  (contravened? [this probe] false))

(defrecord WithinPattern [pattern duration]
  Pattern
  (make-recognizer [this callback] (WithinRecognizer. pattern empty-status callback)))

(defn within-pattern
  [element duration]
  (WithinPattern. (to-pattern element) duration))


;; TODO
(defrecord WithoutRecognizer [target status callback]
  Recognizer
  (handle-signal [this probe]
                 this)
  (recognized [this] nil)
  Contravenable
  (contravened? [this probe] false))

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
