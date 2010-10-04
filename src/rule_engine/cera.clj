(ns rule-engine.cera
  (:use [rule-engine.utils :only [dbg pdbg]]))

;; General note: all times are in milliseconds.
;; So all start and finish values are numbers, not java.util.Dates.

(defrecord Status [value start finish])

;; status values can be any of the following
;; (TODO: so far this isn't used)
(def valid-statuses #{:complete :active :ignore :futile})

(defn make-status
  "Create a Status."
  ([]
     (make-status nil nil nil))
  ([value]
     (make-status value nil nil))
  ([value start finish]
     (Status. value start finish)))

;; empty statuses can always share a single constant since would all be identical
(def empty-status (make-status))

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


(defrecord Signal [tag data start finish])

(defn make-signal
  "Create a Signal."
  ([tag data]
     (make-signal tag data nil nil))
  ([tag data start finish]
     (Signal. tag data start finish)))

(defn signal-eq? [sig1 sig2]
  "Two signals are equal when their tag (type) and data are equal.
Start and finish times are ignored when matching signal patterns this way."
  (and (= (:tag sig1) (:tag sig2))
       (= (:data sig1) (:data sig2))))


(defprotocol Recognizer
  "A recognizer tracks the state of detecting a pattern."
  (transition [this probe]
              "Returns a recognizer in its next state based on the probe signal.")
  (recognized [this]
              "Returns all matched signals recognized.")
  (contravened? [this probe] "Is this recognizer contravened by probe."))


(defrecord PredicateRecognizer [predicate? contravention-predicate? seen status]
  Recognizer
  (transition [this probe] (do
                             (assert (not-ended? status))
                             (if (predicate? probe)
                               (assoc this
                                 :seen probe
                                 :status (make-status :complete (:start probe) (:finish probe)))
                               (assoc this
                                 :status (make-status :ignore nil (:finish probe))))))

  (recognized [this] seen)

  (contravened? [this probe] (and contravention-predicate?
                                  (contravention-predicate? probe))))

(defn pred
  "Create a genaral predicate matching recognizer.
The predicate? should a function with no side effects that takes a single Signal as an argument.
The optional contravention-predicate? should also be a side-effect free function
taking a single Signal argument."
  ([predicate?] (PredicateRecognizer. predicate? nil nil empty-status))
  ([predicate? contravention-predicate?] (PredicateRecognizer. predicate? contravention-predicate? nil empty-status)))

(defn base
  "Create a base recognizer that completes when signal matches probe via signal-eq?
and contravenes nothing."
  [signal] (pred (partial signal-eq? signal)))

(defn contravene-any
  "Create a base recognizer that also contravenes any other signal."
  [signal] (pred (partial signal-eq? signal) (constantly true)))

(defn contravene-same
  "Crease a base recognizer that also contravenes when this signal
is signal-eq? with the probe signal."
  [signal] (let [partial-eq? (partial signal-eq? signal)]
              (pred partial-eq? partial-eq?)))

(defn to-recognizer
  "Ensures element is a recognizer. Wraps non-recognizers in a base recognizer."
  [element] (if (satisfies? Recognizer element)
              element
              (base element)))

(defn to-recognizers [elements] (map to-recognizer elements))


(defrecord OneRecognizer [target status]
  Recognizer
  (transition
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (transition target probe)]
       (OneRecognizer. new-target (:status new-target)))))

  (recognized [this] (if target (recognized target) nil))

  (contravened? [this probe] false))

(defn one [element] (OneRecognizer. (to-recognizer element) empty-status))


(defn- next-status
  "Create a new status based on the start and finish times of current status,
new status from new target and probe."
  [value status new-status probe]
  (make-status value
               (or (:start status)      ; maintain start if set
                   (:start new-status)) ; otherwise use start from new target
               (:finish probe)))

(defrecord InOrderRecognizer [seen remainder status]
  Recognizer
  (transition
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (transition (first remainder) probe)
           new-status (:status new-target)
           next-remainder (next remainder)]
       (case (:value new-status)
             :complete
             (InOrderRecognizer. (conj seen new-target)
                                 next-remainder
                                 (next-status (if next-remainder
                                                :active
                                                :complete)
                                              status new-status probe))
             :active
             (InOrderRecognizer. seen
                                 (conj next-remainder new-target) ; updating target
                                 (next-status :active status new-status probe))
             :ignore
             (assoc this :status
                    (next-status (if (or (contravened? new-target probe)
                                         (contravened? this probe))
                                   :futile
                                   :ignore)
                                 status new-status probe))
             (assoc this :status
                    (next-status :futile status new-status probe))))))

  (recognized [this] (reverse (map recognized seen)))

  (contravened? [this probe] (some #(contravened? % probe) seen)))

(defn in-order [& elements] (InOrderRecognizer. '() (to-recognizers elements) empty-status))


(defrecord AllRecognizer [seen remainder status]
  Recognizer
  (transition
   [this probe]
   (do
     (assert (not-ended? status))
     ;; new-recognizer will be final result of iteratively signaling
     ;; current remainders with probe (the targets loop binding).
     (let [new-recognizer
           (loop [sn seen
                  rmdr remainder
                  st (assoc status :value :ignore)
                  targets remainder]
             (if targets
               (let [target (first targets)
                     new-target (transition target probe)
                     new-st (:status new-target)]
                 (case (:value new-st)
                       ;; if this is the last remainder, this recognizer is complete
                       ;; otherwise it is still active
                       ;; (and first not next is equivalent to count == 1)
                       :complete
                       (if (and (first rmdr) (not (next rmdr)))
                         (AllRecognizer. (conj sn new-target)
                                         '()
                                         (next-status :complete st new-st probe))
                         (recur (conj sn new-target)
                                (remove #(= % target) rmdr)
                                (next-status :active st new-st probe)
                                (next targets)))
                       :active
                       (recur sn
                              (replace {target new-target} rmdr)
                              (next-status :active st new-st probe)
                              (next targets))
                       :futile
                       (AllRecognizer. sn
                                       rmdr
                                       (next-status :futile st new-st probe))
                       ;; default - ignore
                       (recur sn rmdr st (next targets))))
               ;; else targets is nil - create final recognizer
               (AllRecognizer. sn rmdr st)))]
       ;; done looping - check for contravention
       (if (and (ignore? (:status new-recognizer))
                (contravened? new-recognizer probe))
         (assoc new-recognizer :status (make-status :futile (-> new-recognizer :status :start) (:finish probe)))
         new-recognizer))))

  (recognized [this] (reverse (map recognized seen)))

  (contravened? [this probe]
                (let [contra? #(contravened? % probe)]
                  (or (some contra? seen) (some contra? remainder)))))

(defn all [& elements] (AllRecognizer. '() (to-recognizers elements) empty-status))


(defrecord OneOfRecognizer [seen remainder status]
  Recognizer
  (transition
   [this probe]
   (do
     (assert (not-ended? status))
     (loop [sn seen, rmdr remainder, st status, targets remainder]
       (if targets
         (let [target (first targets)             
               new-target (transition target probe)
               new-st (:status new-target)]
           (case (:value new-st)
                 :complete
                 (OneOfRecognizer. new-target
                                   (remove #(= % target) rmdr)
                                   new-st)
                 ;; All sub patterns must become futile for this recognizer to
                 ;; be futile. So update status and remainder, then iterate.
                 :futile
                 (recur nil
                        (replace {target new-target} rmdr)
                        new-st
                        (next targets))
                 (recur nil
                        (replace {target new-target} rmdr)
                        (make-status :active nil (:finish probe))
                        (next targets))))
         (OneOfRecognizer. sn rmdr st)))))

  (recognized [this] (if seen (recognized seen) nil))

  (contravened? [this probe] false))

(defn one-of [& elements] (OneOfRecognizer. nil (to-recognizers elements) empty-status))


(defrecord WithinRecognizer [target duration status]
  Recognizer
  (transition
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (transition target probe)
           new-status (:status new-target)
           {:keys [start finish]} new-status]
       (WithinRecognizer. new-target
                          duration
                          (if (and (complete? new-status)
                                   (> (- finish start) duration))
                            (assoc new-status :value :futile)
                            new-status)))))

  (recognized [this] (if target (recognized target) nil))

  (contravened? [this probe] false))

(defn within
  ;; duration is in milliseconds
  [element duration] (WithinRecognizer. (to-recognizer element) duration empty-status))


(defrecord WithoutRecognizer [target start finish status]
  Recognizer
  (transition
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (transition target probe)
           new-status (:status new-target)]
       (WithoutRecognizer. new-target
                           start
                           finish
                           (if (and (complete? new-status)
                                    (not (or (> (:start new-status) finish)
                                             (< (:finish new-status) start))))
                             (assoc new-status :value :futile)
                             new-status)))))

  (recognized [this] (if target (recognized target) nil))

  (contravened? [this probe] false))

(defn without
  [element start finish] (WithoutRecognizer. (to-recognizer element)
                                             start
                                             finish
                                             empty-status))
