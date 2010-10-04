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


(defprotocol Contravenable
  (contravened? [this other] "Returns true if this is contravened by other."))

(defn signal-eq? [sig1 sig2]
  "Two signals are equal when their tag (type) and data are equal.
Contravention and the start/finish times are ignored as these are
not meant for patterns."
  (and (= (:tag sig1) (:tag sig2))
       (= (:data sig1) (:data sig2))))

(defrecord Signal [tag data start finish]
  Contravenable
  (contravened? [this other] false))

(defn make-signal
  "Create a Signal."
  ([tag data]
     (make-signal tag data nil nil))
  ([tag data start finish]
     (Signal. tag data start finish)))

;; Contravenable signals only happen in patterns
;; so start and finish aren't needed
;; ContraventionSignal contravenes everything
(defrecord ContraventionSignal [tag data]
  Contravenable
  (contravened? [this other] true))

(defn make-contravention-signal
  "Create a ContraventionSignal. This contravenes
any other signal."
  [tag data]
  (ContraventionSignal. tag data))

(defn contravene-any
  "Creates a ContraventionSignal from another signal.
This contravenes any other signal."
     [{:keys [tag data]}]
     (ContraventionSignal. tag data))

;; Contravenable signals only happen in patterns
;; so start and finish aren't needed
(defrecord SameContraventionSignal [tag data]
  Contravenable
  (contravened? [this other] (signal-eq? this other)))

(defn make-same-contravention-signal
  "Create a SameContraventionSignal. This contravenes when
this signal is signal-eq? with the probe signal."
  [tag data]
  (SameContraventionSignal. tag data))

(defn contravene-same
  "Creates a SameContraventionSignal from another signal.
This contravenes when this signal is signal-eq? with the probe
signal."
     [{:keys [tag data]}]
     (SameContraventionSignal. tag data))


(defprotocol Recognizer
  "A recognizer tracks the state of detecting a pattern."
  (handle-signal [this probe]
                 "Returns a recognizer in its next state based on the probe signal.")
  (recognized [this]
              "Returns all matched signals recognized.
This is meant to be called after a recognizer completes
(has a status value of :complete). In other states
the returned signals will be implementation dependent.
The returned value can be a single signal or a list of signals."))


(defrecord BaseRecognizer [target seen status]
  Recognizer
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (if (signal-eq? target probe)
       (assoc this
         :status (make-status :complete (:start probe) (:finish probe))
         :seen probe)
       (assoc this
         :status (make-status :ignore nil (:finish probe))))))
  (recognized [this] seen)
  Contravenable
  (contravened? [this probe] (contravened? target probe)))

(defn base [element] (BaseRecognizer. element nil empty-status))


(defn to-recognizer
  "Ensures element is a recognizer. Wraps non-recognizers in a base recognizer."
  [element] (if (satisfies? Recognizer element)
              element
              (base element)))

(defn to-recognizers [elements] (map to-recognizer elements))


(defrecord OneRecognizer [target status]
  Recognizer
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (handle-signal target probe)]
       (OneRecognizer. new-target (:status new-target)))))

  (recognized [this] (if target (recognized target) nil))

  Contravenable
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
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (handle-signal (first remainder) probe)
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

  Contravenable
  (contravened? [this probe] (some #(contravened? % probe) seen)))

(defn in-order [& elements] (InOrderRecognizer. '() (to-recognizers elements) empty-status))


(defrecord AllRecognizer [seen remainder status]
  Recognizer
  (handle-signal
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
                     new-target (handle-signal target probe)
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

  Contravenable
  (contravened? [this probe]
                (let [contra? #(contravened? % probe)]
                  (or (some contra? seen) (some contra? remainder)))))

(defn all [& elements] (AllRecognizer. '() (to-recognizers elements) empty-status))


(defrecord OneOfRecognizer [seen remainder status]
  Recognizer
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (loop [sn seen, rmdr remainder, st status, targets remainder]
       (if targets
         (let [target (first targets)             
               new-target (handle-signal target probe)
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

  Contravenable
  (contravened? [this probe] false))

(defn one-of [& elements] (OneOfRecognizer. nil (to-recognizers elements) empty-status))


(defrecord WithinRecognizer [target duration status]
  Recognizer
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (handle-signal target probe)
           new-status (:status new-target)
           {:keys [start finish]} new-status]
       (WithinRecognizer. new-target
                          duration
                          (if (and (complete? new-status)
                                   (> (- finish start) duration))
                            (assoc new-status :value :futile)
                            new-status)))))

  (recognized [this] (if target (recognized target) nil))

  Contravenable
  (contravened? [this probe] false))

(defn within
  ;; duration is in milliseconds
  [element duration] (WithinRecognizer. (to-recognizer element) duration empty-status))


(defrecord WithoutRecognizer [target start finish status]
  Recognizer
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (handle-signal target probe)
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

  Contravenable
  (contravened? [this probe] false))

(defn without
  [element start finish] (WithoutRecognizer. (to-recognizer element)
                                             start
                                             finish
                                             empty-status))
