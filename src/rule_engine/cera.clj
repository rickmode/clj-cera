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
(has a status value of :complete). In other state
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
       (if (ignore? status)
         this       ; still ignoring signals - no need to change state
         (assoc this :status ignore-status))))) ; times will be set on complete
  (recognized [this] seen)
  Contravenable
  (contravened? [this probe] (contravened? target probe)))

(defn base [element] (BaseRecognizer. element nil empty-status))


(defn to-recognizer
  "Ensures element is a recognizer. Wraps non-recognizers in a base recognizer."
  [element] (if (satisfies? Recognizer element) element (base element)))

(defn to-recognizers [elements] (map to-recognizer elements))


(defrecord OneRecognizer [target status]
  Recognizer
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (let [new-target (handle-signal target probe)]
       (assoc this :target new-target :status (:status new-target)))))
  (recognized [this] (recognized target))
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
       (case
        (:value new-status)
        :complete (InOrderRecognizer.
                   (conj seen new-target)
                   next-remainder
                   (next-status (if next-remainder :active :complete)
                                status new-status probe))
        :active (InOrderRecognizer.
                 seen
                 (conj next-remainder new-target) ; replacing target with new
                 (next-status :active status new-status probe))
        :ignore (if (or (contravened? new-target probe)
                        (contravened? this probe))
                  (assoc this :status
                         (next-status :futile status new-status probe))
                  (if (ignore? status)
                    this                ; keep ignoring 
                    (assoc-in this [:status :value] :ignore)))
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
     ;; current remainders with probe. During each iteration
     ;; During iteration, targets is a copy of the initial remainder.
     ;; Updates to targets do not need to be reflected in the list
     ;; used for iteration, since we never revisit those items.
     ;; However new target states need to be updated in the
     ;; recognizer's seen and remainder.
     (let [new-recognizer
           (loop [recognizer (assoc-in this [:status :value] :ignore)
                  targets remainder]
             (if targets
               (let [st (:status recognizer)
                     target (first targets)
                     new-target (handle-signal target probe)
                     new-st (:status new-target)]
                 (case
                  (:value new-st)
                  ;; if this is the last remainder, this recognizer is complete
                  ;; otherwise it is still active
                  :complete
                  (let [rmdr (:remainder recognizer)
                        ;; equiv. to (= 1 (count rmdr))
                        last-remainder? (and (first rmdr) (not (next rmdr)))] 
                    (if last-remainder?
                      ;; then update recognizer to complete and return
                      (AllRecognizer.
                       (conj (:seen recognizer) new-target)
                       '()
                       (next-status :complete st new-st probe))
                      ;; else new-target is complete, but this recognizer is still active,
                      ;; add new-target to seen, remove from remainder
                      (recur (AllRecognizer.
                              (conj (:seen recognizer) new-target)
                              (remove #(= % target) rmdr)
                              (next-status :active st new-st probe))
                             (next targets))))
                  ;; update recognizer with active status,
                  ;; and remainder with new-target, then iterate
                  :active
                  (recur (AllRecognizer.
                          (:seen recognizer)
                          (replace {target new-target} (:remainder recognizer))
                          (next-status :active st new-st probe))
                         (next targets))
                  ;; if probe contravenes target,
                  ;; update this recognizer to futile and return
                  :futile
                  (assoc recognizer
                    :status (next-status :futile st new-st probe))
                  ;; default - ignore
                  (recur recognizer (next targets)))) ; ignore: iterate with no state change
               recognizer))] ; else of if target
       ;; done looping - check for contravention
       (if (and (ignore? (:status new-recognizer)) (contravened? new-recognizer probe))
         (assoc new-recognizer :status (make-status :futile
                                                    (:start (:status new-recognizer))
                                                    (:finish probe)))
         new-recognizer))))
  (recognized [this] (reverse (map recognized seen)))
  Contravenable
  (contravened? [this probe]
                (let [contra? #(contravened? % probe)]
                  (or (some contra? seen) (some contra? remainder)))))

(defn all [& elements] (AllRecognizer. '()
                                       (to-recognizers elements)
                                       empty-status))


(defrecord OneOfRecognizer [seen remainder status]
  Recognizer
  (handle-signal
   [this probe]
   (do
     (assert (not-ended? status))
     (loop [recognizer this, targets remainder]
       (if targets
         (let [st (:status recognizer)
               target (first targets)             
               new-target (handle-signal target probe)
               new-st (:status new-target)]
           (case (:value new-st)
                 :complete (OneOfRecognizer.
                            new-target
                            (remove #(= % target) (:remainder recognizer))
                            new-st)
                 ;; all sub patterns must become futile for this level
                 ;; to become futile, so set state to futile and continue
                 ;; iteratoing
                 :futile (recur (OneOfRecognizer.
                                 nil
                                 (replace {target new-target} (:remainder recognizer))
                                 new-st)
                                (next targets))
                 (recur (OneOfRecognizer.
                         nil
                         (replace {target new-target} (:remainder recognizer))
                         (make-status :active nil (:finish probe)))
                        (next targets))))
         recognizer))))
  (recognized [this] (if seen (recognized seen) nil))
  Contravenable
  (contravened? [this probe] false))

(defn one-of [& elements] (OneOfRecognizer. nil
                                            (to-recognizers elements)
                                            empty-status))


;; TODO
(defrecord WithinRecognizer [target duration status]
  Recognizer
  (handle-signal [this probe]
                 this)
  (recognized [this] nil)
  Contravenable
  (contravened? [this probe] false))

(defn within [element duration] (WithinRecognizer. (to-recognizer element)
                                                   duration
                                                   empty-status))


;; TODO
(defrecord WithoutRecognizer [target start finish status]
  Recognizer
  (handle-signal [this probe]
                 this)
  (recognized [this] nil)
  Contravenable
  (contravened? [this probe] false))

(defn without [element start finish] (WithoutRecognizer. (to-recognizer element)
                                                         start
                                                         finish
                                                         empty-status))