(ns rule-engine.test.cera
  (:use [rule-engine.cera] :reload)
  (:use [rule-engine.utils])
  (:use [clojure.test]))

(deftest status-test
  (let [e empty-status
        e1 (make-status nil (Date.) (Date.))
        a (make-status :active)
        a1 (make-status :active (Date.) (Date.))
        i (make-status :ignore)
        i1 (make-status :ignore (Date.) (Date.))
        c (make-status :complete)
        c1 (make-status :complete (Date.) (Date.))
        f (make-status :futile)
        f1 (make-status :futile (Date.) (Date.))]
    (is (active? a))
    (is (active? a1))
    (is (not (active? c)))
    (is (not (active? c1)))
    (is (not (active? i)))
    (is (not (active? i1)))
    (is (not (active? e)))
    (is (not (active? e1)))
    (is (not (active? f)))
    (is (not (active? f1)))
    (is (complete? c))
    (is (complete? c1))
    (is (not (complete? a)))
    (is (not (complete? a1)))
    (is (not (complete? i)))
    (is (not (complete? i1)))
    (is (not (complete? e)))
    (is (not (complete? e1)))
    (is (not (complete? f)))
    (is (not (complete? f1)))
    (is (ignore? i))
    (is (ignore? i1))
    (is (not (ignore? a)))
    (is (not (ignore? a1)))
    (is (not (ignore? c)))
    (is (not (ignore? c1)))
    (is (not (ignore? e)))
    (is (not (ignore? e1)))
    (is (not (ignore? f)))
    (is (not (ignore? f1)))
    (is (futile? f))
    (is (futile? f1))
    (is (not (futile? a)))
    (is (not (futile? a1)))
    (is (not (futile? i)))
    (is (not (futile? i1)))
    (is (not (futile? c)))
    (is (not (futile? c1)))
    (is (not (futile? e)))
    (is (not (futile? e1)))
    (is (not-ended? e))
    (is (not-ended? e1))
    (is (not-ended? a))
    (is (not-ended? a1))
    (is (not-ended? i))
    (is (not-ended? i1))
    (is (not (not-ended? c)))
    (is (not (not-ended? c1)))
    (is (not (not-ended? f)))
    (is (not (not-ended? f1)))))

(deftest signal-test
  (let [sig-1 (safing :mach1 :on)
        sig-1a (safing :mach1 :on (Date.) (Date.))
        sig-2 (safing :mach2 :on)]
    (is (signal-eq? sig-1 sig-1a) "Signals with only date differenes should be equivalent")
    (is (not (signal-eq? sig-1 sig-2)) "Signals with different data should not be equivalent")))

(deftest base-pattern-test
  (let [sig (safing :mach1 :on)
        pat (base-pattern sig)]
    (is (= (:data sig) {:system :mach1 :status :on}))
    (is (satisfies? rule-engine.cera/Pattern pat))
    (is (= (:data (:element pat)) {:system :mach1 :status :on}))
    (is (not (:contravened? pat)))))

(deftest to-pattern-test
  (let [pat (to-pattern (safing :mach1 :on))]
    (is (satisfies? rule-engine.cera/Pattern pat))
    (is (= (:data (:element pat)) {:system :mach1 :status :on}))
    (is (not (:contravened? pat)))))

(deftest make-recognizer-test
  (let [sig (safing :mach1 :on)
        pat (to-pattern sig)
        r1 (recognizer pat)  ; single-argument, non-polymorphic call that calls make-recognizer
        r2 (make-recognizer pat nil)] ; polymorphic make-recognizer
    (is (= sig (:target r1)))
    (is (nil? (:callback r1)))
    (is (= r1 r2) "Either mechanism for creation should create equivalent recognizer")))

(deftest base-recognizer-test
  (let [r (make-recognizer (to-pattern (safing :mach1 :on)) nil)]
    (is (= :ignore
           (:value (:status (handle-signal r (safing :mach2 :on (Date.) (Date.)))))))
    (is (= :complete
           (:value (:status (handle-signal r (safing :mach1 :on (Date.) (Date.)))))))))

(deftest out-of-state-base-recognizer-test
  (let [r1 (make-recognizer (to-pattern (safing :mach1 :on)) nil)
        r2 (handle-signal r1 (safing :mach1 :on (Date.) (Date.)))]
    (is (not-ended? (:status r1)))
    (is (complete? (:status r2)))
    (is (not (not-ended? (:status r2))))
    (is (thrown? AssertionError (handle-signal r2 (safing :mach2 :on (Date.) (Date.)))) "further signals should fail")))

(deftest base-recognizer-recognized-test
  (let [s (safing :mach1 :on)
        r1 (make-recognizer (to-pattern s) nil)
        probe (safing :mach1 :on (Date.) (Date.))
        r2 (handle-signal r1 probe)]
    (is (not-ended? (:status r1)))
    (is (complete? (:status r2)))
    (is (identical? probe (recognized r2)) "The recognized signal should be the probe signal (identical, not merely =)")))

(deftest one-recognizer-test
  (let [s (safing :mach1 :on)
        r1 (make-recognizer (one-pattern s) nil)
        probe1 (safing :mach1 :off (Date.) (Date.))
        probe2 (safing :mach1 :on (Date.) (Date.))
        r2 (handle-signal r1 probe1)
        r3 (handle-signal r2 probe2)]
    (is (ignore? (:status r2)))
    (is (complete? (:status r3)))
    (is (identical? probe2 (recognized r3)))))

(deftest in-order-test
  (let [r1 (recognizer (in-order-pattern (safing :mach1 :on) (safing :mach2 :on)))
        s1a (safing :mach1 :on (date 1) (date 2))
        s2a (safing :mach2 :on (date 3) (date 4))
        s3a (safing :mach2 :on (date 5) (date 6))
        r2a (handle-signal r1 s1a)
        r3a (handle-signal r2a s2a)

        s1b (safing :mach2 :on (date 1) (date 2))
        s2b (safing :mach1 :on (date 3) (date 4))
        s3b (safing :mach3 :on (date 5) (date 6))
        s4b (safing :mach2 :on (date 7) (date 8))
        r2b (handle-signal r1 s1b) 
        r3b (handle-signal r2b s2b) 
        r4b (handle-signal r3b s3b) 
        r5b (handle-signal r3b s4b)]
    (is (not-ended? (:status r1)))
    (is (not-ended? (:status r2a)))
    (is (active? (:status r2a)))
    (is (complete? (:status r3a)))
    (is (not (not-ended? (:status r3a))))
    (is (= [s1a s2a] (recognized r3a)))
    (is (= (:start s1a) (:start (:status r3a))))
    (is (= (:finish s2a) (:finish (:status r3a))))
    (is (thrown? AssertionError (handle-signal r3a s3a)))

    (is (ignore? (:status r2b)))
    (is (active? (:status r3b)))
    (is (ignore? (:status r4b)))
    (is (complete? (:status r5b)))
    (is (= [s2b s4b] (recognized r5b)))
    (is (= (:start s2b) (:start (:status r5b))))
    (is (= (:finish s4b) (:finish (:status r5b))))))

(deftest in-order-contra-test
  (let [r1 (recognizer (in-order-pattern (safing :mach1 :on) (contravene (safing :mach2 :on)) (safing :mach3 :on)))
        s1 (safing :mach1 :on (date 1) (date 2))
        s2 (safing :mach2 :on (date 3) (date 4))
        s3 (safing :mach2 :on (date 5) (date 6))
        r2 (handle-signal r1 s1)
        r3 (handle-signal r2 s2)
        r4 (handle-signal r3 s3)
        ]
    (is (active? (:status r2)))
    (is (active? (:status r3)))
    (is (futile? (:status r4)))
    (is (thrown? AssertionError (handle-signal r4 s3)))))
