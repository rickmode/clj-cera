(ns rule-engine.test.cera
  (:use [rule-engine.cera] :reload)
  (:use [clojure.test])
  (:import [java.util Date]))

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
    (is (not (active? e)))
    (is (not (active? e1)))
    (is (not (active? c)))
    (is (not (active? c1)))
    (is (not (active? f)))
    (is (not (active? f1)))
    (is (complete? c))
    (is (complete? c1))
    (is (not (complete? e)))
    (is (not (complete? e1)))
    (is (not (complete? a)))
    (is (not (complete? a1)))
    (is (not-ended? e))
    (is (not-ended? e1))
    (is (not-ended? a))
    (is (not-ended? a1))
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

(deftest contrevened-test
  (let [contra-pat (contravene (safing :mach1 :on))]
    (is (satisfies? rule-engine.cera/Pattern contra-pat))
    (is (= (:data (:element contra-pat)) {:system :mach1 :status :on}))
    (is (:contravened? contra-pat))))

(deftest bad-base-pattern-test
  (is (thrown? AssertionError (base-pattern 1)))
  (is (thrown? AssertionError (base-pattern {:foo 1})))
  (is (thrown? IllegalArgumentException (to-pattern 1)))
  (is (thrown? IllegalArgumentException (to-pattern {:foo 1})))
  (is (thrown? AssertionError (contravene 1)))
  (is (thrown? AssertionError (contravene {:foo 1}))))

(deftest make-recognizer-test
  (let [sig (safing :mach1 :on)
        pat (to-pattern sig)
        r1 (recognizer pat)  ; single-argument, non-polymorphic call that calls make-recognizer
        r2 (make-recognizer pat nil)] ; polymorphic make-recognizer
    (is (= sig (:target r1)))
    (is (nil? (:callback r1)))
    (is (= r1 r2) "Either mechanism for creating recognizer should create equivalent recognizer")))

(deftest base-recognizer-test
  (let [r (make-recognizer (to-pattern (safing :mach1 :on)) nil)]
    (is (= :ignore
           (:value (:status (handle-signal r (safing :mach2 :on (Date.) (Date.)))))))
    (is (= :complete
           (:value (:status (handle-signal r (safing :mach1 :on (Date.) (Date.)))))))))