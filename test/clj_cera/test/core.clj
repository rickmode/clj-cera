(ns clj-cera.test.core
  (:use [clj-cera.core] :reload)
  (:use [clj-cera.utils] :reload)
  (:use [clojure.test]))

(defn safing
  "Build a safing signal. (One of the CERA examples.)"
  ([system status] (safing system status nil nil))
  ([system status start finish]
     (make-signal ::Safing {:system system :status status} start finish)))

(defn a-sig
  "Create a signal with a single datum."
  ([datum] (a-sig datum nil nil))
  ([datum start finish] (make-signal ::A datum start finish)))


(deftest status-test
  (let [e empty-status
        e1 (make-status nil (date) (date))
        a (make-status :active)
        a1 (make-status :active (date) (date))
        i (make-status :ignore)
        i1 (make-status :ignore (date) (date))
        c (make-status :complete)
        c1 (make-status :complete (date) (date))
        f (make-status :futile)
        f1 (make-status :futile (date) (date))]
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
        sig-1a (safing :mach1 :on (date) (date))
        sig-2 (safing :mach2 :on)]
    (is (signal-eq? sig-1 sig-1a) "Signals with only date differenes should be equivalent")
    (is (not (signal-eq? sig-1 sig-2)) "Signals with different data should not be equivalent")))

(deftest base-recognizer-test
  (let [r (base (safing :mach1 :on))]
    (is (= :ignore
           (-> (transition r (safing :mach2 :on (date) (date)))
               :status :value)))
    (is (= :complete
           (-> (transition r (safing :mach1 :on (date) (date)))
               :status :value)))))

(deftest out-of-state-base-recognizer-test
  (let [r1 (base (safing :mach1 :on))
        r2 (transition r1 (safing :mach1 :on (date) (date)))]
    (is (not-ended? (:status r1)))
    (is (complete? (:status r2)))
    (is (not (not-ended? (:status r2))))
    (is (thrown?
         AssertionError
         (transition r2 (safing :mach2 :on (date) (date))))
        "further signals should fail")))

(deftest base-recognizer-recognized-test
  (let [s (safing :mach1 :on)
        r1 (base s)
        probe (safing :mach1 :on (date) (date))
        r2 (transition r1 probe)]
    (is (not-ended? (:status r1)))
    (is (complete? (:status r2)))
    (is (identical? probe (recognized r2)) "The recognized signal should be the probe signal (identical, not merely =)")))

(defn a-sig-range
  "Create a base recognizer matching a range of a-sig signals"
  [begin end] (base-match (fn [{:keys [tag data]}]
                            (and (= tag ::A)
                                 (>= data begin)
                                 (<= data end)))))

(deftest base-match-test
  (let [r1 (a-sig-range 10 20)
        r2 (transition r1 (a-sig 1 (date-sec 1) (date-sec 2)))
        r3 (transition r2 (a-sig 15 (date-sec 3) (date-sec 4)))]
    (is (ignore? (:status r2)))
    (is (complete? (:status r3)))))

(deftest base-match-in-order-test
  (let [r1 (in-order (a-sig 1) (a-sig-range 10 20))
        r2 (transition r1 (a-sig 15))
        r3 (transition r2 (a-sig 1))
        r4 (transition r3 (a-sig 2))
        r5 (transition r4 (a-sig 11))]
    (is (ignore? (:status r2)))
    (is (active? (:status r3)))
    (is (ignore? (:status r4)))
    (is (complete? (:status r5)))))

(deftest one-recognizer-test
  (let [s (safing :mach1 :on)
        r1 (one s)
        probe1 (safing :mach1 :off (date) (date))
        probe2 (safing :mach1 :on (date) (date))
        r2 (transition r1 probe1)
        r3 (transition r2 probe2)]
    (is (ignore? (:status r2)))
    (is (complete? (:status r3)))
    (is (identical? probe2 (recognized r3)))))

(deftest in-order-test
  (let [r1 (in-order (safing :mach1 :on) (safing :mach2 :on))
        s1a (safing :mach1 :on (date-sec 1) (date-sec 2))
        s2a (safing :mach2 :on (date-sec 3) (date-sec 4))
        s3a (safing :mach2 :on (date-sec 5) (date-sec 6))
        r2a (transition r1 s1a)
        r3a (transition r2a s2a)
        ;; second path
        s1b (safing :mach2 :on (date-sec 1) (date-sec 2))
        s2b (safing :mach1 :on (date-sec 3) (date-sec 4))
        s3b (safing :mach3 :on (date-sec 5) (date-sec 6))
        s4b (safing :mach2 :on (date-sec 7) (date-sec 8))
        r2b (transition r1 s1b) 
        r3b (transition r2b s2b) 
        r4b (transition r3b s3b) 
        r5b (transition r3b s4b)]
    (is (not-ended? (:status r1)))
    (is (not-ended? (:status r2a)))
    (is (active? (:status r2a)))
    (is (complete? (:status r3a)))
    (is (not (not-ended? (:status r3a))))
    (is (= [s1a s2a] (recognized r3a)))
    (is (= (:start s1a) (:start (:status r3a))))
    (is (= (:finish s2a) (:finish (:status r3a))))
    (is (thrown? AssertionError (transition r3a s3a)))
    ;; now check second path
    (is (ignore? (:status r2b)))
    (is (active? (:status r3b)))
    (is (ignore? (:status r4b)))
    (is (complete? (:status r5b)))
    (is (= [s2b s4b] (recognized r5b)))
    (is (= (:start s2b) (:start (:status r5b))))
    (is (= (:finish s4b) (:finish (:status r5b))))))

(deftest in-order-contra-test-1
  (let [r1 (in-order (safing :mach1 :on)
                     (contravene-any (safing :mach2 :on))
                     (safing :mach3 :on))
        s1 (safing :mach1 :on (date-sec 1) (date-sec 2))
        s2 (safing :mach2 :on (date-sec 3) (date-sec 4))
        s3 (safing :mach2 :on (date-sec 5) (date-sec 6))
        r2 (transition r1 s1)
        r3 (transition r2 s2)
        r4 (transition r3 s3)]
    (is (active? (:status r2)))
    (is (active? (:status r3)))
    (is (futile? (:status r4)))
    (is (thrown? AssertionError (transition r4 s3)))))

(deftest in-order-contra-test-2
  (let [r1 (in-order (contravene-any (a-sig 1))
                     (a-sig 2)
                     (a-sig 3))
        s1 (a-sig 2 (date-sec 1) (date-sec 2))
        r2 (transition r1 s1)]
    (is (futile? (:status r2)))
    (is (= (-> r2 :status :start) nil))
    (is (= (-> r2 :status :finish) (date-sec 2)))))

(deftest all-test-1
  (let [r1 (all (safing :mach1 :on))
        s1 (safing :mach2 :on (date-sec 1) (date-sec 2))
        s2 (safing :mach1 :on (date-sec 3) (date-sec 4))
        r2 (transition r1 s1)
        r3 (transition r2 s2)]
    (is (= (count (:remainder r1)) 1))
    (is (= (count (:seen r1)) 0))
    (is (nil? (-> r1 :status :value)))
    (is (ignore? (:status r2)))
    (is (nil? (-> r2 :status :start)))
    (is (nil? (-> r2 :status :finish)))
    (is (= (count (:seen r2)) 0))
    (is (complete? (:status r3)))
    (is (= (-> r3 :status :start) (date-sec 3)))
    (is (= (-> r3 :status :finish) (date-sec 4)))
    (is (= (count (:seen r3)) 1))
    (is (signal-eq? (first (recognized r3)) (safing :mach1 :on)))))

(deftest all-test-2
  (let [r1 (all (safing :mach1 :on)
                (safing :mach2 :on)
                (safing :mach3 :on))
        s1 (safing :mach1 :on (date-sec 1) (date-sec 2))
        s2 (safing :mach3 :on (date-sec 3) (date-sec 4))
        s3 (make-signal ::Whaa 0 (date-sec 5) (date-sec 6))
        s4 (safing :mach2 :on (date-sec 7) (date-sec 8))
        r2 (transition r1 s1)
        r3 (transition r2 s2)
        r4 (transition r3 s3)
        r5 (transition r4 s4)
        recog (recognized r5)
        tgts [(safing :mach1 :on) (safing :mach3 :on) (safing :mach2 :on)]]
    (is (= (count (:remainder r1)) 3))
    (is (= (count (:seen r1)) 0))
    (is (active? (:status r2)))
    (is (= (-> r2 :status :start) (date-sec 1)))
    (is (= (-> r2 :status :finish) (date-sec 2)))
    (is (active? (:status r3)))
    (is (= (-> r3 :status :start) (date-sec 1)))
    (is (= (-> r3 :status :finish) (date-sec 4)))
    (is (ignore? (:status r4)))
    (is (= (-> r4 :status :start) (date-sec 1)))
    (is (= (-> r4 :status :finish) (date-sec 4)))
    (is (complete? (:status r5)))
    (is (= (-> r5 :status :start) (date-sec 1)))
    (is (= (-> r5 :status :finish) (date-sec 8)))
    (is (= (count (:seen r5)) 3))
    (is (= (count recog) 3))
    (is (signal-eq? (first recog) (first tgts)))
    (is (signal-eq? (fnext recog) (fnext tgts)))
    (is (signal-eq? (first (nnext recog)) (first (nnext tgts))))))

(deftest all-test-3
  (let [foo (a-sig "foo")
        baa (a-sig "baa")
        bar (a-sig "bar")
        foobar (a-sig "foobar")
        r1 (all foo (in-order baa bar) foobar)
        r2 (transition r1 (a-sig "bar" (date-sec 1) (date-sec 2)))
        r3 (transition r2 (a-sig "baa" (date-sec 3) (date-sec 4)))
        r4 (transition r3 (a-sig "foobar" (date-sec 5) (date-sec 6)))
        r5 (transition r4 (a-sig "foobar" (date-sec 7) (date-sec 8)))
        r6 (transition r5 (a-sig "foo" (date-sec 9) (date-sec 10)))
        r7 (transition r6 (a-sig "bar" (date-sec 11) (date-sec 12)))]
    (is (ignore? (:status r2)))
    (is (active? (:status r3)))
    (is (active? (:status r4)))
    (is (ignore? (:status r5)))
    (is (active? (:status r6)))
    (is (complete? (:status r7)))))

(deftest all-contra-test-1
  (let [r1 (all (contravene-any (a-sig 1)) (a-sig 2) (a-sig 3))
        r2 (transition r1 (a-sig 1 (date-sec 1) (date-sec 2)))
        r3 (transition r2 (a-sig 4 (date-sec 3) (date-sec 4)))]
    (is (active? (:status r2)))
    (is (futile? (:status r3)))
    (is (= (date-sec 1) (-> r3 :status :start)))
    (is (= (date-sec 4) (-> r3 :status :finish)))))

(deftest all-contra-test-2
  (let [r1 (all (a-sig 1) (contravene-same (a-sig 2)) (a-sig 3))
        r2 (transition r1 (a-sig 3 (date-sec 1) (date-sec 2)))
        r3 (transition r2 (a-sig 4 (date-sec 3) (date-sec 4)))
        r4 (transition r3 (a-sig 2 (date-sec 5) (date-sec 6)))
        r5 (transition r4 (a-sig 2 (date-sec 7) (date-sec 8)))]
    (is (active? (:status r2)))
    (is (ignore? (:status r3)))
    (is (active? (:status r4)))
    (is (futile? (:status r5)))
    (is (= (date-sec 1) (-> r5 :status :start)))
    (is (= (date-sec 8) (-> r5 :status :finish)))))

(deftest one-of-test-1
  (let [r1 (one-of (a-sig 1) (a-sig 2) (a-sig 3))
        r2 (transition r1 (a-sig 4 (date-sec 1) (date-sec 2)))
        r3 (transition r2 (a-sig 2 (date-sec 3) (date-sec 4)))]
    (is (active? (:status r2)))
    (is (nil? (-> r2 :status :start)))
    (is (= (date-sec 2) (-> r2 :status :finish)))   
    (is (complete? (:status r3)))
    (is (= (date-sec 3) (-> r3 :status :start)))   
    (is (= (date-sec 4) (-> r3 :status :finish)))))

(deftest one-of-test-2
  (let [r1 (one-of (a-sig "foo")
                   (in-order (a-sig "baa")
                             (a-sig "bar"))
                   (all (a-sig 1)
                        (a-sig 2)
                        (a-sig "foobar")))
        r2 (transition r1 (a-sig 1 (date-sec 1) (date-sec 2)))
        r3 (transition r2 (a-sig "bar" (date-sec 3) (date-sec 4)))
        r4 (transition r3 (a-sig "baa" (date-sec 5) (date-sec 6)))
        r5 (transition r4 (a-sig "foobar" (date-sec 7) (date-sec 8)))
        r6 (transition r5 (a-sig "foobar" (date-sec 9) (date-sec 10)))
        r7 (transition r6 (a-sig 2 (date-sec 11) (date-sec 12)))]
    (is (active? (:status r2)))
    (is (nil? (-> r2 :status :start)))
    (is (= (date-sec 2) (-> r2 :status :finish)))   
    (is (active? (:status r3)))
    (is (nil? (-> r3 :status :start)))
    (is (= (date-sec 4) (-> r3 :status :finish)))   
    (is (active? (:status r4)))
    (is (nil? (-> r4 :status :start)))
    (is (= (date-sec 6) (-> r4 :status :finish)))   
    (is (active? (:status r5)))
    (is (nil? (-> r5 :status :start)))
    (is (= (date-sec 8) (-> r5 :status :finish)))   
    (is (active? (:status r6)))
    (is (nil? (-> r6 :status :start)))
    (is (= (date-sec 10) (-> r6 :status :finish)))   
    (is (complete? (:status r7)))
    (is (= (date-sec 1) (-> r7 :status :start)))   
    (is (= (date-sec 12) (-> r7 :status :finish)))))

(deftest within-test-1
  (let [r1 (within (a-sig 1) 5000)
        r2a (transition r1 (a-sig 1 (date-sec 1) (date-sec 5)))
        r2b (transition r1 (a-sig 2 (date-sec 2) (date-sec 6)))
        r3b (transition r2b (a-sig 1 (date-sec 1) (date-sec 10)))]
    (is (complete? (:status r2a)))
    (is (= (date-sec 1) (-> r2a :status :start)))
    (is (= (date-sec 5) (-> r2a :status :finish)))
    (is (ignore? (:status r2b)))
    (is (nil? (-> r2b :status :start)))
    (is (= (date-sec 6) (-> r2b :status :finish)))
    (is (futile? (:status r3b)))
    (is (= (date-sec 1) (-> r3b :status :start)))
    (is (= (date-sec 10) (-> r3b :status :finish)))))

(deftest within-test-2
  (let [r1 (within (in-order (a-sig "foo") (a-sig "bar")) 10000)
        r2 (transition r1 (a-sig "bar" (date-sec 1) (date-sec 10)))
        r3 (transition r2 (a-sig "foo" (date-sec 1) (date-sec 10)))
        r4 (transition r3 (a-sig "car" (date-sec 2) (date-sec 11)))
        r5 (transition r4 (a-sig "bar" (date-sec 3) (date-sec 20)))]
    (is (ignore? (:status r2)))
    (is (nil? (-> r2 :status :start)))
    (is (= (date-sec 10) (-> r2 :status :finish)))
    (is (active? (:status r3)))
    (is (= (date-sec 1) (-> r3 :status :start)))
    (is (= (date-sec 10) (-> r3 :status :finish)))
    (is (ignore? (:status r4)))
    (is (= (date-sec 1) (-> r4 :status :start)))
    (is (= (date-sec 11) (-> r4 :status :finish)))
    (is (futile? (:status r5)))
    (is (= (date-sec 1) (-> r5 :status :start)))
    (is (= (date-sec 20) (-> r5 :status :finish)))))

(deftest within-test-3
  (let [r1 (within (all (a-sig 1)
                        (a-sig 2)
                        (a-sig 3))
                   15000)
        r2 (transition r1 (a-sig 2 (date-sec 5) (date-sec 10)))
        r3 (transition r2 (a-sig 2 (date-sec 6) (date-sec 11)))
        r4 (transition r3 (a-sig 3 (date-sec 7) (date-sec 12)))
        r5 (transition r4 (a-sig 1 (date-sec 8) (date-sec 13)))]
    (is (active? (:status r2)))
    (is (= (date-sec 5) (-> r2 :status :start)))
    (is (= (date-sec 10) (-> r2 :status :finish)))
    (is (ignore? (:status r3)))
    (is (active? (:status r4)))
    (is (complete? (:status r5)))
    (is (= (date-sec 5) (-> r5 :status :start)))
    (is (= (date-sec 13) (-> r5 :status :finish)))))

(deftest without-test-1
  (let [r1 (without (a-sig 1) (date-sec 5) (date-sec 10))
        r2a (transition r1 (a-sig 1 (date-sec 1) (date-sec 4)))
        r2b (transition r1 (a-sig 2 (date-sec 2) (date-sec 6)))
        r3b (transition r2b (a-sig 1 (date-sec 1) (date-sec 5)))]
    (is (complete? (:status r2a)))
    (is (= (date-sec 1) (-> r2a :status :start)))
    (is (= (date-sec 4) (-> r2a :status :finish)))
    (is (ignore? (:status r2b)))
    (is (nil? (-> r2b :status :start)))
    (is (= (date-sec 6) (-> r2b :status :finish)))
    (is (futile? (:status r3b)))
    (is (= (date-sec 1) (-> r3b :status :start)))
    (is (= (date-sec 5) (-> r3b :status :finish)))))

(deftest without-test-2
  (let [r1 (without (within (in-order (a-sig "foo")
                                      (a-sig "bar"))
                            5000)
                    (date-sec 10) (date-sec 20))
        r2 (transition r1 (a-sig "bar" (date-sec 1) (date-sec 10)))
        r3 (transition r2 (a-sig "foo" (date-sec 1) (date-sec 10)))
        r4 (transition r3 (a-sig "car" (date-sec 2) (date-sec 11)))
        r5 (transition r4 (a-sig "bar" (date-sec 3) (date-sec 9)))]
    (is (ignore? (:status r2)))
    (is (nil? (-> r2 :status :start)))
    (is (= (date-sec 10) (-> r2 :status :finish)))
    (is (active? (:status r3)))
    (is (= (date-sec 1) (-> r3 :status :start)))
    (is (= (date-sec 10) (-> r3 :status :finish)))
    (is (ignore? (:status r4)))
    (is (= (date-sec 1) (-> r4 :status :start)))
    (is (= (date-sec 11) (-> r4 :status :finish)))
    (is (futile? (:status r5)))
    (is (= (date-sec 1) (-> r5 :status :start)))
    (is (= (date-sec 9) (-> r5 :status :finish)))))

(deftest without-test-3
  (let [r1 (without (all (a-sig 1)
                         (a-sig 2)
                         (a-sig 3))
                    (date-sec 15) (date-sec 100))
        r2 (transition r1 (a-sig 2 (date-sec 5) (date-sec 10)))
        r3 (transition r2 (a-sig 2 (date-sec 6) (date-sec 11)))
        r4 (transition r3 (a-sig 3 (date-sec 7) (date-sec 12)))
        r5 (transition r4 (a-sig 1 (date-sec 8) (date-sec 16)))]
    (is (active? (:status r2)))
    (is (= (date-sec 5) (-> r2 :status :start)))
    (is (= (date-sec 10) (-> r2 :status :finish)))
    (is (ignore? (:status r3)))
    (is (active? (:status r4)))
    (is (futile? (:status r5)))
    (is (= (date-sec 5) (-> r5 :status :start)))
    (is (= (date-sec 16) (-> r5 :status :finish)))))

