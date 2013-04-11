(ns elo100.negamax-test
  (:use clojure.test
        minichess.core
        elo100.heuristic
        elo100.negamax))

(deftest negater-test
  (testing "Is sane"
    (is (= (* -1 (:alpha vanilla-pgs)) (:alpha (negater vanilla-pgs))))))

(deftest prune-test
  (testing "Is sane"
    (is (= false (prune? vanilla-pgs)))
    (is (= true (prune? {:alpha 3 :beta -5})))))

(deftest invert-ab-test
  (testing "is sane"
    (is (= -3333 (:beta (invert-ab {:alpha 3333 :beta -2222}))))))
