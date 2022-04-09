(defpackage competitive-programming-v2/tests/main
  (:use :cl
        :competitive-programming-v2
        :rove))
(in-package :competitive-programming-v2/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :competitive-programming-v2)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
