(defpackage competitive-programming-v2
  (:use
   :cl
   :cl-ppcre
   :iter))
(in-package :competitive-programming-v2)

;; # Exercise 1.2.3:

;; ## 1. Convert between bases

(format nil "~d" "13")
(format nil "~x" (read-from-string "13"))
(format nil "~o" (read-from-string "13"))
(format nil "~b" (read-from-string "13"))
(format nil "~3r" (read-from-string "13"))

;; ## 2. Is an integer's in a list of 1M integers with < 20 comparisons

;; TODO (probably bit twiddling)

;; ## 3. day of week

(nth (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 8 4 2022)))
     '(monday tuesday wednesday thursday friday saturday sunday))

;; ## 4. Replace special words

(cl-ppcre:regex-replace-all "($|[^0-9a-z])([a-z][0-9]{2})([^0-9a-z]|^)"
                            "this a22 is y55 a test"
                            "\\1***\\3")

(cl-ppcre:regex-replace-all "(^|[^0-9a-z])([a-z][0-9]{2})([^0-9a-z]|$)"
                            "this a22, is y55 a test z77"
                            "\\1***\\3")

(cl-ppcre:regex-replace-all "(^|[^0-9a-z])([a-z][0-9]{2})([^0-9a-z]|$)"
                            "y82 this a22, is y55 a test z77"
                            "\\1***\\3")

;; ## 5. Read a double (LOL)

#+nil
(read)

;; ## 6. Generate all possible permutations of 1..10

(defun permutations (n)
  "Generate all permutations of numbers 1..n.

Theoretical maximum N 64."
  (labels
      ((permutations-iter (rem acc)
         (cond
           ((= 0 rem) (list acc))
           (t (iter
                (for i from 0 below 64)
                (when (> (logand (ash 1 i) rem) 0)
                  (appending
                   (permutations-iter (logand rem (lognot (ash 1 i)))
                                      (cons i acc)))))))))
    (permutations-iter (1- (ash 1 n)) nil)))
