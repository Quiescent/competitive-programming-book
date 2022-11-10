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
  (let ((xs (iter (for i from 0 below n) (collecting i :result-type vector))))
    (iter
      (when (first-iteration-p)
        (collecting (copy-seq xs)))
      (for pivot = (iter
                     (for j from (1- n) downto 0)
                     (for x = (aref xs j))
                     (for y previous x initially most-negative-fixnum)
                     (finding j such-that (> y x))))
      (when (null pivot)
        (finish))
      (for x = (aref xs pivot))
      (for g-pivot = (iter
                       (for j from (1- n) downto 0)
                       (for y = (aref xs j))
                       (finding j such-that (> y x))))
      (rotatef (aref xs pivot)
               (aref xs g-pivot))
      (iter
        (for i from (1+ pivot) below n)
        (for j from (1- n) downto 0)
        (when (>= i j)
          (finish))
        (rotatef (aref xs i)
                 (aref xs j)))
      (collecting (copy-seq xs)))))

(defun permutations-recursive (n)
  "Compute the permutations of 1..N using consing."
  (let ((lookup (make-hash-table :test #'eq)))
    (labels ((recur (rem)
               (cond
                 ((= 0 rem) '(()))
                 (#1=(gethash rem lookup) #1#)
                 (t (setf #1#
                          (iter outer
                            (for i from 0 below n)
                            (when (> (logand (ash 1 i) rem) 0)
                              (let ((subresults (recur (logand (lognot (ash 1 i))
                                                               rem))))
                                (iter
                                  (for subresult in subresults)
                                  (in outer (collecting (cons i subresult))))))))))))
      (recur (1- (ash 1 n)))
      (gethash (1- (ash 1 n)) lookup))))

(defun print-permutations (n)
  "Generate all permutations of numbers 1..n.

Theoretical maximum N 64."
  (labels
      ((permutations-iter (rem acc)
         (cond
           ((= 0 rem) (print acc))
           (t (iter
                (for i from 0 below n)
                (when (> (logand (ash 1 i) rem) 0)
                  (permutations-iter (logand rem (lognot (ash 1 i)))
                                     (cons i acc))))))))
    (permutations-iter (1- (ash 1 n)) nil)))

;; ## 7. Generate all possible subsets of 1..20

(defun subsets (n)
  "Generate all subsets in 1..N."
  (labels
      ((recur (xs)
         (if (null xs)
             '(())
             (let ((subresults (recur (cdr xs))))
               (iter
                 (for subresult in subresults)
                 (collecting (cons (car xs) subresult))
                 (collecting subresult))))))
    (recur (iter (for i from 0 below n) (collecting i)))))

;; # Bit mask

;; Large
(make-array (list 100) :element-type 'bit :initial-element 1)
;; OR fixnum
#+nil
(x (1- (ash 1 100)))

;; # Chapter 3: 8 queuens

(defvar zero-to-7 #(0 1 2 3 4 5 6 7))

(defun place (board x y)
  "Attempt to place a queen on BOARD at X, Y."
  (when (not (some (lambda (other-x other-y)
                     (and other-y
                          (or (= (abs (- other-x x))
                                 (abs (- other-y y)))
                              (= other-x x)
                              (= other-y y))))
                   zero-to-7 board))
    (setf (aref board x) y)))

(defun eight-queens ()
  "Solve the eight queens problem."
  (let ((board (make-array (list 8) :initial-element nil)))
    (labels ((recur (x y)
               (cond
                 ((= x 8)           board)
                 ((= y 8)           (progn
                                      (let ((prev-y #1=(aref board (1- x))))
                                        (setf #1# nil)
                                        (recur (1- x) (1+ prev-y)))))
                 ((place board x y) (recur (1+ x) 0))
                 (t                 (recur x      (1+ y))))))
      (recur 0 0))))

;; # Chapter 3: Longest Increasing Subsequence

(defun longest-increasing-subsequence (xs)
  "Find the longest increasing subsequence in XS."
  (declare (type simple-array xs))
  (let ((cache (make-array (list (length xs))
                           :initial-element -1
                           :element-type 'fixnum)))
    (labels ((recur (i)
               (let ((cached #1=(aref cache i)))
                 (if (/= cached -1)
                     cached
                     (setf #1#
                           (if (= 0 i)
                               1
                               (do ((current (aref xs i))
                                    (j (1- i) (1- j))
                                    (l 1))
                                   ((< j 0) l)
                                 (let ((x (aref xs j)))
                                   (when (< x current)
                                     (setf l (max l (1+ (recur j)))))))))))))
      (do ((i 0 (1+ i))
           (l 1))
          ((>= i (length xs)) l)
        (setf l (max l (recur i)))))))

#+nil
(longest-increasing-subsequence #(-7 10 9 2 3 8 8 1))
