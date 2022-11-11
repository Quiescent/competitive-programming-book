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

;; # Chapter 3: Max Sum Matrix

(defun max-sum-matrix (xss dim-x dim-y)
  "Compute the maximum sum of a submatrix in XSS with dimensions (DIM-X DIM-Y).."
  (destructuring-bind (x y) (array-dimensions xss)
    (do ((i 0 (1+ i)))
        ((>= i x))
      (do ((j 0 (1+ j)))
          ((>= j y))
        (incf (aref xss i j)
              (+ (if (> i 0) (aref xss (1- i) j)      0)
                 (if (> j 0) (aref xss i      (1- j)) 0)
                 (if (and (> i 0)
                          (> j 0))
                     (- 0 (aref xss (1- i) (1- j)))
                     0)))))
    (format t "xss: ~a~%" xss)
    (do ((i 0 (1+ i))
         (dx (1- dim-x))
         (max-sum most-negative-fixnum))
        ((>= i (- x dx)) max-sum)
      (format t "i: ~a~%" i)
      (do ((j 0 (1+ j))
           (dy (1- dim-y)))
          ((>= j (- y dy)))
        (format t "j: ~a~%" j)
        (let ((current-sum (+ (aref xss (+ i dx) (+ j dy))
                              (- 0 (if (> j 0) (aref xss (+ i dx) (1- j))   0))
                              (- 0 (if (> i 0) (aref xss (1- i)   (+ j dy)) 0))
                              (if (and (> i 0)
                                       (> j 0))
                                  (- 0 (aref xss (1- i) (1- j)))
                                  0))))
          (format t "current-sum: ~a~%" current-sum)
          (setf max-sum (max current-sum max-sum)))))))

#+nil
(let ((xss (make-array (list 5 5)
                       :initial-element 1)))
  (max-sum-matrix xss 2 2))

#+nil
(let ((xss (make-array (list 4 4)
                       :initial-contents '((0 -2 -7 0)
                                           (9 2 -6 2)
                                           (-4 1 -4 1)
                                           (-1 8 0 -2)))))
  (max-sum-matrix xss 2 2))

(defun max-sum-any-submatrix (xss)
  "Find the maximum sum of any sub matrix of any sized in "
  (destructuring-bind (x y) (array-dimensions xss)
    (do ((i 0 (1+ i)))
        ((>= i x))
      (do ((j 0 (1+ j)))
          ((>= j y))
        (incf (aref xss i j)
              (+ (if (> i 0) (aref xss (1- i) j)      0)
                 (if (> j 0) (aref xss i      (1- j)) 0)
                 (if (and (> i 0)
                          (> j 0))
                     (- 0 (aref xss (1- i) (1- j)))
                     0)))))
    (do ((i 0 (1+ i))
         (maximum most-negative-fixnum))
        ((>= i x) maximum)
      (do ((j 0 (1+ j)))
          ((>= j y))
        (do ((k i (1+ k)))
            ((>= k x))
          (do ((l j (1+ l)))
              ((>= l y))
            (let ((next (+ (aref xss k l)
                           (- 0 (if (> i 0) (aref xss (1- i) l)      0))
                           (- 0 (if (> j 0) (aref xss k      (1- j)) 0))
                           (if (and (> i 0)
                                    (> j 0))
                               (aref xss (1- i) (1- j))
                               0))))
              (setf maximum (max maximum next)))))))))

#+nil
(let ((xss (make-array (list 4 4)
                       :initial-contents '((0 -2 -7 0)
                                           (9 2 -6 2)
                                           (-4 1 -4 1)
                                           (-1 8 0 -2)))))
  (max-sum-any-submatrix xss))

;; # Chapter 3 Knapsack problem

(defun knapsack (values weights capacity)
  "Produce the maximum value that can fit into a knapsack.

You may place items that have corresponding indices from VALUES &
WEIGHTS into the bag, which has a total carrying CAPACITY."
  (let ((cache (make-array (list (length values) (1+ capacity))
                           :initial-element -1)))
    (labels ((recur (i remaining)
               (if (>= i (length values))
                   0
                   (let ((cached #1=(aref cache i remaining)))
                     (if (/= -1 cached)
                         cached
                         (setf #1#
                               (let ((current-value (aref values i))
                                     (current-weight (aref weights i)))
                                 (max (if (<= current-weight remaining)
                                          (+ current-value
                                             (recur (1+ i)
                                                    (- remaining current-weight)))
                                          0)
                                      (recur (1+ i) remaining)))))))))
      (recur 0 capacity))))

#+nil
(let ((values (make-array (list 3) :initial-contents '(100 70 50)))
      (weights (make-array (list 3) :initial-contents '(10 5 7)))
      (capacity 12))
  (knapsack values weights capacity))

;; # Chapter 3: Coin change

(defun min-coins (denominations value)
  "Produce the smallest number of coins of DENOMINATIONS to make VALUE."
  (let ((cache (make-array (list (length denominations) (1+ value))
                           :initial-element -1)))
    (labels ((recur (i remaining)
               (if (>= i (length denominations))
                   (if (= remaining 0) 0 most-positive-fixnum)
                   (let ((cached #1=(aref cache i remaining)))
                     (if (/= -1 cached)
                         cached
                         (setf #1#
                               (let ((current-value (aref denominations i)))
                                 (if (<= current-value remaining)
                                     (min (1+ (recur 0 (- remaining current-value)))
                                          (recur (1+ i) remaining))
                                     (recur (1+ i) remaining)))))))))
      (recur 0 value))))

#+nil
(min-coins #(1 5) 20)
