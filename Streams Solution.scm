; Ziyu Qiu (ziyuqiu@brandeis.edu) 2016-03-21
; PS4-Streams and Lazy Evaluation

;;;HERE IS THE GIVEN CODE
;; This is the code for Streams and Lazy Evaluation

(define-syntax cons-stream
 (syntax-rules ()
   ((cons-stream a b)
    (cons a (delay b)))))

(define head car)
(define (tail s) (force (cdr s)))
(define stream-car car)
(define stream-cdr tail)

(define the-empty-stream (delay '()))
(define (empty-stream? s)
 (and (not (pair? s))
      (null? (force s))))

(define (1+ x) (+ 1 x))
(define (write-line x)
 (display x)
 (newline))

(define (divisible? x y) (= (remainder x y) 0))

;; Useful stream utility functions

(define (stream-filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;  Mapping functions

(define (stream-map proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

;;  Iterating along a stream.

(define (stream-for-each proc stream)
  (if (empty-stream? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))

;;  Streams of numbers

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (stream-car s1) (stream-car s2))
                           (add-streams (stream-cdr s1) (stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;  Differs from book by not checking for empty streams
(define (interleave s1 s2)
  (cons-stream (stream-car s1)
               (interleave s2
                           (stream-cdr s1))))

(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((h1 (stream-car s1))
               (h2 (stream-car s2)))
           (cond ((< h1 h2) (cons-stream h1 (merge (stream-cdr s1) s2)))
                 ((> h1 h2) (cons-stream h2 (merge s1 (stream-cdr s2))))
                 (else (cons-stream h1 
                                    (merge (stream-cdr s1) 
                                           (stream-cdr s2)))))))))

;; This next procedure is to be used in forming streams of pairs,
;; once you have defined the procedure MERGE-WEIGHTED
(define (weighted-pairs s t pair-weight)
  (cons-stream (cons (stream-car s) (stream-car t))
               (merge-weighted
                  (stream-map (lambda (x) (cons (stream-car s) x))
                              (stream-cdr t))
                  (weighted-pairs (stream-cdr s) (stream-cdr t) pair-weight)
                  (lambda (p) (pair-weight (car p) (cdr p))))))

;; This procedure forms streams of weighted pairs, where pairs of the
;; same weight have been combined.  In order to use it, you must
;; define an appropriate procedure COMBINE-SAME-WEIGHTS
(define (same-weight-pairs s t pair-weight)
  (combine-same-weights (weighted-pairs s t pair-weight)
                        pair-weight))

(define print-stream
  (let ()
    (define (iter s)
      (if (empty-stream? s)
          (display "]")
          (begin (write (stream-car s))
                 (write " ")
                 (iter (stream-cdr s)))))
    (lambda (s)
      (write "")
      (iter s))))
;; You may wonder why PRINT-STREAM has been written in such an obscure
;; way, when
;; (define (print-stream s)
;;   (write "[")
;;   (stream-for-each (lambda (x) (write x) (write " ")) s)
;;   (write "]"))
;; would have the same effect.  If you think about the "actor model"
;; and tail recursion, however, you may begin to see why.

;;  For exercise 3.43
(define (show x)
  (write-line x)
  x)

(define (nth-stream n s)
  (if (= n 0)
      (stream-car s)
      (nth-stream (- n 1) (stream-cdr s))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (enumerate-interval (1+ low) high))))


;;;HERE IS MY CODE
;Problem 1.
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
;;;Verify that they work by using the print-stream procedure
;(print-stream integers)
; See results in pdf

;Show how to define the stream of all integers that are not divisible by either 2,
;3, or 5. (Use stream-filter.)

;stream of integers that are indivisible by 3
(define no-3 
  (stream-filter (lambda (x) (not (divisible? x 3)))
                 integers))
;stream of integers that are indivisible by either 2 or 3
(define no-2-3
  (stream-filter (lambda (x) (not (divisible? x 2)))
                 no-3))
;stream of integers that are indivisible by either 2, 3 or 5
(define no-2-3-5
  (stream-filter (lambda (x) (not (divisible? x 5)))
                 no-2-3))

;;;Problem 2.
;Test the procedure by interleaving the stream of integers that are not divisible
;by 7 with the stream of integers not divisible by 3.
(define no-7
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
;What expression did you evaluate to find this result?
;(print-stream (interleave no-7 no-3))
;What are the first few elements of the resulting stream?
;See print results in pdf file

;;;Problem 3.
(define alt (cons-stream 0 (interleave integers alt)))
;What can you say about the structure of this stream? Do you notice any regularities,
;and can you explain how they arise? (For example, which elements are equal to 0?
;Equal to 1? Equal to 2?)
;(print-stream alt)
; 0, 1, 2 appear with regularity
; For n>=1
; 0: 2*2^(n-1) - 1 = 2^n-1
; 1: 3*2^(n-1)-1
; 2: 5*2^(n-1)-1

;;;Problem 4.
;(define (pairs s t)
;     (cons-stream (cons (stream-car s) (stream-car t))
;                  (combine-in-some-way
;                     (stream-map (lambda (x) (cons (stream-car s) x))
;                                 (stream-cdr t))
;                     (pairs (stream-cdr s) (stream-cdr t)))))

;Define a procedure interleave-pairs that uses the general method for forming pairs
;as above, using interleave—as in problem 2 above—to combine the two streams. 
(define (interleave-pairs s t)
     (cons-stream (cons (stream-car s) (stream-car t))
                  (interleave
                     (stream-map (lambda (x) (cons (stream-car s) x))
                                 (stream-cdr t))
                     (interleave-pairs (stream-cdr s) (stream-cdr t)))))
;Examine the resulting stream
;(print-stream (interleave-pairs integers integers))
;See results in pdf. file

;Can you make any general comments about the order in which the pairs are placed into the stream? 
;The position of (i, j) is (j-i)*2^i + 2^(i-1)

;For example, about how many pairs precede the pair (1,100)? the pair (99,100)? the pair (100,100)?
;(99*2^1+2^0-1)=198 pairs precede (1, 100)
;(1*2^99+2^98-1)=3*2^98-1 pairs precede (99,100)
;(0+2^99-1)=2^99-1 pairs precede (100, 100)

;;;Problem 5. Write a procedure merge-weighted
;takes an additional argument weight of pair
;should not discard elements with duplicate weights
(define (merge-weighted s1 s2 pair-weight)
  (let ((h1 (stream-car s1))
        (h2 (stream-car s2))
        (w1 (pair-weight(stream-car s1)))
        (w2 (pair-weight(stream-car s2))))
    (if (>= w1 w2)
        (cons-stream h2 (merge-weighted s1 (stream-cdr s2) pair-weight))
        (cons-stream h1 (merge-weighted (stream-cdr s1) s2 pair-weight)))))

;;;Problem 6. Use weighted-pairs and merge-weighted to define the streams:
;(a) all pairs of positive integers (i, j) ordered according to the sum i + j
(define weight1
  (lambda (x y) (+ x y)))
(define pairs1 (weighted-pairs integers integers weight1))
;Expression:
;(print-stream pairs1)
;See results in pdf. file

;(b) all pairs of positive integers (i, j) ordered according to the sum i^3 + j^3.
(define weight2
  (lambda (x y) (+ (* x x x) (* y y y))))
(define pairs2 (weighted-pairs integers integers weight2))
;Expression:
;(print-stream pairs2)
;See results in pdf. file

;(c) all pairs of positive integers (i,j), where neither i nor j is divisible by 2, 3, or 5,
;ordered according to the sum 2i + 3j + 5ij.
(define weight3
  (lambda (x y) (+ (* 2 x) (* 3 y) (* 5 x y))))
(define pairs3 (weighted-pairs no-2-3-5 no-2-3-5 weight3))
;Expression:
;(print-stream pairs3)
;See results in pdf. file

;;;Problem 7. Implement the procedure combine-same-weights as described above.
(define (stream-cadr p) (stream-car (stream-cdr p)))
(define (stream-cddr p) (stream-cdr (stream-cdr p)))
(define (combine-same-weights s pair-weight)
  (define current-weight (pair-weight (car (stream-car s)) (cdr (stream-car s))))
  (define (combine p)
     (if (= current-weight (pair-weight (car (stream-cadr p)) (cdr (stream-cadr p))))
         (combine (cons-stream (cons (stream-cadr p) (stream-car p)) (stream-cddr p)))
         p))
  (let ((group (combine (cons-stream '() s))))
    (cons-stream (cons current-weight (stream-car group))
              (combine-same-weights (stream-cdr group) pair-weight))))
;(print-stream (combine-same-weights pairs1 +))

;;; Problem 8. Use this code to generate the Ramanujan numbers by first generating
;and filtering the result for lists that contain more than one pair. What are the
;first five Ramanujan numbers?
(define (cube x) (* x x x))
(define cube-sum-numbers
  (same-weight-pairs integers
                     integers 
                     (lambda (i j) (+ (cube i) (cube j)))))
(define more-than-1
  (lambda(x)(> (length (cdr x)) 1)))
(define Ramanujan-numbers
  (stream-filter more-than-1 cube-sum-numbers))
;(print-stream Ramanujan-numbers)
;The first five Ramanujan numbers are 1729, 4104, 13832, 20683, 32832

;;;Problem 9. In a similar way to problem 8, generate streams of
;(a) All numbers that can be written as the sum of two squares in three different
;ways (showing how they can be so written). What are the five smallest such numbers?
(define (square x) (* x x))
(define square-sum-numbers 
  (same-weight-pairs integers
                     integers 
                     (lambda (x y) (+ (square x) (square y)))))
(define 3-way
  (lambda(x)(> (length (cdr x)) 2)))
(define 3way-squaresum-numbers
  (stream-filter 3-way square-sum-numbers))
;Expression
;(print-stream 3way-squaresum-numbers)
;Five smallest number: 325, 425, 650, 725, 845

;(b) All numbers that can be written, in two different ways, in the form i^3 + j^2
;where i is an odd positive integer and j is an even positive integer. (Also give,
;for each number, the appropriate values for i and j.) What are the five smallest
;such numbers?
(define odd-pos-int 
  (stream-filter (lambda (x) (not (divisible? x 2)))
                                               integers))
(define even-pos-int 
  (stream-filter (lambda (x) (divisible? x 2))
                                               integers))
(define cube-square 
  (same-weight-pairs odd-pos-int
                     even-pos-int
                     (lambda (i j) (+ (cube i) (square j)))))

(define 2way-cubesquare-numbers
  (stream-filter more-than-1 cube-square))
;Expression
;(print-stream 2way-cubesquare-numbers)
;Five smallest number: 1025, 4901, 6209, 6427, 6849
