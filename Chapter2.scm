(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))


(define k (cons 4 5))
(cdr k)

(define (make-integer a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-integer integer)
  (calculate-integer integer 2))

(define (cdr-integer integer)
  (calculate-integer integer 3))

(define (calculate-integer n m)
  (define (iter n count) ;; Do i have to add m together as paramenter
    (if (= 0 (remainder n m))
      (iter (/ n m) (+ count 1))
      count))
  (iter n 0))

;(car-integer (make-integer 4 5))
;(cdr-integer (make-integer 4 5))

;Exercises2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(add-1 zero)

(define one
  (lambda (f) (f x)))


;; Exercises2.7
(define make-interval cons)
(define upper-bound car)
(define lower-bound cdr)

;;Exercise 2.8
(define (sub-interval x y)
  (make-interval (max (- (upper-bound x) (lower-bound y))
                      (- (upper-bound y) (lower-bound x)))
                 (min (- (lower-bound x) (upper-bound y))
                      (- (lower-bound y) (upper-bound x)))))
;;Exercise 2.9
(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (sum-width interval1 interval2)
  (+ (width interval1) (width interval2)))

(define (div-interval interval1 interval2)
    (mul-interval interval1
                (make-interval (/ 1.0 (upper-bound interval2))
                               (/ 1.0 (lower-bound interval2)))))

 (define (mul-interval x y)
      (let ((p1 (* (lower-bound x) (lower-bound y)))
            (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (upper-bound y)))
            (p4 (* (upper-bound x) (lower-bound y))))
           (make-interval (min p1 p2 p3 p4)
           (max p1 p2 p3 p4))))

;(define (div-interval interval1 interval2)
;  (define (change interval)
;    (cond ((and (> (upper-bound interval) 0) (< (lower-bound interval) 0))
;           (make-interval (/ 1.0 (upper-bound interval))
;                          (/ 1.0 (lower-bound interval)))
;          ((and (> (lower-bound interval) 0) (< (upper-bound interval) 0))
;           (display "WRONG")))
;           (else interval)))
;  (mul-interval interval1
;                (change interval2)))


;; Exercise 2.12
(define (make-center-tolerance center tolerance)
  (make-interval (* center (+ 1 tolerance))
                 (* center (- 1 tolerance))))

(define (percent interval)
  (/ (- (upper-bound interval) (lower-bound interval)) (+ (upper-bound interval) (lower-bound interval))))

(define (center interval)
  (/ (+ (upper-bound interval) (lower-bound interval)) 2))

(define (add-interval interval1 interval2)
  (make-interval (+ (upper-bound interval1) (upper-bound interval2))
                 (+ (lower-bound interval1) (lower-bound interval2))))
(define (par1 interval1 interval2)
  (div-interval (mul-interval interval1 interval2)
     (add-interval interval1 interval2)))

(define (par2 interval1 interval2)
  (let ((one (make-interval 1 1)))
        (div-interval one
                      (add-interval (div-interval one interval1) (div-interval one interval2)))))

(define part1 (make-center-tolerance 5 0.01))
(define part2 (make-center-tolerance 1 0.01))

(define (print-interval interval)
  (newline)
  (display (lower-bound interval))
  (display ",")
  (display (upper-bound interval))
  (newline))

(print-interval (par1 part1 part2))
(print-interval (par2 part1 part2))
