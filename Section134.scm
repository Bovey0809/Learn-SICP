;Procedure as the return value
(define (average-damp f)
  (lambda (y) (average y (f y))))

(define (average v1 v2)
  (/ (+ v1 v2) 2))

(define (fix-point f guess)
  (define (enough? v1 v2)
    (< (abs (- v1 v2)) 0.0001))
  (if (enough? guess (f guess))
      guess
      (fix-point f (f guess))))

(define (sqrt x)
  (fix-point (average-damp (lambda (y) (/ x y))) 1.5))
;;;fix point needs two parameters one is the procedure type the other is double

;;Newton's method
(define (deriv f)
  (lambda (x) (/ (- (f (+ x 0.0001)) (f x)) 0.0001)))

(define (cube x) (* x x x))

((deriv cube) 5)

(define (transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method f)
  (fix-point (transform f) 1.0))

(define (sqrt-new x)
  (newton-method (lambda (y) (- (* y y) x))))
