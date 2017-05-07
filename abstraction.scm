(define (square x)
  (* x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

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

(define (multi a b c)
  (newton-method (cubic a b c)))

;Exercises 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

;Exercises 1.43
(define (repeat f n)
  ;(display n)
  (if (= n 1);why the counter should be 1 instead of 0
    f
    (compose f (repeat f (- n 1)))))
;Exercises 1.44
(define (smooth f n)
  (define dx 0.00001)
  (define g (lambda (x) (/ (+ (f (- x dx))
                              (f x)
                              (f (+ x dx))) 3)))
  (repeat g n))

(define (fixed-point-transform g transform guess)
  (fix-point (transform g) guess))

(define (^ a n)
  ((repeat (lambda (x) (* a x)) n) 1))

;Exercises 1.45
(define (nth-a a n k)
  (define g (lambda (x) (/ a (^ x (- n 1)))))
  (fix-point ((repeat average-damp k) g) 1.1))

;Exercises 1.46
;(define (iterative-improve p1 p2)
;  (display (lambda (x y) (p1 x y)))
;  (if ((lambda (x y) (p1 x y))) ;x is the initial guess, y is the aim value
;      (lambda (x y) x)
;      ((iterative-improve p1 p2) (lambda (x y) (p2 x y)) (lambda (x y) y))))

;;for sqrt
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (iterative-improve p1 p2)
  (define (iter guess)
    (if (p1 guess)
        guess
        (iter (p2 guess))))
  iter)

(define (square-root x)
  ((iterative-improve (lambda (guess) (good-enough? guess x))
                      (lambda (guess) (improve guess x))) 1.0))

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 0.0001))

(define (fix-point-new f)
  ((iterative-improve (lambda (guess) (close-enough? (f guess) guess))
                      (lambda (guess) (f guess))) 1.0))
