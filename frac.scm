(define (cont-frac n d k)
  (define (cont-frac-iter c)
    (if (= c k)
        0
        (/ (n c) (+ (d c) (cont-frac-iter (+ c 1))))))
  (cont-frac-iter 1))

(define (golden-ratio k)
  (cont-frac (lambda (a) 1.0) (lambda (a) 1.0) k))



;The iteration version for the continuous function
(define (cont-frac-i n d k)
    (define (continuous result c)
      (if (> c 0)
          (continuous (/ (n (- c 1)) (+ (d (- c 1)) result))
                      (- c 1))
          result))
    ;(display k)
    (continuous (/ (n k) (d k)) k))

;(golden-ratio 100)
;why this is wrong


(cont-frac-rewrite (lambda (a) 1.0) (lambda (a) 1.0) 100)
(display "iteration version")
(cont-frac-i (lambda (a) 1.0) (lambda (a) 1.0) 100)
(display "recursive version")

;The algorithm to simulate natrual number e
(+ 2.0 (cont-frac-rewrite (lambda (y) 1.0) (lambda (i) (if (= (remainder (- i 2) 3) 0)
                (* 2 (/ (+ i 1) 3))
                1)) 100))

;Write a procedure to calculate tanx use the continuous function
;(define (tan-cf x k)
;  (/ x (+ 1.0 (cont-frac-rewrite  (lambda (y) (* -1 x x)) (lambda (y) (+ (* 2 y) 1)) k))))

(define (cont-frac-rewrite n d k)
  (define (iter i f)
    (if (= i 0)
        f
        (iter (- i 1)
              (/ (n i) (+ (d i) f)))
        ))
  (iter k (/ (n k) (d k))))
(define (square x) (* x x))
(define (tan-cf-answer x k)
  (cont-frac-rewrite (lambda (y) (if (= y 1) x (- (square x))))
                      (lambda (y) (- (* 2 y) 1))
                      k))

;The above answers are all incorrect although i dont konw why
(define (cont-frac-iter n d k)
   (define (frac-iter i result)
       (if (= i 0)
           result
           (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
   (frac-iter (- k 1) (/ (n k) (d k))))



(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
      x
      (- (square x))))


  (define (D i)
    (- (* 2 i) 1))
  (exact->inexact (cont-frac-iter N D k)))

;;After look for the answers I found that my cont-frac function is wrong.
