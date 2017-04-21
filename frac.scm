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
(define (cont-frac-rewrite n d k)
  (define (iter count f)
    (if (> count 0)
        (iter (- count 1)
              (/ (n (- count 1)) (+ (d (- count 1)) f)))
        f))
  (iter k (/ (n k) (d k))))

(cont-frac-rewrite (lambda (a) 1.0) (lambda (a) 1.0) 100)
(display "iteration version")
(cont-frac-i (lambda (a) 1.0) (lambda (a) 1.0) 100)
(display "recursive version")
