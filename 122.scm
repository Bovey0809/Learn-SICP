(define (smallest-divisor n)
  (find-divisor n 2))

(define (next divisor)
  (if (= divisor 2)
    3
    (+ divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n ) test-divisor)
	(else (find-divisor n (next test-divisor)))))
;if the test divisor can divide the n, then it is a divisor, else find another one.

(define (divides? a b) (= (remainder b a ) 0))

(define (prime? n)
  (fast-prime? n 100))

;(prime? 3)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
    (remainder (square (expmod base (/ exp 2) m)) m))
	(else
    (remainder (* base (expmod base (- exp 1) m)) m))))

(define (even? n)
  (= (remainder n 2) 0))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (time-prime-test n)
  (newline)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time)))
      #f)

(define (report-prime n elapsed-time)
  (display n)
  (display "***")
  (display elapsed-time)
  #t)

(if (time-prime-test 100 )
  (display "ture")
  (display "false"))

(define (search-for-primes k n)
    (if (> n 0)
        (siter k n)
        (display "Complete"))
    (define (siter k n )
      (if (even? k)
        (search-for-primes (+ k 2) n )
        (if (time-prime-test k)
            (search-for-primes (+ k 2) (- n 1 ))
            (search-for-primes (+ k 2) n )))))

(search-for-primes 10 3)
(display (time-prime-test 11))

(display (time-prime-test 13))
(display (time-prime-test 14))
(display (time-prime-test 17))
