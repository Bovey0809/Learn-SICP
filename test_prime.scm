(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
    (define (next input)
        (if (= input 2)
            3
            (+ input 2)))
  (cond ((> (square test-divisor) n) n )
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))


(define (prime? n)
  (= (smallest-divisor n) n))

(define (square n)
    (* n n))

(define (divides? n test-divisor)
    (if (= (remainder n test-divisor) 0)
        #t
        #f))

(define (expmod base exp m)
  (cond ((= exp 0) 1 )
        ((even? exp)
            (remainder
                (square (expmod base (/ exp 2) m))
                m))
        (else
            (remainder
                (* base (expmod base (- exp 1) m))
                m))))

(define (even? n)
    (= (remainder n 2) 0))

(define (fermat-test n)
    (define (try-it a )
      (= (expmod a n n) a)
    (try-it (+ (random (- n 1) 1)))
    ))

(define (fast-prime n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime n (- times 1)))
        (else false)))

(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (fast-prime n 1000000)
        (report-prime n (- (runtime) start-time))
        false))

(define (report-prime n elapsedtime)
    (newline)
    (display n)
    (display "***")
    (display elapsedtime)
    (display "***")
    true)

(define (search-for-primes n)
    (define (search-engine n count max-count)
        (if (even? n)
            (search-engine (+ n 1) count max-count)
            (if (< count max-count)
                (if (start-prime-test n (runtime))
                    (search-engine (+ n 2) (+ count 1) max-count)
                    (search-engine (+ n 2) count max-count))
                (display "Finish Calsulation"))))
    (search-engine n 0 3))

(define (cube a)
    (* a a a))

(define (sum-integers a b)
    (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
    (if (> a b)
        0
        (+ (cube a) (sum-cubes (+ a 1) b))))

; Procedure as arguments
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
            (sum term (next a) next b))))

;if we want to sum integers from a to b
(define (sum-integers a b )
    (define (inc n)
        (+ n 1))
    (define (identity x) x)
    (sum identity a inc b))
;(sum-integers 1 100)
