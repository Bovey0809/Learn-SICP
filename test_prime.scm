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

(define (sum-cubes a b )
  (define (increment arguments)
    (+ arguments 1))
    (sum cube a increment b))

(define (inc n)
    (+ n 1))

(define (simpson f a b n)
    (define h (/ (- b a) n))
    (define (add-h x)
        (+ x h))
    (define (add-2h x)
        (+ x (* 2 h)))
    (* (/ h 3)
        (+ (f a) (f b)
        (* 2 (sum f a add-h b))
        (* 2 (sum f (+ a h) add-2h (+ a (* (- n 1) h)))))))

;Exercise 1.30 write a procedure to use the iteration structure

(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result ;not 0
            (iter (next a) (+ (term a) result))))
    (iter a 0))

;Exercise 1.31 Write the product procedure of different versions
; a. recursive version
(define (product-r term a next b)
    (if (> a b)
        1 ;Here should be 1 instead of a
        (* (term a)
            (product-r term (next a) next b))))

(define (factorial n)
    (define (multiply x)
        x)
    (define (inc x)
        (+ x 1))
    (product-r multiply 1 inc n))

(define (product-i term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (factorial-i n)
    (define (multiply x)
        x)
    (define (inc x)
        (+ 1 x))
    (product-i multiply 1 inc n))

; exercise 1.32
(define (accumulate-i combine null-value term a next b)
    (if (> a b)
        null-value
        (accumulate-i combine (combine (term a) null-value) term (next a) next b)))

;use the accumulate-i to define sum-integers
(define (sum-acc a b)
    (define (add x y)
        (+ x y))
    (define (increment x)
        (+ x 1))
    (define (func x)
        x)
    (accumulate-i add 0 func a increment b))

;ues the recusive process to formulate accumulation
(define (accumulate-r combine null-value term a next b)
    (if (> a b)
        null-value
        (combine (term a)
                (accumulate-r combine null-value term (next a) next b))))

(define (sum-acc-r a b)
    (define (add x y)
            (+ x y))
    (define (increment x)
        (+ x 1))
    (define (func x) x)
    (accumulate-r add 0 func a increment b))
