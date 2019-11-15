(define (abs x)
  (cond ((> x 0) x (+ 1 x) (+ 2 x))
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (expt b n)
  (define (even? k) (= (remainder k 2) 0))
  (define (expt-iter acc a ex)
    (cond ((= ex 1) (* acc a))
          ((even? ex) (expt-iter acc (* a a) (/ ex 2)))
          (else (expt-iter (* acc a) a (- ex 1)))))
  (expt-iter 1 b n))

(expt 2 7)
(expt-iter 2 3)
(expt-iter 2 3)
(expt-iter 4 2)
(expt-iter 8 1)

(expt-iter 4 4)
(expt-iter 8 2)
(expt-iter 16 1)
(expt-iter 32 0)

(expt-iter 2 3)
(expt-iter (* 2 2) (- 3 1))
(expt-iter 4 2)
(expt-iter 16 1)
(expt-iter 32 0)

(exp-iter 2 17)
(exp-iter 4 16)
(exp-iter 16 8)
(exp-iter 256 4)
(exp-iter 65536 2)
(exp-iter )

(define (mult a b)
  (define (double x) (* 2 x))
  (define (halve x) (/ x 2))
  (define (even? x) (= (remainder x 2) 0))
  (cond ((= b 1) a)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))))

(mult 12 5)

(define (fast-mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (even? x) (= (remainder x 2) 0))
  (define (mult-iter acc x y)
    (cond ((= y 1) (+ acc x))
          ((even? y) (mult-iter acc (double x) (halve y)))
          (else (mult-iter (+ acc a) a (- y 1)))))
  (mult-iter 0 a b))

(fast-mult 5 5)

(define (fib n) (fib-iter 1 0 0 1 n))

(define (even? x) (= (remainder x 2) 0))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))

(fib 7)
(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1) (= n (smallest-divisor n))))

(prime? 2)

(prime? 57)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(fermat-test 11)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let*
             ((inner (expmod base (/ exp 2) m))
              (sq (square inner))
              (rem (remainder sq m))
              (rtrem (remainder inner m)))
           (if (and (= rem 1) (> rtrem 1) (< rtrem (- m 1))) 0 rem)))
        (else
         (remainder
          (* base (expmod base (- exp
                                  1) m))
          m))))
(even? 2)

if remainder square .... m is 1:
- and its sqrt is not 1 or n-1
- return 0

(define (is-rt-unity base exp))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n count)
  (start-prime-test n (runtime) count))

(define (start-prime-test n start-time count)
  (if (fast-prime? n 100000)
      (begin
          (report-prime n (- (runtime) start-time))
          (+ count 1))
      count))
(define (report-prime p elapsed-time)
  (display p)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start count)
  (define (search-iter n c)
    (cond
     ((= c count) '())
     (else (search-iter (+ n 1) (timed-prime-test n c)))))
  (search-iter start 0))

(search-for-primes 1000000000000000000000 3)

(define (next x)
  (if (= x 2) 3 (+ x 2)))

(define (is-carmichael n)
  (define (car-iter a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (car-iter (+ a 1)))
          (else false)))
    (car-iter 1))


(is-carmichael 6601)

(define (mr-test n)
  ())

(define (!= a b) (not (= a b)))
(define (mr-test n)
  (define (try-it a)
    (!= (expmod a (- n 1) n) 0))
  (try-it (+ 1 (random (- n 1)))))

(mr-test 12)

(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((mr-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime-mr? 104 1000)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx))

(define (const x) (lambda (y) x))
(integral (const 100) 0 1 0.0001)

(define (simpson f a b n)
  (define h (/ (- b a) (* 2 n)))
  (define (y k) (f (+ a (* k h))))
  (define (term i)
    (cond ((= i 0) (y i))
          ((= i (* 2 n)) (y i))
          (else (* (+ (* 2 (remainder i 2)) 2) (y i)))))
  (* (/ h 3) ((lambda (term a next b) (fold-range + 0 term a next b)) term 0 (lambda (x) (+ x 1)) (* 2 n))))

(define (cube x) (* x x x))
(simpson cube 0 1.0 10000)
(integral cube 0 1.0 0.0001)

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (fold-range op init term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (op (term a) result))))
  (iter a init))

(fold-range * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 4)

(define (fact n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (fold-range * 1 id 1 inc n))

(fact 6)

(define (pi k)
  (define (inc2 x) (+ x 2))
  (define (term y) (* (/ y (- y 1)) (/ y (+ y 1))))
  (* (/ 8.0 3) (fold-range * 1 term 4.0 inc2 k)))

(pi 1000000)

(define (accum-rec op init term a next b)
  (if (> a b)
      init
      (op (term a) (accum-rec op init term (next a) next b))))

(accum-rec + 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)

(define (filtered-accum op init term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (op (term a) result))
            (iter (next a) result))))
  (iter a init))

(define (identity x) x)
(define (inc x) (+ x 1))
(gcd 2 4)
(define (relatively-prime? x y) (= 1 (gcd x y)))

(define (rp-product n)
  (filtered-accum * 1 identity 1 inc n (lambda (x) (relatively-prime? x n))))

(rp-product 20)

(define (f g) (g 2))

(define (search f neg-point pos-point)
  (define (average a b) (/ (+ a b) 2.0))
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "values are not of opposite signs" a b)))))

(half-interval-method sin 2.0 4.0)

(define (fixed-point f init)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try init))

(define (average x y) (/ (+ x y) 2))
(fixed-point (lambda (y) (average y (/ 2 y))) 1.0)

(fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

(expt 4.5555322708 4.5555322708)

(define (flip x) (lambda (y z) (x z y)))
(define (cont-frac n d k)
  (define (iter i result)
    (display result)
    (newline)
    (if (= i 0) result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (cont-frac-next n d))

(/ 1 ((cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 10) 0))


n1/cont-frac - d1
if x = 1 return lambda f : n1 / f
else return lambda x : cont-frac k-1 (nk  / (dk + x))

; what the heck is this??? corecursion or something?
(define (cont-frac-rec n d k)
  (if (= k 1)
      (lambda (x) (/ (n 1) (+ (d 1) x)))
      (lambda (x) ((cont-frac-rec n d (- k 1)) (/ (n k) (+ (d k) x))))))

(define (e-denom i)
  (if (= (remainder i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))

(e-denom 8)

(+ 2 (cont-frac (lambda (x) 1.0) e-denom 100))

(define (tan-cf x k)
  (- 0 (cont-frac (lambda (y) (- 0 (expt x (min y 2)))) (lambda (y) (- (* 2 y) 1)) k)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (nthrt x n)
  (fixed-point (average-damp (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(nthrt (expt 7 5) 5)



(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

((deriv square) 1.0)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrtt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrttt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrtttt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

(define (cubic a b c)
  (define (cube a x) (* a x x x))
  (define (square a x) (* a x x))
  (lambda (x) (+ (cube 1 x) (square a x) (* b x) c)))

((cubic 1 2 3) (newtons-method (cubic 1 2 3) 1.0))

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1)

(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

(define (smooth f)
  (define (average a b c)
    (/ (+ a b c) 3))
  (let ((dx 0.00001))
    (lambda (x)
      (average (f (- x dx)) (f x) (f (+ x dx))))))

(define (nsmooth f n)
  ((repeated smooth n) f))

((nsmooth square 10) 5)

(define (nthrt x n)
  (fixed-point ((repeated average-damp n) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(nthrt 200 20)

(log 40)

(ceiling 2.5)

(expt (nthrt 200 20) 20)

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter g)
      (if (good-enough? g)
          g
          (iter (improve g))))
    (iter guess)))


(define (fixed-point f init)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try init))

(define (fp-ii f init)
  ((iterative-improve
    (lambda (g) (< (abs (- g (f g))) 0.00001))
    f)
   init))

(define (nthii x n)
  (fp-ii ((repeated average-damp n) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(nthii 27 3)
(average 1 2)
(define (sqrtii x)
  ((iterative-improve
    (lambda (g) (< (abs (- (square g) x)) 0.0001))
    (lambda (g) (average g (/ x g))))
   1.0))

(sqrtii 9)


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(cons 1 (cons 2 3))

(= (cons 1 2) (cons 1 2))

(define z (cons (cons 1 2) (cons 3 4)))

(cdr (car z))

(define (make-rat n d)
  (define (pos? x)
    (>= x 0))
  (define (same? x y)
    (or (and x y) (and (not x) (not y))))
  (let ((div (gcd n d)))
    (if (same? (pos? n) (pos? d))
        (cons (abs (/ n div)) (abs (/ d div)))
        (cons (/ (* -1 (abs n)) div) (abs (/ d div))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(mul-rat (make-rat 37 18) (make-rat 12 46))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 128 256))


(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))
(define (make-segment a b) (cons a b))
(define (start-segment a) (car a))
(define (end-segment a) (cdr a))
(define (midpoint-segment a b)
  (make-point (average (x-point a) (x-point b)) (average (y-point a) (y-point b))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ") "))

(print-point (make-point 1 2))

(midpoint-segment (make-point 1 1) (make-point -1 1))

(define (make-rect pt h w)
  (cons pt (cons h w)))

(define (norm point)
  (sqrt (+ (square (x-point point)) (square (y-point point)))))

(define (add-points x y)
  (make-point (+ (x-point x) (x-point y)) (+ (y-point x) (y-point y))))

(define (scale-point a x)
  (make-point (* a (x-point x)) (* a (y-point x))))

(define (neg pt)
  (scale-point -1 pt))
(define (sub-points x y)
  (add-points x (neg y)))
(print-point (scale-point 2 (make-point 1 2)))
(define (segment-length x)
  (norm (sub-points (end-segment x) (start-segment x))))

(segment-length (make-segment (make-point 0 0) (make-point 1 1)))

(define (rect-height r)
  (car (cdr r)))

(define (rect-width r)
  (cdr (cdr r)))

(define (perimeter r)
  (* 2 (+ (rect-height r) (rect-width r))))

(define (area r)
  (* (rect-height r) (rect-width r)))

(perimeter (make-rect (make-point 1 1) 2 3))

(define (make-rect bl tr)
  (cons bl tr))

(define (rect-height r)
  (abs (y-point (sub-points (car r) (cdr r)))))

(define (rect-width r)
  (abs (x-point (sub-points (car r) (cdr r)))))

(area (make-rect (make-point 1 1) (make-point 2 3)))

(define (: a b)
  (define (dispatch m)
    (cond ((= m 0) a)
          ((= m 1) b)
          (else (error "invalid"))))
  dispatch)

(define (fst z) (z 0))
(define (snd z) (z 1))

(fst (: 1 2))

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

(car2 (cons2 1 2))

(define (ncons a b)
  (* (expt 2 a) (expt 3 b)))

(define (w/o-n-part a n)
  (let ((rem (remainder a n)))
    (cond
     ((= a n) 1)
     ((= rem 0) (w/o-n-part (/ a n) n))
     (else a))))

(w/o-n-part 10 2)
(define (ncar z)
  (log-a (w/o-n-part z 3) 2))

(define (ncdr z)
  (log-a (w/o-n-part z 2) 3))
(ncar (ncons 7 5))
(ncdr (ncons -79 59))

(nth-part 10 3)

(define (log-a a n)
  (/ (log a) (log n)))

(expt 2 (log-a 23 2))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (succ n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (cplus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))


(define (add-interval x y)
  (mk-interval (+ (lb x) (lb y))
               (+ (ub x) (ub y))))

(define (mul-interval x y)
  (let ((p1 (* (lb x) (lb y)))
        (p2 (* (lb x) (ub y)))
        (p3 (* (ub x) (lb y)))
        (p4 (* (ub x) (ub y))))
    (mk-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (cant-div? a)
    (and (>= (ub a) 0) (<= (lb a) 0)))
  (if (or (cant-div? x) (cant-div? y)) (error "can't divide by zero")
  (mul-interval
   x
   (mk-interval (/ 1.0 (ub y))
                (/ 1.0 (lb y))))))

(div-interval (mk-interval -1 1) (mk-interval 1 2))

(define (sub-interval x y)
  (add-interval
   x
   (mk-interval (- 0 (ub y))
                (- 0 (lb y)))))

(define (mk-interval a b) (cons a b))

(define (ub x) (cdr x))
(define (lb x) (car x))

(define (print-interval x)
  (newline)
  (display "[")
  (display (lb x))
  (display ", ")
  (display (ub x))
  (display "] "))

(print-interval
 (sub-interval
  (mk-interval 0 1)
  (mk-interval 2 3)))

(define (interval-width x)
  (/ (- (ub x) (lb x)) 2))

(interval-width (mk-interval -3 2))

(define (mul-interval-9 x y)
  (define (pos? x) (>= x 0))
  (define (neg? x) (not (pos? x)))
  (let ((lbx (lb x))
        (lby (lb y))
        (ubx (ub x))
        (uby (ub y)))
    (cond ((and (pos? lbx) (pos? lby)) (mk-interval (* lbx lby) (* ubx uby)))
          ((and (neg? ubx) (neg? uby)) (mk-interval (* ubx uby) (* lbx lby)))
          ((and (neg? ubx) (pos? lby)) (mk-interval (* lbx uby) (* ubx lby)))
          ((and (neg? uby) (pos? lbx)) (mk-interval (* lby ubx) (* uby lbx)))
          ((and (neg? ubx) (neg? lby) (pos? uby)) (mk-interval (* lbx uby) (* lbx lby)))
          ((and (neg? uby) (neg? lbx) (pos? ubx)) (mk-interval (* lby ubx) (* lby lbx)))
          ((and (pos? lbx) (neg? lby) (pos? uby)) (mk-interval (* ubx uby) (* lby ubx)))
          ((and (pos? lby) (neg? lbx) (pos? ubx)) (mk-interval (* uby ubx) (* lbx uby)))
          (else (mk-interval (min (* lbx uby) (* lby ubx)) (max (* lbx lby) (* ubx uby)))))))

(mul-interval (mk-interval -1 1) (mk-interval -2 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lb i) (ub i)) 2))

(define (width i)
  (/ (- (ub i) (lb i)) 2))

(define (make-center-percent c p)
  (let ((width (* (/ p 100) c)))
    (mk-interval (- c (/ width 2)) (+ c (/ width 2)))))

(define (percent i)
  (let ((width (* 2 (width i)))
        (center (center i)))
    (* 100 (/ width center))))

(percent (make-center-percent 2 20))

tolerance = width / center
= (ub - lb) / 2 * center
= (ub - lb) / (ub + lb)
= ((ub1 * ub2) - (lb1 * lb2)) / (ub1 * ub2 + lb1 * lb2)

(ub1 - lb1)(ub2 - lb2)/(ub1 + lb1)(ub2 + lb2)
= (ub1ub2 - ub1lb2 - lb1ub2 + lb1lb2)/(ub1ub2 + lb1lb2 + ub1lb2 + lb1ub2)

w1w2/c1c2

(2ub1ub2 - 2lb1lb2 + lb1lb2 - ub1ub2 - ub1)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (mk-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))


(par1 (make-center-percent 20000 10) (make-center-percent 10000 0))




(list 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (last-pair list1)
  (if (= (length list1) 1)
      list1
      (last-pair (cdr list1))))

(last-pair (list 23 72 149 34))

(define (reverse_ lst)
  (if (null? lst)
      lst
      (cons (reverse_ (cdr lst)) (car lst))))

(reverse '(1 4 9 16 25))
(list 1 4 9 16 25)

(define (count-change amount) (cc amount 5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination
                         coin-values))
                     coin-values)))))
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(cc 100 (list 25 50 10 5 1))

(define (same-parity x . xs)  
  (define (same-iter acc vals)
    (cond ((null? vals) acc)
          ((= (remainder (+ x (car vals)) 2) 0) (same-iter (cons (car vals) acc) (cdr vals)))
          (else (same-iter acc (cdr vals)))))
  (reverse (same-iter (list x) xs)))

(same-parity 2 3 4 5 6 7)

(define nil (list))
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))

(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map (lambda (x) (* 10 x)) (list 1 2 3 4 5))

(define (bigmap proc xss)
  (cond ((null? xss) (list))
        ((null? (car xss)) (list))
        (else
         (let ((cx (map cdr xss)))
           (cons
            (apply proc (map car xss))
            (bigmap proc cx))))))

(define (map_ proc . xss)
  (bigmap proc xss))

(map_ + (list 1 2 3) (list 4 5 6) (list 7 8 9))

(define (flatten f p)
  (lambda (xs) (apply f (cons p xs))))

(flatten + '(1 2 3))

(apply + '(1 2 3))

(apply car '((1 2 3)))

(define (square-list-old items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items)) (square-list-old (cdr items)))))

(square-list-old (list 1 2 3 4))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(square-list '(1 2 3 4))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))

(define (for-each proc xs)
  (if (null? xs)
      true
  (and (proc (car xs))
  (for-each proc (cdr xs)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 12 14 16))

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)

(define (count-leaves tree)
  (cond ((null? tree) 0)
      ((not (pair? tree)) 1)
      (else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))


(define (deep-reverse xs)
  (if (null? xs)
      xs
      (cons (deep-reverse (cdr xs)) (car xs))))

(define (foldl init op xs)
  (define (foldl-iter res rem)
    (if (null? rem)
        res
        (foldl-iter (op (car rem) res) (cdr rem))))
  (foldl-iter init xs))

(foldl 0 + '(1 2 3 4 5))
(define (correct-reverse xs)
  (foldl '() cons xs))

(correct-reverse '(1 2 3 4 5))

(define (deep-reverse ts)
  (cond ((null? ts) ts)
        ((not (pair? ts)) ts)
        (else (correct-reverse (map deep-reverse ts)))))

(deep-reverse (list (list 1 2) (list 3 4)))

(define (fringe ts)
  (display ts)
  (newline)
  (cond ((null? ts) ts)
        ((not (pair? ts)) (list ts))
        (else (append (fringe (car ts)) (fringe (cdr ts))))))

(define x (list (list 1 2) (list 3 4)))

(fringe (list x x))
(car x)
(cdr x)

(define (make-mobile left right)
  (list left right))

(define (make-mobile-cons left right)
  (cons left right))

(define (make-branch-cons length structure)
  (cons length structure))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mob)
  (if (pair? mob)
      (car mob)
      mob))

(define (left-branch-cons mob)
  (if (pair? mob)
      (car mob)
      mob))

(define (right-branch-cons mob)
  (if (pair? mob)
      (cdr mob)
      mob))

(branch-structure-cons (right-branch-cons mb-cons))

(define (right-branch mob)
  (if (pair? (cdr mob))
      (cadr mob)
      mob))
(define (branch-length br)
  (car br))
(define (branch-length-cons br)
  (car br))

(define (branch-structure-cons br)
  (cdr br))

(define (branch-structure br)
  (cadr br))

(define (total-weight-cons mob)
  (if (not (pair? mob))
      mob
      (+ (total-weight-cons
          (branch-structure-cons (left-branch-cons mob)))
         (total-weight-cons
          (branch-structure-cons (right-branch-cons mob))))))


(define (total-weight mob)
  (if (not (pair? mob))
      mob
      (+ (total-weight
          (branch-structure (left-branch mob)))
         (total-weight
          (branch-structure (right-branch mob))))))

(define mb-cons (make-mobile-cons
                 (make-branch-cons 2 (make-mobile-cons (make-branch-cons 2 5) (make-branch-cons 2 5)))
                 (make-branch-cons 2 (make-mobile-cons (make-branch-cons 2 5) (make-branch-cons 2 5)))))
(define mb (make-mobile
            (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 2 5)))
            (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 2 5)))))

(total-weight mb)
(total-weight-cons mb-cons)
(branch-structure (right-branch mb))

(define (is-balanced? mob)
  (display mob)
  (newline)
  (if (not (pair? mob))
      true
      (and
       (=
       (* (branch-length (left-branch mob)) (total-weight (branch-structure (left-branch mob))))
       (* (branch-length (right-branch mob)) (total-weight (branch-structure (right-branch mob)))))
       (is-balanced? (branch-structure (left-branch mob)))
       (is-balanced? (branch-structure (right-branch mob))))))

(is-balanced? mb)

(and
(=
(* (branch-length (left-branch mb)) (total-weight (branch-structure (left-branch mb))))
(* (branch-length (right-branch mb)) (total-weight (branch-structure (right-branch mb))))))

(branch-structure (right-branch mb))

(and true true true)

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
(define tr (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define (square-tree-2 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-2 (car tree))
                    (square-tree-2 (cdr tree))))))
(square-tree-2 tr)

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(tree-map square tr)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3))

(define (filter predicate? xs)
  (cond ((null? xs) xs)
        ((predicate? (car xs))
         (cons (car xs) (filter predicate? (cdr xs))))
        (else (filter predicate? (cdr xs)))))

(filter odd? '(1 2 3 4 5
                 ))

(define (reduce op init xs)
  (if (null? xs)
      init
      (op (car xs)
          (reduce op init (cdr xs)))))

(reduce )


(define (-> init . fs)
  (if (null? fs)
      init
      (apply thread (cons ((car fs) init) (cdr fs)))))

(thread 1 (lambda (x) (+ x 1)) (lambda (x) (+ x 2)))

(thread 2 square square square)

(define (_cons x)
  (lambda (ys) (cons x ys)))

(thread '() (_cons 1) (_cons 2) (_cons 3))

(define (range low high)
  (if (> low high)
      nil
      (cons low (range (+ low 1) high))))

(define (curry f x) (lambda (y) (apply f (append x (list y)))))

(thread '(2 3 4) (curry map (list square))) ((curry reduce '(+ 0)))
(define (_map f)
  (lambda (xs) (map f xs)))

(define (_reduce op init)
  (lambda (xs) (reduce op init xs)))

(define (_+ x)
  (lambda (y) (+ x y)))
(define (_* x)
  (lambda (y) (* x y)))
(define (_- x)
  (lambda (y) (- x y)))
(define (_/ x)
  (lambda (y) (/ x y)))


(define (_filter pred?)
  (lambda (xs) (filter pred? xs)))
(-> (range 1 20)
    (_map (_* 2))
    (_map (_* 3))
    (_map square)
    (_reduce * 1)
    (_snoc '(1 2 3)))

(define (_snoc xs)
  (lambda (y) (cons y xs)))

(define (enumerate-tree ts) (fringe ts))

(define (sum-odd-squares ts)
  (-> ts
      enumerate-tree
      (_filter odd?)
      (_map square)
      (_reduce + 0)))

(fib 5)
(define (_range x)
  (lambda (y) (range x y)))

(define (even-fibs n)
  (-> n
      (_range 0)
      (_map fib)
      (_filter even?)
      (_reduce cons nil)))

(even-fibs 10)

(sum-odd-squares (list (list 1 2) (list 3 (list 4 5) (list 6 7))))

(define (list-fib-squares n)
  (-> n
      (_range 0)
      (_map fib)
      (_map square)
      (_reduce cons nil)))

(list-fib-squares 20)

(define (prod-sq-odd-el seq)
  (-> seq
      (_filter odd?)
      (_map square)
      (_reduce * 1)))

(prod-sq-odd-el (range 1 5))

(define (accmap f xs)
  (-> xs
      (_reduce (lambda (x y) (cons (f x) y)) nil)))

(define (accappend xs ys)
  (-> xs
      (_reduce cons ys)))

(define (acclength xs)
  (-> xs
      (_reduce (lambda (x y) (if (null? x) y (+ y 1))) 0)))

(acclength '())
(accappend '(1 2 3) '(4 5 6))
(accmap square '(1 2 3))

(define (horner-eval x coeffs)
  (-> coeffs
      (_reduce (lambda (coef higher)
                 (+ coef (* x higher)))
               0)))

(horner-eval 2 (list 1 3 0 5 0 1))

(define (_count-leaves ts)
  (-> ts
      (_map (lambda (x) (if (not (pair? x)) 1 (_count-leaves x))))
      (_reduce + 0)))

(count-leaves (list (list 1 2 (list 3 5)) (list 5 (list 2 6 7))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

(define (fringe ts)
  (display ts)
  (newline)
  (cond ((null? ts) ts)
        ((not (pair? ts)) (list ts))
        (else (append (fringe (car ts)) (fringe (cdr ts))))))

(define (_enumerate-tree ts)
  (-> ts
      (_reduce (lambda (x y)
                 (display y)
                 (newline)
                 (cond ((null? x) y)
                       ((not (pair? x)) (cons x y))
                       (else (append (_enumerate-tree x) y))))
               '())))

(define (reduce-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (reduce op init (-> seqs (_map car)))
            (reduce-n op init (-> seqs (_map cdr))))))
(define (_reduce-n op init)
  (lambda (xss)
    (reduce-n op init xss)))
(-> (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))
    (_reduce-n + 0))

(define (dot-product v w)
  (reduce + 0 (map * v w)))

(dot-product '(1 2 3) '(1 2 3))

(define-macro (curry func args)
        `(lambda (a) (apply ,func (append ,args (list a)))))

(define-syntax _
  (syntax-rules ()
    ((_ func args)
     ((lambda (x) (apply func (list args x)))))))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define mt '((1 1 1) (2 2 2) (3 3 3)))

(define (const n)
  (lambda (x) n))

((const '()) 1)
(define (transpose mat)
  (-> mat (_reduce-n cons '())))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (-> m
        (_map (lambda (x) (matrix-*-vector cols x))))))

(matrix-*-matrix mt (scalar 3 4))

(define (scalar n k)
  (if (= n 1)
      (list (list k))
      (let ((scn1
             (map (lambda (x)
                    (cons 0 x))
                  (scalar (- n 1) k))))
        (cons
         (cons k (map (const 0) (range 1 (- n 1)))) scn1))))

(scalar 4 4)

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define fold-right reduce)

(fold-right + 1 '(1 2 3))
(fold-left + 1 '(1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil '(1 2 3))

(define (rev sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (revl sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(revl '(1 2 3 4 5))

(define (flatmap proc seq)
  (-> seq
      (_map proc)
      (_reduce append '())))

(define (_flatmap proc)
  (lambda (seq) (flatmap proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (-> n
      (_range 1)
      (_flatmap (lambda (i)
                  (-> (- i 1)
                      (_range 1)
                      (_map (lambda (j)
                              (list i j))))))
      (_filter prime-sum?)))

(prime-sum-pairs 10)

(define-syntax fn
  (syntax-rules ()
    ((fn vars expr)
     `(lambda ,(vars) expr))))

((fn (x) (+ x 1)) 1)

(define-macro (fn (vars) (expr))
  `(lambda ,(vars) ,(expr)))

(define (permutations s)
  (if (null? s)
      (list '())
      (-> s
          (_flatmap (lambda (x)
                      (-> (permutations (remove x s))
                          (_map (lambda (p)
                                  (cons x p)))))))))
(define (remove item sequence)
  (-> sequence
      (_filter (lambda (x) (not (= x item))))))

(-> '(1 2 3 4 5) permutations)

(define (unique-pairs n)
  (-> n
      (_range 1)
      (_flatmap (lambda (i)
                  (-> (- i 1)
                      (_range 1)
                      (_map (lambda (j)
                              (list i j))))))))

(unique-pairs 5)

(define (prime-sum-pairs n)
  (-> (unique-pairs n)
      (_filter prime-sum?)))

(prime-sum-pairs 10)

(define (unique-triples n)
  (-> n
      (_range 1)
      (_flatmap (lambda (i)
                  (-> (- i 1)
                      (_range 1)
                      (_flatmap (lambda (j)
                              (-> (- j 1)
                                  (_range 1)
                                  (_map (lambda (k)
                                          (list i j k)))))))))))

(unique-triples 10)

(define (triples-sum-to n s)
  (-> (unique-triples n)
      (_filter (lambda (triple) (= (apply + triple) s)))))

(triples-sum-to 10 6)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (-> (queen-cols (- k 1))
            (_flatmap (lambda (rest-of-queens)
                        (-> board-size
                            (_range 1)
                            (_map (lambda (new-row)
                                    (adjoin-position
                                     new-row rest-of-queens))))))
            (_filter safe?))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new rest)
  (cons new rest))

  ;(map (lambda (x) (cons new x)) rest))
(flatmap (lambda (rest-of-queens)
(-> 2
    (_range 1)
    (_map (lambda (new-row)
            (adjoin-position
             new-row rest-of-queens))))) '((1)))
(-> '((1 2 1) (2 2 1) (3 2 1))
    (_filter safe?))
(adjoin-position 4 '())
(range 1 2)
(define (safe? positions)
  (and (-> (cdr positions)
      (_filter (lambda (x) (= (car positions) x)))
      null?) (not (hits-diagonal (car positions) (cdr positions)))))
(hits-diagonal 1 '(2 3))
(define (last xs)
  (cond ((null? xs) xs)
        ((= 1 (length xs)) (car xs))
        (else (last (cdr xs)))))

(safe? '(3 7 2 8 5 1 4 6))

(lambda (val acc)
  (78))
(define (first-k k xs)
  (cond
      ((= 0 k) '())
      ((>= k (length xs)) xs)
      ((= 1 (length xs)) xs)
      (else (cons (car xs) (first-k (- k 1) (cdr xs))))))

(define (hits-diagonal new rest)
  (if (null? rest)
      false
      (or (= (+ (car rest) 1) new) (= (- (car rest) 1) new)
          (hits-diagonal (- (car rest) 1) (cdr rest)) (hits-diagonal (+ (car rest) 1) (cdr rest)))))
(hits-diagonal 3 '(1 2))
(safe? )
(define (display-and-return x)
  (display x)
  (newline)
  x)
(map car empty-board)

(safe? '((1 2 3)) (2 3 4) (5 6 7))

(queens 6)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (cons (+ (car v1) (car v2)) (+ (cdr v1) (cdr v2))))

(define (scale-vect a v)
  (cons (* a (car v)) (* a (cdr v))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cddr f))

(define (origin-frame f)
  (car f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (draw-line s1 s2)
  (display "start: ")
  (display s1)
  (display ", end: ")
  (display s2)
  (newline))
(draw-line (make-vect 1 2) (make-vect 3 4))
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

((segments->painter (list
                     (make-segment
                      (make-vect 0 1)
                      (make-vect 1 0))
                     (make-segment
                      (make-vect 1 0)
                      (make-vect 0 1))))
 (make-frame (make-vect 9 10) (make-vect 11 12) (make-vect 13 14)))



(cdr (cons (make-vect 1 2) (make-vect 3 4)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))


(define (memq item x) ; equivalent to \x -> dropWhile (/= item) x in haskell
  (cond ((null? x) false)
        ((eq? item (car x)) x)
         (else (memq item (cdr x)))))


(memq 'a '(b c (d e f) a b cd ef))

(define (_equal? xs ys)
  (display "xs: ")
  (display xs)
  (newline)
  (display "ys: ")
  (display ys)
  (newline)
  (cond ((and (symbol? xs) (symbol? ys)) (eq? xs ys))
        ((or (null? xs) (null? ys)) true)
        ((or (not (pair? xs)) (not (pair? ys))) (eq? xs ys))
        (else (and (_equal? (car xs) (car ys)) (_equal? (cdr xs) (cdr ys))))))

(_equal? '(a b c d) '(a b 1 c))


DOIN A BOOOORK

(define (print-bork-repeatedly n)
  (display "bork")
  (newline)
  (if (not (= n 0)) (print-bork-repeatedly (- n 1))))

(define (deriv exp var)
  (display exp)
  (newline)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (sum-fst exp) var)
                              (deriv (sum-snd exp) var)))
        ((product? exp)
         (make-sum
          (make-product (mul-fst exp)
                        (deriv (mul-snd exp) var))
          (make-product (deriv (mul-fst exp) var)
                        (mul-snd exp))))
        ((exp? exp)
         (make-product
          (make-product (exp-snd exp)
                        (make-exp (exp-fst exp) (make-sum (exp-snd exp) -1)))
          (deriv (exp-fst exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(deriv '(** x -2) 'x)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (product? x) (and (pair? x) (not (sum? x)) (memq '* x)))

(define (mul-fst p) (car p))
(define (mul-fst p) (car-memq '* p))

(define (mul-fst p)
  (let ((res (car-memq '* p)))
    (if (= (length res) 1) (car res) res)))

(define (mul-snd p)
  (let ((prod (cddr p)))
    (if (null? (cdr prod)) (car prod)
        (make-product (car prod) (cadr prod)))))
(define (mul-snd p) (cdr-memq '* p))

(define (mul-snd p)
  (let ((res (cdr-memq '* p)))
    (if (= (length res) 1) (car res) res)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (not (pair? m1)) (not (pair? m2))) (list m1 '* m2))
        ((and (pair? m2) (not (pair? m1))) (list m1 '* m2))
        ((and (pair? m2) (or (number? (car m2)) (symbol? (car m2)))) (cons m1 (cons '* m2)));(cons '* (cons m1 m2)))
        ((not (pair? m1)) (cons m1 (cons '* m2)))
        (else (append m1 (cons '* m2)))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (sum? x)
  (and (pair? x) (memq '+ x)))
(define (sum-fst s) (car s))
(define (sum-fst s) (car-memq '+ s))

(define (sum-fst s)
  (let ((res (car-memq '+ s)))
    (if (= (length res) 1) (car res) res)))

(define (sum-snd s)
  (let ((sm (cddr s)))
    (if (null? (cdr sm)) (car sm)
        (make-sum (car sm) (cadr sm)))))

(define (sum-snd s)
  (let ((res (cdr-memq '+ s)))
    (if (= (length res) 1) (car res) res)))

(cadr ()cddr '(x + 3 * (x + y + 2)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((and (not (pair? a1)) (not (pair? a2))) (list a1 '+ a2))
        ((and (pair? a2) (or (number? (car a2)) (symbol? (car a2)))) (cons a1 (cons '+ a2)))
        ((not (pair? a1)) (cons a1 (cons '+ a2)))
        (else (append a1 (cons '+ a2)))))

(make-sum '(1 * 3) '(x + y))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sine? x) (and (pair? x) (eq? (car x) 'sin)))

(define (make-sin x)
  (if (number? x) (sin x) (list 'sin x)))

(define (cos? x) (and (pair? x) (eq? (car x) 'cos)))
(define (make-cos x)
  (if (number? x) (cos x) (list 'cos x)))

(define (tan? x) (and (pair? x) (eq? (car x) 'tan)))
(define (make-tan x) (if (number? x) (tan x) (list 'tan x)))
(define (exp? x) (and (pair? x) (eq? (car x) '**)))
(define (make-exp a b)
  (cond ((=number? a 0) 0)
        ((=number? b 0) 1)
        ((=number? b 1) b)
        ((=number? a 1) a)
        ((and (number? a) (number? b)) (expt a b))
        (else (list '** a b))))

(define (exp-fst e) (cadr e))
(define (exp-snd e) (caddr e))
(define (ln? x) (and (pair? x) (eq? (car x) 'log)))
(define (make-ln a)
  (if (number? a) (log a) (list 'ln a)))


(deriv '(+ (* 2 (* x x)) (* 4 x) 4) 'x)

(deriv '(* x y (+ x 3)) 'x)

(mul-snd '(* 1 2 3 4 5))

(cdr (cddr '(* x y (+ x 3))))

(deriv '((x * x) + 1) 'x)

(define (parse-expr expr)
  (con))

(define (car-memq item x)
  (let ((result (memq item (reverse x))))
    (cond ((not (pair? result)) #f)
          ((= 1 (length result)) (car result))
    (else (reverse (cdr result))))))

(define (cdr-memq item x)
  (let ((result (memq item x)))
    (cond ((not (pair? result)) #f)
          ((= 1 (length result)) (car result))
          (else (cdr result)))))

(memq '+ )
(car-memq '+ '(1 *  2 + 2))
(car-memq '* '(3 * (x + y + 2)))
(deriv '(x + 3 * (x * x + y + 2)) 'x)

(make-sum 'x 'x)

                                        ; HUGE MESS ABOVE BUT WHATEVS!!!!!!!

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(union-set '(1 2 3 4) '(3 4 5 6))
;ordered list representation
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-list (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((> x1 x2)
               (intersection-set-list set1 (cdr set2)))))))
(intersection-set '(1 2 3 4 5) '(3 4 5 6 7))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 '(1 3 4 6))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))))

(union-set '(1 3 6 8 9 13 16) '(4 6 9 13 17 20))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define test-tree (make-tree 4 (make-tree 2 (make-tree 1 '() '()) (make-tree 3 '() '())) (make-tree 6 '() '())))

(element-of-set? 3 test-tree)

(< 5 (entry (right-branch test-tree)))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))
(define (adjoin-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((<= x (entry set))
         (make-tree (entry set)
                    (adjoin-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-tree x (right-branch set))))))
(adjoin-set 5 '())

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(tree->list-2 '())

(define (leaf x) (make-tree x '() '()))
(define t1 (make-tree 7 (make-tree 3 (leaf 1) (leaf 5)) (make-tree 9 '() (leaf 11))))
(define t2 (make-tree 3 (leaf 1) (make-tree 7 (leaf 5) (make-tree 9 '() (leaf 11)))))
(define t3 (make-tree 5 (make-tree 3 (leaf 1) '()) (make-tree 9 (leaf 7) (leaf 11))))

(tree->list-1 (make-tree 4 (make-tree 3 (make-tree 2 (leaf 1) '()) '()) '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))

                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
;base case is trivial.
; recursive case: first, make a partial tree of length approx half of what you want. you then have (recursively) your left subtree, plus the leftover elements. take the car of your leftover elements: this will be the root of your tree. finally, make a partial tree, recursively, out of the remaining elements, big enough so that your tree will overall use n elements. the result will be your right subtree plus the remaining elements overall.

; '(1 3 5 7 9 11) -> left subtree is made from (1 3), remaining is (5 7 9 11), so root is 5
; (1 3) -> left subtree is made from (), remaining is (1 3), so root is 1, right tree is (tree 3 '() '())
; so our first left subtree is (tree 1 '() (tree 3 '() '()))
; right subtree is made from (7 9 11): left is from (7), right is from (9 11), so root is 9
; so we have (tree 9 (tree 7 '() '()) (tree 11 '() '()))
; overall: (tree 5 (tree 1 '() (tree 3 '() '())) (tree 9 (tree 7 '() '()) (tree 11 '() '())))

(list->tree)

; 2.63 gives us \Theta(n) tree-to-list; 2.64 gives us \Theta(n) list-to-tree
(define (union-set tree1 tree2)
  (list->tree (union-set (tree->list-1 tree1) (tree->list-1 tree2))))

(define (intersection-set tree1 tree2)
  (list->tree (intersection-set-list (tree->list-2 tree1) (tree->list-2 tree2))))

(tree->list-2 (intersection-set (list->tree '(1 2 3 4 5 6 7)) (list->tree '(3 4 5 6 7 8))))



(list->tree '(1 2 3 4 5 6 7))
(list->tree '(3 4 5 6 7 8))

(intersection-set '() '())
(tree->list-2 '())
(intersection-set-list '())

(define (intersection-set tree1 tree2)
  )

(tree->list-1 ())

(define (sort lst)
  (define (unsorted-list-to-tree xs)
    (if (null? xs)
        '()
        (adjoin-tree (car xs) (unsorted-list-to-tree (cdr xs)))))
  (tree->list-1 (unsorted-list-to-tree lst)))

(sort '(5 2 8 5 12 8 2 8))

(unsorted-list-to-tree '())

(adjoin-set)

(adjoin-set 1 '())

(define (unsorted-list-to-tree lst)
  (if (null? list)
      '()))

(null? '())

(define (lookup-binary-tree given-key set)
  (cond ((null? set) false)
        ((equal? given-key (entry set)) (entry set))
        ((< (key (entry set)) given-key) (lookup-binary-tree given-key (left-tree set)))
        ((> (key (entry set)) given-key) (lookup-binary-tree given-key (right-tree set)))))

(define (lookup-ordered-list given-key set)
  (cond ((null? set) false)
        ((equal? given-key (key (car set))) (car set))
        ((> (key (car set)) given-key) false)
        (else (lookup-ordered-list given-key (cdr set)))))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-in-set? sym set)
  (memq sym set))

  ;(cond ((null? set) false)
   ;     ((equal? sym (symbol-leaf (car set))) true)
    ;    (else (symbol-in-set? sym (cdr set)))))

(define (encode-symbol sym tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) sym)) '())
        ((symbol-in-set? sym (symbols (left-branch tree))) (cons 0 (encode-symbol sym (left-branch tree))))
        ((symbol-in-set? sym (symbols (right-branch tree))) (cons 1 (encode-symbol sym (right-branch tree))))
        (else (error "symbol not in tree"))
      ))
(decode (encode '(a d a b b c a) sample-tree) sample-tree)

(symbol-in-set? 'a '(a b c d))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge
               (adjoin-set
                (make-code-tree (car leaf-set) (cadr leaf-set))
                (cddr leaf-set))))))

(encode '(a d a b b c a) (generate-huffman-tree '((c 1) (d 1) (b 2) (a 3))))

(define song-tree (generate-huffman-tree '((WAH 1) (BOOM 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16))))

(decode (encode '(Get a job sha na na na na na na na get a job sha na na na na na na na wah yip yip yip yip yip yip yip yip sha boom) song-tree) song-tree)

                                        ; /\
                                        ;16 \
                                        ;   /\
                                        ;  8  \
                                        ;     /\
                                        ;    4  \
                                        ;       /\
                                        ;      2  1
                                        ;
                                        ;1 for most freq, n - 1 for least freq

