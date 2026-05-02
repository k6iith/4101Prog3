;; Type predicates
(define (integer? x) (s48-integer? x))

(define (rational? x)
  (or (integer? x)
      (and (pair? x)
           (eq? (car x) 'rational)
           (pair? (cdr x))
           (integer? (car (cdr x)))
           (pair? (cdr (cdr x)))
           (integer? (car (cdr (cdr x))))
           (null? (cdr (cdr (cdr x)))))))

(define (number? x) (rational? x))

;; Numerator and denominator
(define (numerator x)
  (if (integer? x) x (car (cdr x))))

(define (denominator x)
  (if (integer? x) 1 (car (cdr (cdr x)))))

;; quotient by repeated subtraction
(define (q-nonneg a b)
  (if (b< a b) 0 (b+ 1 (q-nonneg (b- a b) b))))

(define (quotient a b)
  (if (b< a 0)
      (if (b< b 0)
          (q-nonneg (b- 0 a) (b- 0 b))
          (b- 0 (q-nonneg (b- 0 a) b)))
      (if (b< b 0)
          (b- 0 (q-nonneg a (b- 0 b)))
          (q-nonneg a b))))

;; remainder by repeated subtraction
(define (r-nonneg a b)
  (if (b< a b) a (r-nonneg (b- a b) b)))

(define (remainder a b)
  (let ((absb (if (b< b 0) (b- 0 b) b)))
    (if (b< a 0)
        (b- 0 (r-nonneg (b- 0 a) absb))
        (r-nonneg a absb))))

;; gcd - Euclid's algorithm
;; Reference: https://en.wikipedia.org/wiki/Euclidean_algorithm
(define (gcd a b)
  (let ((a (if (b< a 0) (b- 0 a) a))
        (b (if (b< b 0) (b- 0 b) b)))
    (if (b= b 0) a (gcd b (remainder a b)))))

(define (lcm a b)
  (if (or (b= a 0) (b= b 0)) 0
      (let ((a (if (b< a 0) (b- 0 a) a))
            (b (if (b< b 0) (b- 0 b) b)))
        (quotient (b* a b) (gcd a b)))))

;; Reduce n/d to lowest terms; keep denominator positive; return integer if d=1
(define (simplify-rational n d)
  (let ((g (gcd n d)))
    (let ((n (quotient n g))
          (d (quotient d g)))
      (if (b< d 0)
          (let ((n (b- 0 n)) (d (b- 0 d)))
            (if (b= d 1) n (list 'rational n d)))
          (if (b= d 1) n (list 'rational n d))))))

;; Binary arithmetic
(define (add2 x y)
  (simplify-rational
   (b+ (b* (numerator x) (denominator y))
       (b* (numerator y) (denominator x)))
   (b* (denominator x) (denominator y))))

(define (sub2 x y)
  (simplify-rational
   (b- (b* (numerator x) (denominator y))
       (b* (numerator y) (denominator x)))
   (b* (denominator x) (denominator y))))

(define (mul2 x y)
  (simplify-rational
   (b* (numerator x) (numerator y))
   (b* (denominator x) (denominator y))))

(define (div2 x y)
  (simplify-rational
   (b* (numerator x) (denominator y))
   (b* (denominator x) (numerator y))))

;; N-ary arithmetic
(define (+ . args)
  (if (null? args) 0
      (let loop ((r (car args)) (rest (cdr args)))
        (if (null? rest) r (loop (add2 r (car rest)) (cdr rest))))))

(define (- x . rest)
  (if (null? rest)
      (simplify-rational (b- 0 (numerator x)) (denominator x))
      (let loop ((r x) (rest rest))
        (if (null? rest) r (loop (sub2 r (car rest)) (cdr rest))))))

(define (* . args)
  (if (null? args) 1
      (let loop ((r (car args)) (rest (cdr args)))
        (if (null? rest) r (loop (mul2 r (car rest)) (cdr rest))))))

(define (/ x . rest)
  (if (null? rest)
      (simplify-rational (denominator x) (numerator x))
      (let loop ((r x) (rest rest))
        (if (null? rest) r (loop (div2 r (car rest)) (cdr rest))))))

;; Binary comparison helpers (valid when denominators are positive)
(define (num= x y)
  (b= (b* (numerator x) (denominator y))
      (b* (numerator y) (denominator x))))

(define (num< x y)
  (b< (b* (numerator x) (denominator y))
      (b* (numerator y) (denominator x))))

;; N-ary comparisons
(define (= x y . rest)
  (if (num= x y)
      (if (null? rest) #t (apply = (cons y rest)))
      #f))

(define (< x y . rest)
  (if (num< x y)
      (if (null? rest) #t (apply < (cons y rest)))
      #f))

(define (> x y . rest)
  (if (num< y x)
      (if (null? rest) #t (apply > (cons y rest)))
      #f))

(define (<= x y . rest)
  (if (num< y x) #f
      (if (null? rest) #t (apply <= (cons y rest)))))

(define (>= x y . rest)
  (if (num< x y) #f
      (if (null? rest) #t (apply >= (cons y rest)))))

;; Numeric predicates
(define (zero? x) (num= x 0))
(define (positive? x) (num< 0 x))
(define (negative? x) (num< x 0))
(define (abs x) (if (negative? x) (- x) x))

(define (max x . rest)
  (if (null? rest) x
      (let ((y (car rest)))
        (apply max (cons (if (num< x y) y x) (cdr rest))))))

(define (min x . rest)
  (if (null? rest) x
      (let ((y (car rest)))
        (apply min (cons (if (num< y x) y x) (cdr rest))))))

(define (not x) (if x #f #t))

;; Equality
(define (eqv? x y)
  (if (number? x)
      (if (number? y) (num= x y) #f)
      (eq? x y)))

(define (equal? x y)
  (if (number? x)
      (if (number? y) (num= x y) #f)
      (if (pair? x)
          (if (pair? y)
              (if (equal? (car x) (car y)) (equal? (cdr x) (cdr y)) #f)
              #f)
          (eq? x y))))

;; Association lists
(define (assq key alist)
  (if (null? alist) #f
      (if (eq? key (car (car alist))) (car alist)
          (assq key (cdr alist)))))

(define (assv key alist)
  (if (null? alist) #f
      (if (eqv? key (car (car alist))) (car alist)
          (assv key (cdr alist)))))

(define (assoc key alist)
  (if (null? alist) #f
      (if (equal? key (car (car alist))) (car alist)
          (assoc key (cdr alist)))))

;; Write: prints rationals as n/d, lists on one line
(define (w x)
  (if (number? x)
      (if (integer? x)
          (write x)
          (begin (write (numerator x)) (display "/") (write (denominator x))))
      (if (pair? x)
          (begin (display "(") (w (car x)) (w-rest (cdr x)) (display ")"))
          (write x))))

(define (w-rest x)
  (if (null? x) '()
      (if (pair? x)
          (begin (display " ") (w (car x)) (w-rest (cdr x)))
          (begin (display " . ") (w x)))))

;; Car/cdr combinations
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

;; List operations
(define (list . args) args)

(define (length lst)
  (if (null? lst) 0
      (b+ 1 (length (cdr lst)))))

(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define (for-each f lst)
  (if (null? lst) '()
      (begin (f (car lst)) (for-each f (cdr lst)))))
