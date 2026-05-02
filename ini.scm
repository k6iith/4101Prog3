(define (integer? x) (s48-integer? x))

(define (rational? x)
  (or (integer? x)
      (and (pair? x)
	   (eq? (car x) 'rational)
	   (pair? (cdr x))
	   (integer? (cadr x))
	   (pair? (cddr x))
	   (integer? (caddr x))
	   (null? (cdddr x)))))

(define (number? x) (rational? x))

(define (numerator x)
  (cond ((integer? x) x)
	((rational? x) (cadr x))
	(else (error "numerator: not a number" x))))

(define (denominator x)
  (cond ((integer? x) 1)
	((rational? x) (caddr x))
	(else (error "denominator: not a number" x))))

;; Helper for integer absolute value to avoid recursion in gcd
(define (b-abs x)
  (if (b< x 0) (b- 0 x) x))

(define (gcd a b)
  (let ((a (b-abs a))
	(b (b-abs b)))
    (if (b= b 0) a
	(gcd b (remainder a b)))))

(define (lcm a b)
  (if (or (b= a 0) (b= b 0)) 0
      (b-abs (b* (quotient a (gcd a b)) b))))

(define (quotient a b) (s48-quotient a b))
(define (remainder a b) (s48-remainder a b))

(define (simplify-rational n d)
  (if (b= d 0) (error "Division by zero")
      (let ((g (gcd n d)))
	(let ((n1 (quotient n g))
	      (d1 (quotient d g)))
	  (let ((n2 (if (b< d1 0) (b- 0 n1) n1))
		(d2 (if (b< d1 0) (b- 0 d1) d1)))
	    (if (b= d2 1) n2
		(list 'rational n2 d2)))))))

;; Redefine arithmetic
(define (add2 x y)
  (simplify-rational (b+ (b* (numerator x) (denominator y))
			 (b* (numerator y) (denominator x)))
		     (b* (denominator x) (denominator y))))

(define (+ . l)
  (if (null? l) 0
      (let loop ((res (car l)) (rest (cdr l)))
	(if (null? rest) res
	    (loop (add2 res (car rest)) (cdr rest))))))

(define (mul2 x y)
  (simplify-rational (b* (numerator x) (numerator y))
		     (b* (denominator x) (denominator y))))

(define (* . l)
  (if (null? l) 1
      (let loop ((res (car l)) (rest (cdr l)))
	(if (null? rest) res
	    (loop (mul2 res (car rest)) (cdr rest))))))

(define (sub2 x y)
  (simplify-rational (b- (b* (numerator x) (denominator y))
			 (b* (numerator y) (denominator x)))
		     (b* (denominator x) (denominator y))))

(define (- x . l)
  (if (null? l)
      (simplify-rational (b- 0 (numerator x)) (denominator x))
      (let loop ((res x) (rest l))
	(if (null? rest) res
	    (loop (sub2 res (car rest)) (cdr rest))))))

(define (div2 x y)
  (simplify-rational (b* (numerator x) (denominator y))
		     (b* (denominator x) (numerator y))))

(define (/ x . l)
  (if (null? l)
      (simplify-rational (denominator x) (numerator x))
      (let loop ((res x) (rest l))
	(if (null? rest) res
	    (loop (div2 res (car rest)) (cdr rest))))))

;; Comparisons
(define (num= x y)
  (b= (b* (numerator x) (denominator y))
      (b* (numerator y) (denominator x))))

(define (= x y . l)
  (if (null? l) (num= x y)
      (and (num= x y) (apply = (cons y l)))))

(define (num< x y)
  (b< (b* (numerator x) (denominator y))
      (b* (numerator y) (denominator x))))

(define (< x y . l)
  (if (null? l) (num< x y)
      (and (num< x y) (apply < (cons y l)))))

(define (> x y . l)
  (if (null? l) (num< y x)
      (and (num< y x) (apply > (cons y l)))))

(define (<= x y . l)
  (if (null? l) (or (num= x y) (num< x y))
      (and (<= x y) (apply <= (cons y l)))))

(define (>= x y . l)
  (if (null? l) (or (num= x y) (num< y x))
      (and (>= x y) (apply >= (cons y l)))))

(define (zero? x) (= x 0))
(define (positive? x) (< 0 x))
(define (negative? x) (< x 0))

(define (abs x)
  (if (negative? x) (- x) x))

(define (eqv? x y)
  (cond ((and (number? x) (number? y)) (= x y))
	(else (eq? x y))))

(define (equal? x y)
  (cond ((pair? x) (and (pair? y)
			(equal? (car x) (car y))
			(equal? (cdr x) (cdr y))))
	((and (number? x) (number? y)) (= x y))
	(else (eq? x y))))

(define (assq obj alist)
  (if (null? alist) #f
      (if (eq? obj (caar alist)) (car alist)
	  (assq obj (cdr alist)))))

(define (assv obj alist)
  (if (null? alist) #f
      (if (eqv? obj (caar alist)) (car alist)
	  (assv obj (cdr alist)))))

(define (assoc obj alist)
  (if (null? alist) #f
      (if (equal? obj (caar alist)) (car alist)
	  (assoc obj (cdr alist)))))

(define (w x)
  (cond ((rational? x)
	 (if (integer? x) (write x)
	     (begin (write (numerator x)) (display "/") (write (denominator x)))))
	((pair? x)
	 (display "(")
	 (w (car x))
	 (let loop ((l (cdr x)))
	   (cond ((null? l) (display ")"))
		 ((pair? l) (display " ") (w (car l)) (loop (cdr l)))
		 (else (display " . ") (w l) (display ")")))))
	(else (write x))))

;; Copy over other definitions from original ini.scm
(define (not b) (if b #f #t))
(define (and x y) (if x y #f))
(define (or x y) (if x #t y))

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

(define (list . l) l)

(define (length l)
  (if (null? l) 0
      (b+ 1 (length (cdr l)))))

(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map f (cdr l)))))

(define (for-each f l)
  (if (null? l) '()
      (begin (f (car l)) (for-each f (cdr l)))))

(define (eof-object? x)
  (eq? x 'end-of-file))
