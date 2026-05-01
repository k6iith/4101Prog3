;; Store away the Scheme48 definitios of +, -, *, /, =, <
(define s48-+ +)
(define s48-- -)
(define s48-* *)
(define s48-/ /)
(define s48-= =)
(define s48-< <)
(define s48-integer? integer?)
(define s48-rational? rational?)

;; It would be easier to use "and" in the code below, but that will
;; get redefined for the test code when loading undefine.scm.

(define (type-error msg val)
  (display " [ERROR: ")
  (display msg)
  (display "] ")
  val)

;; Define binary built-in functions from Project 2
(define (b+ x y)
  (if (s48-integer? x)
      (if (s48-integer? y)
	  (s48-+ x y)
	  (type-error "arguments of b+ are not integers" 0))
      (type-error "arguments of b+ are not integers" 0)))

(define (b- x y)
  (if (s48-integer? x)
      (if (s48-integer? y)
	  (s48-- x y)
	  (type-error "arguments of b- are not integers" 0))
      (type-error "arguments of b- are not integers" 0)))

(define (b* x y)
  (if (s48-integer? x)
      (if (s48-integer? y)
	  (s48-* x y)
	  (type-error "arguments of b* are not integers" 1))
      (type-error "arguments of b* are not integers" 1)))

(define (b/ x y)
  (if (s48-integer? x)
      (if (s48-integer? y)
	  (s48-/ x y)
	  (type-error "arguments of b/ are not integers" 1))
      (type-error "arguments of b/ are not integers" 1)))

(define (b= x y)
  (if (s48-integer? x)
      (if (s48-integer? y)
	  (s48-= x y)
	  (type-error "arguments of b= are not integers" #f))
      (type-error "arguments of b= are not integers" #f)))

(define (b< x y)
  (if (s48-integer? x)
      (if (s48-integer? y)
	  (s48-< x y)
	  (type-error "arguments of b< are not integers" #f))
      (type-error "arguments of b< are not integers" #f)))

;; n-ary minus
(define (- x . l)
  (s48-- x (apply s48-+ l)))

;; n-ary division
(define (/ x . l)
  (s48-/ x (apply s48-* l)))

;; The functions below are only used in the function test before
;; undefine.scm is loaded, so here using and is ok.  These functions
;; are not for use in ini.scm.

;; conversion functions for rational numbers
(define (s48-rational->s4101-rational x)
  (cond ((null? x) x)
	((s48-integer? x) x)
	((s48-rational? x) (list 'rational (numerator x) (denominator x)))
	((pair? x) (cons (s48-rational->s4101-rational (car x))
			 (s48-rational->s4101-rational (cdr x))))
	(else x)))

(define (s4101-rational? x)
  (if (and (pair? x)
	   (eq? (car x) 'rational)
	   (pair? (cdr x))
	   (pair? (cddr x))
	   (null? (cdddr x)))
      (let ((num (cadr x))
	    (den (caddr x)))
	(if (and (s48-integer? num) (s48-integer? den))
	    #t
	    #f))
      #f))

(define (s4101-rational->s48-rational x)
  (cond ((null? x) x)
	((s48-integer? x) x)
	((s4101-rational? x) (/ (cadr x) (caddr x)))
	((pair? x) (cons (s4101-rational->s48-rational (car x))
			 (s4101-rational->s48-rational (cdr x))))
	(else x)))
