;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 6


;; the 'a, like other symbols, was unquoted in the book
;; however, in Racket, this would make it an undefined identifier
;; which cannot be referenced before definition and would also
;; not be the same as the quoted symbol
(let ((y 'a))
  (eq? (quote a) y)) ;=> #t

(let ((a +))
  (eq? '+ a)) ;=> #f

;; -----------------------------------------------------------------------------
;; previous (misguided) drafts of numbered:

;; (define (arithmetic-sym? x)
;;   (cond ((or (eq? x '+)
;;              (eq? x '×)
;;              (eq? x '↑)
;;              (number? x)) #t)
;;         (else #f)))

;; (define (numbered?-draft1 aexp)
;;   (cond ((null? aexp) #t)
;;         ((arithmetic-sym? (car aexp))
;;          (numbered?-draft1 (cdr aexp)))
;;         (else #f)))

;; (define (numbered?-draft2 aexp)
;;   (cond ((null? aexp) #t)
;;         ((atom? (car aexp))
;;          (cond ((arithmetic-sym? (car aexp)) (numbered?-draft2 (cdr aexp)))
;;                (else #f)))
;;         (else (and (numbered?-draft2 (car aexp))
;;                    (numbered?-draft2 (cdr aexp))))))

;; (define (numbered? aexp)
;;   (cond ((atom? aexp) (arithmetic-sym? aexp))
;;         (else (numbered-list? aexp))))

;; (define (numbered-list? aexp)
;;   (cond ((null? aexp) #t)
;;         ((numbered? (car aexp)) (numbered-list? (cdr aexp)))
;;         (else #f)))
;; -----------------------------------------------------------------------------

(define (operator? a)
  (cond ((or (eq? a '+)
             (eq? a '×)
             (eq? a '↑)) #t)
        (else #f)))

(define (numbered? aexp)
  (cond ((atom? aexp) (number? aexp))
        (else (and (null? (cdr (cdr (cdr aexp))))
                   (operator? (car (cdr aexp)))
                   (numbered? (car aexp))
                   (numbered? (car (cdr (cdr aexp))))))))

;; if we assume that `aexp` is a correct arithmetic expression:
(define (numbered?-simpler aexp)
  (cond ((atom? aexp) (number? aexp))
        (else (and (numbered?-simpler (car aexp))
                   (numbered?-simpler (car (cdr (cdr aexp))))))))

(eq? (numbered? '(3 + (4 ↑ 5)))
     #t)

(eq? (numbered? '(2 × sausage))
     #f)

(define (value nexp)
  (cond ((atom? nexp) nexp)
        ((eq? (car (cdr nexp)) '+) (o+ (value (car nexp))
                                       (value (car (cdr (cdr nexp))))))
        ((eq? (car (cdr nexp)) '×) (× (value (car nexp))
                                      (value (car (cdr (cdr nexp))))))
        (else (↑ (value (car nexp))
                 (value (car (cdr (cdr nexp))))))))

(= (value '(1 + (3 ↑ 4)))
   82)

;; No answer, because contract violation:
;; (= (value 'cookie)
;;    '?)

; [[file:Commandments.org::*The Seventh Commandment][The Seventh Commandment (first draft)]]

(define (operator nexp)
  (car nexp))

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (value nexp)
  (cond ((atom? nexp) nexp)
        ((eq? (operator nexp) '+) (o+ (value (1st-sub-exp nexp))
                                      (value (2nd-sub-exp nexp))))
        ((eq? (operator nexp) '×) (× (value (1st-sub-exp nexp))
                                     (value (2nd-sub-exp nexp))))
        (else (↑ (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp))))))

(= (value '(+ 1 (↑ 3 4)))
   82)

(define (value-sexp nexp)
  (eval nexp))

(= (value-sexp '(+ 1 (↑ 3 4)))
   82)

; [[file:Commandments.org::*The Eighth Commandment][The Eighth Commandment (first draft)]]

;; Different representation of numbers:
;; (() () () ())
;; (((())))
;; (I V)

;; 4 primitives for numbers:
;; - (number?)
;; - (zero?)
;; - (add1)
;; - (sub1)

(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(equal? (edd1 '())
        '(()))

(equal? (edd1 (edd1 '()))
        '(() ()))

(equal? (edd1 '(() ()))
        '(() () ()))

(define (zub1 n)
  (cdr n))

(equal? (zub1 '(()))
        '())

(equal? (zub1 '(() ()))
        '(()))

(equal? (zub1 (edd1 '()))
        '())

(define (e+ n m)
  (cond ((sero? m) n)
        (else (edd1 (e+ n (zub1 m))))))

(equal? (e+ '(() ()) '(() () ()))
        '(() () () () ()))

(equal? (e+ '() '())
        '())

(equal? (e+ '(()) '())
        '(()))

(equal? (e+ '() '(()))
        '(()))

;; representation of (1 2 3):
'((()) (() ()) (() () ()))

;; Although we use this representation of numbers just like atoms,
;; our abstraction leaks through in functions like `lat`,
;; which only work on “real” atoms, so we must beware of shadows.
