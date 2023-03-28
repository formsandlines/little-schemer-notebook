
;; Chapter 4

;; All numbers are atoms
(atom? 14)

(add1 67) ;; in Racket stdlib

(sub1 5) ;; in Racket stdlib

(sub1 0) ;; => -1 (no answer according to the book)

(zero? 0) ;; => true (in Racket stdlib)

(zero? 1492) ;; => false

(+ 46 12) ;; => 58

(define (o+-iter n m)
  (cond ((zero? m) n)
        (else (o+-iter (add1 n)
                       (sub1 m)))))
(define (o+ n m)
  (cond ((zero? m) n)
        (else (add1
               (o+ n (sub1 m))))))

(let ((n 46) (m 12))
  (= (o+ n m)
     (o+-iter n m)
     (+ n m)))

(define (o--iter n m)
  (cond ((zero? m) n)
        (else (o--iter (sub1 n)
                       (sub1 m)))))

(define (o- n m)
  (cond ((zero? m) n)
        (else (sub1
               (o- n (sub1 m))))))

(let ((n 14) (m 3))
  (= (o- n m)
     (o--iter n m)
     (- n m)))

(let ((n 17) (m 9))
  (= (o- n m)
     (o--iter n m)
     (- n m)))

(let ((n 18) (m 25))
  (= (o- n m)
     (o--iter n m)
     (- n m)))
;=> -7 (no answer according to the book)


(define (tup? lat)
  (cond ((null? lat) #t)
        ((number? (car lat)) (tup? (cdr lat)))
        (else #f)))

(eq? (tup? '(2 11 3 79 47 6))
     #t)
(eq? (tup? '(8 55 5 555))
     #t)

(eq? (tup? '(1 2 8 apple 4 3))
     #f)
(eq? (tup? '(3 (7 4) 13 9))
     #f)

(eq? (tup? '())
     #t)

(define (addtup tup)
  (cond ((null? tup) 0)
        (else (o+ (car tup)
                  (addtup (cdr tup))))))

(= (addtup '(3 5 2 8))
   18)

(= (addtup '(15 6 7 12 3))
   43)

;; [[file:Commandments.org::*The First Commandment][The First Commandment (first revision)]]

(define (× n m)
  (cond ((zero? m) 0)
        (else (o+ n
                  (× n (sub1 m))))))

(= (× 5 3)
   (* 5 3)
   15)

(= (× 13 4)
   (* 13 4)
   52)

;; [[file:Commandments.org::*The Fourth Commandment][The Fourth Commandment (first revision)]]

(define (=* x . ys)
  (cond ((null? ys) #t)
        (else (and (= x (car ys))
                   (apply =* (car ys) (cdr ys))))))

(=* (× 12 3)
    (o+ 12 (× 12 2))
    (o+ 12 (o+ 12 (× 12 1)))
    (o+ 12 (o+ 12 (o+ 12 (× 12 0))))
    (o+ 12 (o+ 12 (o+ 12 0)))
    36)

;; [[file:Commandments.org::*The Fifth Commandment][The Fifth Commandment (1. draft)]]

;; (define (tup+ tup1 tup2)
;;   (cond ((and (null? tup1)
;;               (null? tup2)) '())
;;         ((null? tup1) (raise 'non-eq-lengths))
;;         ((null? tup2) (raise 'non-eq-lengths))
;;         (else (cons (o+ (car tup1)
;;                         (car tup2))
;;                     (tup+ (cdr tup1) (cdr tup2))))))

(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (o+ (car tup1)
                        (car tup2))
                    (tup+ (cdr tup1) (cdr tup2))))))

(equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
        '(11 11 11 11 11))

(equal? (tup+ '(2 3) '(4 6))
        '(6 9))

(equal? (tup+ '(3 7) '(4 6 8 1))
        '(7 13 8 1))
(equal? (tup+ '(3 7 8 1) '(4 6))
        '(7 13 8 1))


(define (o> n m)
  (cond ((zero? n) #f)
        ((zero? m) #t)
        (else (o> (sub1 n) (sub1 m)))))

(eqv? (o> 12 133)
      #f)

(eqv? (o> 120 11)
      #t)

(define (o< n m)
  (cond ((zero? m) #f)
        ((zero? n) #t)
        (else (o< (sub1 n) (sub1 m)))))

(eqv? (o< 8 3)
      #f)

(eqv? (o< 6 6)
      #f)

(define (o= n m)
  (cond ((and (zero? n)
              (zero? m)) #t)
        ((or (zero? n)
             (zero? m)) #f)
        (else (o= (sub1 n) (sub1 m)))))

;; alternative definitions:
(define (o=2 n m)
  (cond ((o> n m) #f)
        ((o< n m) #f)
        (else #t)))
(define (o=3 n m)
  (and (eqv? #f (o> n m))
       (eqv? #f (<o n m))))

(define (↑ n m)
  (cond ((zero? m) 1)
        (else (× n (↑ n (sub1 m))))))

(= (↑ 1 1) 1)
(= (↑ 2 3) 8)
(= (↑ 5 3) 125)

(define (÷ n m)
  (cond ((< n m) 0)
        (else (add1 (÷ (o- n m) m)))))

(÷ 6 2)
(÷ 9 3)
(÷ 12 3)

(= (÷ 15 4)
   (quotient 15 4)
   3)

(define (-length lat)
  (cond ((null? lat) 0)
        (else (add1 (-length (cdr lat))))))

(-length '(hotdogs with mustard sauerkraut and pickles))
(-length '(ham and cheese on rye))

(define (pick n lat)
  (cond ((zero? (sub1 n)) (car lat))
        (else (pick (sub1 n) (cdr lat)))))

(eq? (pick 4 '(lasagna spaghetti ravioli macaroni meatball))
     'macaroni)

;; (pick 0 '(a))  ;; exception!

(define (rempick-draft n lat)
  (cond ((zero? (sub1 n)) (cdr lat))
        (else (cons (car lat)
                    (rempick-draft (sub1 n) (cdr lat))))))

(equal? (rempick-draft 3 '(hotdogs with hot mustard))
        '(hotdogs with mustard))

(define (-number? x)
  (cond ((zero? x) #t)
        (else (-number? (sub1 x)))))

;; (-number? 'tomato) ;; not defined -> onumber is primitive function

(define (no-nums lat)
  (cond ((null? lat) '())
        (else (cond ((number? (car lat)) (no-nums (cdr lat)))
                    (else (cons (car lat)
                                (no-nums (cdr lat))))))))

(equal? (no-nums '(5 pears 6 prunes 9 dates))
        '(pears prunes dates))

(define (all-nums lat)
  (cond ((null? lat) '())
        (else (cond ((number? (car lat)) (cons (car lat)
                                               (all-nums (cdr lat))))
                    (else (all-nums (cdr lat)))))))

(equal? (all-nums '(5 pears 6 prunes 9 dates))
        '(5 6 9))

(define (eqan? a1 a2)
  (cond ((and (number? a1)
              (number? a2)) (= a1 a2))
        ((or (number? a1)
             (number? a2)) #f)
        (else (eq? a1 a2))))

(eq? (eqan? 1 'a) #f)
(eq? (eqan? 'a 1) #f)
(eq? (eqan? 1 1) #t)
(eq? (eqan? 'a 'a) #t)

;; Why does the book use `eq?` here instead of `eqan?`?
;; Why make the distinction if we now throw it out of the window?
(define (occur a lat)
  (cond ((null? lat) 0)
        ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat)))))

(= (occur 'a '(1 a 2 a 3))
   2)
(= (occur 2 '(1 a 2 a 3))
   1)
(= (occur 'a '(1 2 3))
   0)

(define (one?-draft n)
  (cond ((zero? n) #f)
        (else (zero? (sub1 n)))))

(define (one? n)
  (= 1 n))

(define (rempick n lat)
  (cond ((one? n) (cdr lat))
        (else (cons (car lat)
                    (rempick (sub1 n) (cdr lat))))))

(equal? (rempick 3 '(lemon meringue salty pie))
        '(lemon meringue pie))
