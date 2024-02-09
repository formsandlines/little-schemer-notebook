;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 5


(define (rember*-draft1 a l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eq? a (car l)) (rember*-draft1 a (cdr l)))
               (else (cons (car l)
                           (rember*-draft1 a (cdr l))))))
        (else (cons (rember*-draft1 a (car l))
                    (rember*-draft1 a (cdr l))))))

(equal? (rember*-draft1 'cup '((coffee) cup ((tea) cup)
                               (and (hick)) cup))
        '((coffee) ((tea)) (and (hick))))

(equal? (rember*-draft1 'sauce '(((tomato sauce))
                                 ((bean) sauce)
                                 (and ((flying)) sauce)))
        '(((tomato))
          ((bean))
          (and ((flying)))))

(eq? (lat? '(((tomato sauce))
             ((bean) sauce)
             (and ((flying)) sauce)))
     #f)

(define (insertR* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eq? old (car l)) (cons old
                                        (cons new
                                              (insertR* new old (cdr l)))))
               (else (cons (car l)
                           (insertR* new old (cdr l))))))
        (else (cons (insertR* new old (car l))
                    (insertR* new old (cdr l))))))

(equal? (insertR* 'roast 'chuck '((how much (wood))
                                  could
                                  ((a (wood) chuck))
                                  (((chuck)))
                                  (if (a) ((wood chuck)))
                                  could chuck wood))
        '((how much (wood))
          could
          ((a (wood) chuck roast))
          (((chuck roast)))
          (if (a) ((wood chuck roast)))
          could chuck roast wood))

;; [[file:Commandments.org::*The First Commandment][The First Commandment (final version)]]

;; [[file:Commandments.org::*The Fourth Commandment][The Fourth Commandment (final version)]]

(define (occur* a l)
  (cond ((null? l) 0)
        ((atom? (car l))
         (cond ((eq? (car l) a) (add1 (occur* a (cdr l))))
               (else (occur* a (cdr l)))))
        (else (o+ (occur* a (car l))
                  (occur* a (cdr l))))))

(= (occur* 'banana '((banana)
                     (split ((((banana ice)))
                             (cream (banana))
                             sherbet))
                     (banana)
                     (bread)
                     (banana brandy)))
   5)

(define (subst* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eq? old (car l)) (cons new
                                        (subst* new old (cdr l))))
               (else (cons (car l)
                           (subst* new old (cdr l))))))
        (else (cons (subst* new old (car l))
                    (subst* new old (cdr l))))))

(equal? (subst* 'orange 'banana '((banana)
                                  (split ((((banana ice)))
                                          (cream (banana))
                                          sherbet))
                                  (banana)
                                  (bread)
                                  (banana brandy)))
        '((orange)
          (split ((((orange ice)))
                  (cream (orange))
                  sherbet))
          (orange)
          (bread)
          (orange brandy)))

(define (insertL* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eq? old (car l)) (cons new
                                        (cons old
                                              (insertL* new old (cdr l)))))
               (else (cons (car l)
                           (insertL* new old (cdr l))))))
        (else (cons (insertL* new old (car l))
                    (insertL* new old (cdr l))))))

(equal? (insertL* 'pecker 'chuck '((how much (wood))
                                   could
                                   ((a (wood) chuck))
                                   (((chuck)))
                                   (if (a) ((wood chuck)))
                                   could chuck wood))
        '((how much (wood))
          could
          ((a (wood) pecker chuck))
          (((pecker chuck)))
          (if (a) ((wood pecker chuck)))
          could pecker chuck wood))

(define (member* a l)
  (cond ((null? l) #f)
        ((atom? (car l)) (or (eq? a (car l))
                             (member* a (cdr l))))
        (else (or (member* a (car l))
                  (member* a (cdr l))))))

(eq? (member* 'chips '((potato) (chips ((with) fish) (chips))))
     #t)

(eq? (member* 'chips '((potato) (dips ((with) fish) (dips))))
     #f)

(define (leftmost l)
  (cond ((atom? (car l)) (car l))
        (else (leftmost (car l)))))

(define (leftmost-nonempty l)
  (cond ((null? l) #f) ;; but list should be non-empty
        ((atom? (car l)) (car l))
        (else (or (leftmost-nonempty (car l))
                  (leftmost-nonempty (cdr l))))))

(eq? (leftmost '((potato) (chips ((with) fish) (chips))))
     'potato)

(eq? (leftmost '(((hot) (tuna (and))) cheese))
     'hot)

(eq? (leftmost-nonempty '(((() four)) 17 (seventeen)))
     'four) ;; no answer in leftmost

(eq? (leftmost-nonempty '())
     #f) ;; no answer in leftmost

(let ((x 'pizza)
      (l '(mozzarella pizza)))
  (and (atom? (car l))
       (eq? (car l) x)))

(let ((x 'pizza)
      (l '((mozzarella mushroom) pizza)))
  (and (atom? (car l))
       (eq? (car l) x)))

(let ((x 'pizza)
      (l '(pizza (tastes good))))
  (and (atom? (car l))
       (eq? (car l) x)))

;; `and` and `or` can be defined in terms of `cond`:
(define cand
  (lambda (a? b?)
    (cond (a? b?)
          (else #f))))

(define cor
  (lambda (a? b?)
    (cond (a? #t)
          (else b?))))

(cand #t #t) ;=> #t
(cand #t #f)
(cand #f #t)
(cand #f #f)

(cor #t #t) ;=> #t
(cor #t #f) ;=> #t
(cor #f #t) ;=> #t
(cor #f #f)

(define (eqlist?-draft1 l1 l2)
  (cond ((null? l1) (null? l2))
        ((null? l2) (null? l1))
        ((and (atom? (car l1))
              (atom? (car l2))) (and (eqan? (car l1) (car l2))
                                     (eqlist?-draft1 (cdr l1) (cdr l2))))
        ((or (atom? (car l1))
             (atom? (car l2))) #f)
        (else (and (eqlist?-draft1 (car l1) (car l2))
                   (eqlist?-draft1 (cdr l1) (cdr l2))))))

;; Both lists can appear in one of 9 cases:
;; ()    ()
;; ()    (a …)
;; (a …) ()
;; ()    (l …)
;; (l …) ()
;; (a …) (a …)
;; (l …) (a …)
;; (a …) (l …)
;; (l …) (l …)

;; alternative definition:
(define (eqlist?-draft2 l1 l2)
  (cond ((and (null? l1)
              (null? l2)) #t)
        ((or (null? l1)
             (null? l2)) #f)
        ((and (atom? (car l1))
              (atom? (car l2))) (and (eqan? (car l1) (car l2))
                                     (eqlist?-draft2 (cdr l1) (cdr l2))))
        ((or (atom? (car l1))
             (atom? (car l2))) #f)
        (else (and (eqlist?-draft2 (car l1) (car l2))
                   (eqlist?-draft2 (cdr l1) (cdr l2))))))

(eq? (eqlist?-draft1 '(strawberry ice cream) '(strawberry ice cream))
     #t)

(define (-equal? s1 s2)
  (cond ((and (atom? s1)
              (atom? s2)) (eqan? s1 s2))
        ((or (atom? s1)
             (atom? s2)) #f)
        (else (eqlist? s1 s2))))

(define (eqlist? l1 l2)
  (cond ((and (null? l1)
              (null? l2)) #t)
        ((or (null? l1)
             (null? l2)) #f)
        (else (and (-equal? (car l1) (car l2))
                   (eqlist? (cdr l1) (cdr l2))))))

(eq? (eqlist? '(strawberry ice cream) '(strawberry ice cream))
     #t)

; [[file:Commandments.org::*The Sixth Commandment][The Sixth Commandment (first draft)]]

(define (rember s l)
  (cond ((null? l) '())
        ((-equal? s (car l)) (cdr l))
        (else (cons (car l)
                    (rember s (cdr l))))))

(define (rember* s l)
  (cond ((null? l) '())
        ((-equal? s (car l)) (rember* s (cdr l)))
        ((atom? (car l)) (cons (car l)
                               (rember* s (cdr l))))
        (else (cons (rember* s (car l))
                    (rember* s (cdr l))))))

(equal? (rember* 'apple '((apple (cheese cake) ((apple) pie))))
        '(((cheese cake) (() pie))))
