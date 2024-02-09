;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 8

;; first draft, not very useful
(define (rember-f test? a l)
  (cond ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    (rember-f test? a (cdr l))))))

(equal? (rember-f = 5 '(6 2 5 3))
        '(6 2 3))

(equal? (rember-f eq? 'jelly '(jelly beans are good))
        '(beans are good))

(equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
        '(lemonade and (cake)))

;; currying!
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(eq? (eq?-salad 'salad) #t)
(eq? (eq?-salad 'tuna) #f)

(eq? ((eq?-c 'salad) 'salad) #t)
(eq? ((eq?-c 'salad) 'tuna) #f)

;; writing it like this lets us define more specific functions
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? a (car l)) (cdr l))
            (else (cons (car l)
                        ((rember-f test?) a (cdr l))))))))

(equal? ((rember-f =) 5 '(6 2 5 3))
        '(6 2 3))

;; like this one
(define rember-eq? (rember-f eq?))

(equal? (rember-eq? 'tuna '(tuna salad is good))
        '(salad is good))

(equal? ((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
        '(shrimp salad and salad))

(equal? ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))
        '(equal? eqan? eqlist? eqpair?))


(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? old (car l)) (cons new l))
            (else (cons (car l)
                        ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? old (car l)) (cons old (cons new (cdr l))))
            (else (cons (car l)
                        ((insertR-f test?) new old (cdr l))))))))

;; first draft (takes a boolean to switch to left insertion)
(define insert-g
  (lambda (left?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((eq? old (car l))
             (cond (left? (cons new l))
                   (else (cons old (cons new (cdr l))))))
            (else (cons (car l)
                        ((insert-g left?) new old (cdr l))))))))

(equal? ((insert-g #t) 'coffee 'cake '(get cake and relax))
        '(get coffee cake and relax))

;; however, we can make it take arbitrary functions to build the new seq
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) '())
            ((eq? old (car l)) (seq new old (cdr l)))
            (else (cons (car l)
                        ;; may be inefficient having to call the outer function
                        ;; but how to recur on an anonymous lambda?
                        ((insert-g seq) new old (cdr l))))))))

(equal? ((insert-g seqL) 'coffee 'cake '(get cake and relax))
        '(get coffee cake and relax))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

;; this is better, since it avoids unnecessary mental load for naming
;; since the functions are not used elsewhere
(define insertL (insert-g (lambda (new old l)
                            (cons new (cons old l)))))
(define insertR (insert-g (lambda (new old l)
                            (cons old (cons new l)))))

(define subst (insert-g (lambda (new old l)
                          (cons new l))))

(equal? (subst 'corn 'rice '(curry with rice tastes good))
        '(curry with corn tastes good))

;; like insertL/R, can be defined anonymously to avoid redundancy
(define seqrem
  (lambda (new old l)
    l))

;; [[file:Commandments.org::*The Ninth Commandment][The Ninth Commandment (first draft)]]

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(equal? (rember 'sausage '(pizza with sausage and bacon))
        '(pizza with and bacon))

(define atom-to-function
  (lambda (x)
    (cond ((eq? x '+) +)
          ((eq? x '×) ×)
          (else ↑))))

(= ((atom-to-function '+) 3 2) 5)
(= ((atom-to-function '×) 3 2) 6)

(eq? (atom-to-function (operator '(+ 5 3))) +)

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else ((atom-to-function (operator nexp))
                 (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multirember-f test?) a (cdr lat))))))))

(equal? ((multirember-f =) 5 '(5 6 1 2 5 3))
        '(6 1 2 3))

(equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
        '(shrimp salad salad and))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) '())
          ((test? (car lat)) (multiremberT test? (cdr lat)))
          (else (cons (car lat)
                      (multiremberT test? (cdr lat)))))))

(equal? (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
        '(shrimp salad salad and))

;; builds and returns two lists as arguments to `col` (-> “collector”):
;; remaining-list and removed-list
(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col '() '()))
          ((eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car lat) seen)))))
          (else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat) newlat)
                                  seen)))))))

;; can use the lists to build a pair or do something else with them
(equal? (multirember&co 'tuna '(shrimp salad tuna salad and tuna)
                        pair)
        '((shrimp salad salad and)
          (tuna tuna)))


(define a-friend
  (lambda (x y)
    (null? y)))

(equal? (multirember&co 'tuna '(strawberry tuna and swordfish)
                        a-friend)
        #f)

(equal? (multirember&co 'tuna '() a-friend)
        #t)

(equal? (multirember&co 'tuna '(tuna) a-friend)
        #f)

;; this function demonstrates how the last collector uses the one that
;; is being passed to `multirember&co` and prepends `tuna` to `y`, to
;; show what happened in our previous test
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
              (cons 'tuna seen))))

(equal? (multirember&co 'tuna '(and tuna) a-friend)
        #f)

;; yet another demonstration to show what else happened in the new test
;; when `and` is added to `newlat` in the last collector
(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat)
              seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(equal? (multirember&co 'tuna '(strawberries tuna and swordfish)
                        last-friend)
        3)

;; [[file:Commandments.org::*The Tenth Commandment][The Tenth Commandment (first draft)]]

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? oldL (car lat))
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? oldR (car lat))
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertLR new oldL oldR (cdr lat)))))))

(equal? (multiinsertLR 'corn 'apple 'cherry '(new apple cherry juice with beans))
        '(new corn apple cherry corn juice with beans))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? oldL (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat countL countR)
                               (col (cons new (cons oldL newlat))
                                    (add1 countL)
                                    countR))))
          ((eq? oldR (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat countL countR)
                               (col (cons oldR (cons new newlat))
                                    countL
                                    (add1 countR)))))
          (else (multiinsertLR&co new oldL oldR (cdr lat)
                                  (lambda (newlat countL countR)
                                    (col (cons (car lat) newlat)
                                         countL
                                         countR)))))))

(define col (lambda (l n m)
              (list l n m)))

(equal? (multiinsertLR&co 'corn 'apple 'cherry
                          '(new apple cherry juice with beans)
                          col)
        '((new corn apple cherry corn juice with beans)
          1 1))

(equal? (multiinsertLR&co 'cranberries 'fish 'chips
                          '()
                          col)
        '(() 0 0))

(equal? (multiinsertLR&co 'salty 'fish 'chips
                          '(chips and fish or fish and chips)
                          col)
        '((chips salty and salty fish or salty fish and chips salty)
          2 2))

;; // ASIDE //
;; trying to understand continuations

;; Functions take the continuation as an argument and call it with
;; their return value.
(define (add-cont a b cont)
  (cont (+ a b)))

(define (mul-cont a b cont)
  (cont (* a b)))

;; The continuation is a representation of the control state of the program
;; at a particular point in its execution.
;; It includes the current values and the next instructions to be executed.
(add-cont
 3 4
 (lambda (sum)
   (mul-cont
    sum 5
    (lambda (prod)
      prod))))


;; So in the case of `multirember&co`, the continuation is the `col` argument
;; which is passed on in each recursive step and called in the final step
;; with the base values of the recursion.
;;
;; At each step, however, the previous continuation is enclosed in a new one
;; which adds an item to either one of its argument lists and passes those on
;; to the previous continuation.
;; Apparently, this mechanism is called a “collector”, which captures and
;; aggregates multiple continuations into one single data structure.

;; example:
(multirember&co 'pie '(apple pie tree) pair)
;=> ((apple tree) (pie))

;; Internally, each continuation will be wrapped/captured in a new continuation
;; which builds up a collector which is finally called with two empty lists.
;; Those would be the first arguments onto which elements may be consed on
;; at each step to build `newlat` and `seen`, which are then passed to the
;; original continuation passed by the caller.
((lambda (newlat seen)
   ((lambda (newlat seen)
      ((lambda (newlat seen)
         (pair
          (cons 'apple newlat) seen))
       newlat (cons 'pie seen)))
    (cons 'tree newlat) seen))
 '() '())

;; I am actually unsure if this is a correct explanation.

;; // END ASIDE //


(define -even?
  (lambda (n)
    (= (× (÷ n 2) 2) n)))

(define all-even?*
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (and (-even? (car l))
                                (all-even?* (cdr l))))
          (else (and (all-even?* (car l))
                     (all-even?* (cdr l)))))))

(eq? (all-even?* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
     #f)

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((-even? (car l)) (cons (car l)
                                         (evens-only* (cdr l))))
                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))))))

(equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
        '((2 8) 10 (() 6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 1 0))
          ((atom? (car l))
           (cond ((-even? (car l))
                  (evens-only*&co (cdr l)
                                  (lambda (evens even-prod odd-sum)
                                    (col (cons (car l) evens)
                                         (× (car l) even-prod)
                                         odd-sum))))
                 (else (evens-only*&co (cdr l)
                                       (lambda (evens even-prod odd-sum)
                                         (col evens
                                              even-prod
                                              (o+ (car l) odd-sum)))))))
          (else (evens-only*&co
                 (car l)
                 (lambda (evens even-prod odd-sum)
                   (evens-only*&co (cdr l)
                                   (lambda (p-evens p-even-prod p-odd-sum)
                                     ;; p -> parent list
                                     (col (cons evens p-evens)
                                          (× even-prod p-even-prod)
                                          (o+ odd-sum p-odd-sum))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

(equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                        the-last-friend)
        '(38 1920 (2 8) 10 (() 6) 2))
