;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 7

(define (-set?-draft1 lat)
  (lambda (aux set lat)
    (cond ((null? lat) #t)
          (else (and (not-contains? (car lat) set)
                     (aux (cons (car lat) set)
                          (cdr lat)))))))

(define (-set?-draft2 lat)
  (cond ((null? lat) #t)
        (else (and (= 1 (occur (car lat) lat))
                   (-set?-draft2 (cdr lat))))))

(define (-set?-draft3 lat)
  (cond ((null? lat) #t)
        (else (and (eq? #f (member? (car lat) (cdr lat)))
                   (-set?-draft3 (cdr lat))))))

(define (-set? lat)
  (cond ((null? lat) #t)
        ((member? (car lat) (cdr lat)) #f)
        (else (-set? (cdr lat)))))

(eq? (-set? '(apple peaches apple plum))
     #f)

(eq? (-set? '(apples peaches pears plums))
     #t)

(eq? (-set? '())
     #t)

(eq? (-set? '(apple 3 pear 4 9 apple 3 4))
     #f)

(define (makeset-alt1 lat)
  (cond ((null? lat) '())
        ((member? (car lat) (cdr lat)) (makeset-alt1 (cdr lat)))
        (else (cons (car lat)
                    (makeset-alt1 (cdr lat))))))

;; order will be different in this approach (doesn’t matter in sets)
(equal? (makeset-alt1 '(apple peach pear peach
                        plum apple lemon peach))
        '(pear plum apple lemon peach))

(define (makeset lat)
  (cond ((null? lat) '())
        (else (cons (car lat)
                    (makeset (multirember (car lat) (cdr lat)))))))

(equal? (makeset '(apple peach pear peach
                   plum apple lemon peach))
        '(apple peach pear plum lemon))

(equal? (makeset '(apple 3 pear 4 9 apple 3 4))
        '(apple 3 pear 4 9))

(define (-subset-alt1 set1 set2)
  (cond ((null? set1) #t)
        ((member? (car set1) set2) (-subset-alt1 (cdr set1) set2))
        (else #f)))

(define (-subset? set1 set2)
  (cond ((null? set1) #t)
        (else (and (member? (car set1) set2)
                   (-subset? (cdr set1) set2)))))

(eq? (-subset? '(5 chicken wings)
               '(5 hamburgers
                 2 pieces of fried chicken and light duckling wings))
     #t)

(eq? (-subset? '(4 pounds of horseradish)
               '(four pounds chicken and
                 5 ounces horseradish))
     #f)

(define (eqset? set1 set2)
  (and (-subset? set1 set2)
       (-subset? set2 set1)))

(eq? (eqset? '(6 large chickens with wings)
             '(6 chickens with large wings))
     #t)

(define (intersect?-alt1 set1 set2)
  (cond ((or (null? set1)
             (null? set2)) #f)
        ((member? (car set1) set2) #t)
        (else (intersect?-alt1 (cdr set1) set2))))

(define (intersect? set1 set2)
  (cond ((or (null? set1)
             (null? set2)) #f)
        (else (or (member? (car set1) set2)
                  (intersect? (cdr set1) set2)))))

(eq? (intersect? '(stewed tomatoes and macaroni)
                 '(macaroni and cheese))
     #t)

(define (intersect set1 set2)
  (cond ((null? set1) '())
        ((member? (car set1) set2) (cons (car set1)
                                         (intersect (cdr set1) set2)))
        (else (intersect (cdr set1) set2))))

(equal? (intersect '(stewed tomatoes and macaroni)
                   '(macaroni and cheese))
        '(and macaroni))

(define (union-alt1 set1 set2)
  (makeset (append set1 set2)))

(define (union-alt2 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1)
                    (union-alt2 (cdr set1)
                                (rember (car set1) set2))))))

(equal? (union-alt2 '(stewed tomatoes and macaroni casserole)
                    '(macaroni and cheese))
        '(stewed tomatoes and macaroni casserole cheese))

(define (union set1 set2)
  (cond ((null? set1) set2)
        ((member? (car set1) set2) (union (cdr set1) set2))
        (else (cons (car set1)
                    (union (cdr set1) set2)))))

(equal? (union '(stewed tomatoes and macaroni casserole)
               '(macaroni and cheese))
        '(stewed tomatoes casserole macaroni and cheese))

(define (difference set1 set2)
  (cond ((null? set1) '())
        ((member? (car set1) set2) (difference (cdr set1) set2))
        (else (cons (car set1)
                    (difference (cdr set1) set2)))))

(equal? (difference '(stewed tomatoes and macaroni casserole)
                    '(macaroni and cheese))
        '(stewed tomatoes casserole))

(define (intersectall-alt1 l-set)
  (cond ((null? l-set) '())
        ((null? (cdr l-set)) (car l-set))
        (else (intersectall-alt1
               (cons (intersect (car l-set) (cadr l-set))
                     (cddr l-set))))))

(define (intersectall l-set)
  (cond ((null? (cdr l-set)) (car l-set))
        (else (intersect
               (car l-set)
               (intersectall (cdr l-set))))))

(let ((l-set '((a b c) (c a d e) (e f g h a b)))
      (res   '(a)))
  (and (equal? (intersectall l-set) res)
       (equal? (intersectall-alt1 l-set) res)))

(let ((l-set '((6 pears and)
               (3 peaches and 6 peppers)
               (8 pears and 6 plums)
               (and 6 prunes with some apples)))
      (res   '(6 and)))
  (and (equal? (intersectall l-set) res)
       (equal? (intersectall-alt1 l-set) res)))

(define (a-pair? x)
  (cond ((or (atom? x)
             (null? x)
             (null? (cdr x))) #f)
        (else (null? (cdr (cdr x))))))

(eq? (a-pair? 'a) #f)
(eq? (a-pair? '()) #f)
(eq? (a-pair? '(a)) #f)
(eq? (a-pair? '(a b c)) #f)
(eq? (a-pair? '(a b)) #t)
(eq? (a-pair? '(() #f)) #t)

;; called “build” in the book, but I think this name is too generic
(define (pair s1 s2)
  (cons s1 (cons s2 '())))

;; update: actually it makes naming params as pairs easier this way:
(define build pair)

;; called “first” and “second” in the book, but these are already defined
(define (fst p) (car p))
(define (snd p) (car (cdr p)))

(let ((p (pair 'x 'y)))
  (cons (fst p) (cons (snd p) '())))

(define (trd p) (car (cdr (cdr p))))

;; ! fails -> needs to check for set
(define (rel? l)
  (cond ((null? l) #t)
        (else (and (pair? (car l))
                   (rel? (cdr l))))))

(eq? (rel? '(apples peaches pumpkin pie))
     #f)

(eq? (rel? '((apples peaches)
             (pumpkin pie)
             (apples peaches)))
     #f)

(eq? (rel? '((apples peaches)
             (pumpkin pie)))
     #t)

(eq? (rel? '((4 3) (4 2) (7 6) (6 2) (3 4)))
     #t)

(define (firsts rel)
  (cond ((null? rel) '())
        (else (cons (fst (car rel))
                    (firsts (cdr rel))))))

(equal? (firsts '((4 3) (4 2) (7 6) (6 2) (3 4)))
        '(4 4 7 6 3))

;; think about functions as relations with uniqueness property
(define (fun? rel)
  (-set? (firsts rel)))

(eq? (fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
     #f)

(eq? (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
     #t)

(eq? (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
     #f)


(define (revpair p)
  (pair (snd p) (fst p)))

(equal? (revpair '(a b))
        '(b a))

(define (revrel rel)
  (cond ((null? rel) '())
        (else (cons (revpair (car rel))
                    (revrel (cdr rel))))))

(equal? (revrel '((8 a) (pumpkin pie) (got sick)))
        '((a 8) (pie pumpkin) (sick got)))


(define (seconds rel)
  (cond ((null? rel) '())
        (else (cons (snd (car rel))
                    (seconds (cdr rel))))))

(equal? (seconds '((4 3) (4 2) (7 6) (6 2) (3 4)))
        '(3 2 6 2 4))

;; defined for functions, therefore, no need to check for `fun?`!
(define (fullfun? fun)
  (-set? (seconds fun)))

(eq? (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
     #f)

(eq? (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
     #t)

(eq? (fullfun? '((grape raisin)
                 (plum prune)
                 (stewed prune)))
     #f)

(eq? (fullfun? '((grape raisin)
                 (plum prune)
                 (stewed grape)))
     #t)

;; equivalent alternative to `fullfun?`
(define (one-to-one? fun)
  (fun? (revrel fun)))

(eq? (one-to-one? '((chocolate chip) (doughy cookie)))
     #t)
