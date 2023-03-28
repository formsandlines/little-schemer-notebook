
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
