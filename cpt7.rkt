
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
