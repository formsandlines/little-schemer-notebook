
;; Chapter 7

(define (oset?-draft1 lat)
  (lambda (aux set lat)
    (cond ((null? lat) #t)
          (else (and (not-contains? (car lat) set)
                     (aux (cons (car lat) set)
                          (cdr lat)))))))

(define (oset?-draft2 lat)
  (cond ((null? lat) #t)
        (else (and (= 1 (occur (car lat) lat))
                   (oset?-draft2 (cdr lat))))))

(define (oset?-draft3 lat)
  (cond ((null? lat) #t)
        (else (and (eq? #f (member? (car lat) (cdr lat)))
                   (oset?-draft3 (cdr lat))))))

(define (oset? lat)
  (cond ((null? lat) #t)
        ((member? (car lat) (cdr lat)) #f)
        (else (oset? (cdr lat)))))

(eq? (oset? '(apple peaches apple plum))
     #f)

(eq? (oset? '(apples peaches pears plums))
     #t)

(eq? (oset? '())
     #t)

(eq? (oset? '(apple 3 pear 4 9 apple 3 4))
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
