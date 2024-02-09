;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 2

;; list of atoms
(define (lat? l)
  (cond ((null? l) #t)
        ((atom? (car l)) (lat? (cdr l)))
        (else #f)))

(lat? '(Jack Sprat could eat no chicken fat))

(lat? '((Jack) Sprat could eat no chicken fat))

(lat? '(Jack (Sprat could) eat no chicken fat))

(lat? '())

(or (null? '()) (atom? '(d e f g)))

(or (null? '(a b c)) (null? '()))

(or (null? '(a b c)) (null? '(atom)))

(define (member? a lat)
  (cond ((null? lat) #f)
        (else (or (eq? a (car lat))
                  (member? a (cdr lat))))))

(member? 'tea '(coffee tea or milk))

(member? 'poached '(fried eggs and scrambled eggs))

;; [[file:Commandments.org::*The First Commandment][The First Commandment (preliminary)]]

(member? 'meat '(meat gravy))

(member? 'meat '(and meat gravy))

(member? 'meat '(potatoes and meat gravy))

(member? 'meat '(mashed potatoes and meat gravy))

(member? 'liver '())
