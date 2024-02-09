;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 3

(define (rember-draft1 a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat)) (cdr lat))
        (else (cons (car lat)
                    (rember-draft1 a (cdr lat))))))


(equal?
 (rember-draft1 'mint '(lamb chops and mint jelly))
 '(lamb chops and jelly))

(equal?
 (rember-draft1 'mint '(lamb chops and mint flavored mint jelly))
 '(lamb chops and flavored mint jelly))

(equal?
 (rember-draft1 'toast '(bacon lettuce and tomato))
 '(bacon lettuce and tomato))

(equal?
 (rember-draft1 'cup '(coffee cup tea cup and hick cup))
 '(coffee tea cup and hick cup))

;; [[file:Commandments.org::*The Second Commandment][The Second Commandment (first draft)]]

(rember-draft1 'sauce '(soy sauce and tomato sauce))

(define (firsts llat)
  (cond ((null? llat) '())
        (else (cons (caar llat)
                    (firsts (cdr llat))))))

(equal?
 (firsts '((apple peach pumpkin)
           (plum pear cherry)
           (grape raisin pea)
           (bean carrot eggplant)))
 '(apple plum grape bean))

(equal?
 (firsts '((a b) (c d) (e f)))
 '(a c e))

(equal?
 (firsts '())
 '())

(equal?
 (firsts '((five plums)
           (four)
           (eleven green oranges)))
 '(five four eleven))

(equal?
 (firsts '(((five plums) four)
           (eleven green oranges)
           ((no) more)))
 '((five plums) eleven (no)))

;; [[file:Commandments.org::*The Third Commandment][The Third Commandment (first draft)]]

(define (insertR new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat)) (cons old
                                   (cons new
                                         (cdr lat))))
        (else (cons (car lat)
                    (insertR new old (cdr lat))))))
(equal?
 (insertR 'jalapeño 'and '(tacos tamales and salsa))
 '(tacos tamales and jalapeño salsa))

(equal?
 (insertR 'e 'd '(a b c d f g d h))
 '(a b c d e f g d h))

(define (insertL new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new lat))
        (else (cons (car lat)
                    (insertL new old (cdr lat))))))

(equal?
 (insertL 'e 'f '(a b c d f g d h))
 '(a b c d e f g d h))

(define (subst new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new (cdr lat)))
        (else (cons (car lat)
                    (subst new old (cdr lat))))))

(equal?
 (subst 'topping 'fudge '(ice cream with fudge for dessert))
 '(ice cream with topping for dessert))

(define (subst2 new o1 o2 lat)
  (cond ((null? lat) '())
        ((or (eq? o1 (car lat))
             (eq? o2 (car lat))) (cons new (cdr lat)))
        (else (cons (car lat)
                    (subst2 new o1 o2 (cdr lat))))))

(equal?
 (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
 '(vanilla ice cream with chocolate topping))

(define (multirember a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat))
         (multirember a (cdr lat)))
        (else (cons (car lat)
                    (multirember a (cdr lat))))))

(equal?
 (multirember 'cup '(coffee cup tea cup and hick cup))
 '(coffee tea and hick))

(define (multiinsertR new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat))
         (cons old
               (cons new
                     (multiinsertR new old (cdr lat)))))
        (else (cons (car lat)
                    (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat))
         (cons new
               (cons old
                     (multiinsertL new old (cdr lat)))))
        (else (cons (car lat)
                    (multiinsertL new old (cdr lat))))))

(equal?
 (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
 '(chips and fried fish or fried fish and fried))

;; [[file:Commandments.org::*The Fourth Commandment][The Fourth Commandment (preliminary)]]

(define (multisubst new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new
                                   (multisubst new old (cdr lat))))
        (else (cons (car lat)
                    (multisubst new old (cdr lat))))))
