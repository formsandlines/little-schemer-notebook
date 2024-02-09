;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 10


;; Entries
;; - pair of lists of equal length
;; - first list must be a set

(build (makeset '(a b c))
       '(1 2 3))

(define new-entry
  build)

(new-entry '(appetizer entrée beverage)
           '(paté boeuf vin))

(new-entry '(appetizer entrée beverage)
           '(beer beer beer))

(new-entry '(beverage dessert)
           '((food is) (number one with us)))


;; own attempt
(define lookup-in-entry
  (lambda (name entry not-found)
    (cond ((null? (fst entry)) not-found)
          ((eq? (car (fst entry)) name) (car (snd entry)))
          (else
           (lookup-in-entry name (new-entry (cdr (fst entry))
                                            (cdr (snd entry))) not-found)))))

(eq? (lookup-in-entry 'entrée '((appetizer entrée beverage)
                                (food tastes good)) 'nil)
     'tastes)

(eq? (lookup-in-entry 'desert '((appetizer entrée beverage)
                                (food tastes good)) 'nil)
     'nil)

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? name (car names)) (car values))
          (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (fst entry) (snd entry) entry-f)))

(eq? (lookup-in-entry 'entrée '((appetizer entrée beverage)
                                (food tastes good))
                      (lambda (name) `(not-found ,name)))
     'tastes)

(equal? (lookup-in-entry 'desert '((appetizer entrée beverage)
                                   (food tastes good))
                         (lambda (name) `(not-found ,name)))
        '(not-found desert))


;; Tables
;; - list of entries

'() ;; <- empty table

'(((appetizer entrée beverage)
   (paté boeuf vin))
  ((beverage dessert)
   ((food is) (number one with us))))

'(((a b c)
   (1 2 3))
  ((x y)
   (p q)))

(define extend-table
  cons)

;; the anonymous function passed to `lookup-in-entry` here works as a
;; continuation when `name` is not found in the current entry
;; - this enables us to continue recursion without the need for a local
;;   binding of the result of `lookup-in-entry`
(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
          (else
           (lookup-in-entry name (car table)
                            (lambda (name)
                              (lookup-in-table name (cdr table) table-f)))))))

(equal? (lookup-in-table 'entrée
                         '(((entrée dessert)
                            (spaghetti spumoni))
                           ((appetizer entrée beverage)
                            (food tastes good)))
                         (lambda (name) `(not-found ,name)))
        'spaghetti)

(equal? (lookup-in-table 'z
                         '(((a b c)
                            (1 2 3))
                           ((x y)
                            (p q)))
                         (lambda (name) `(not-found ,name)))
        '(not-found z))


;; Examples of s-expressions and their values in our Scheme-in-Scheme:

'(equal? (cons 'car
               (cons (cons 'quote
                           (cons
                            (cons 'a
                                  (cons 'b
                                        (cons 'c
                                              (quote ()))))
                            (quote ())))
                     (quote ())))
  '(car (quote (a b c))))

;; will evaluate `car`, then needs to eval `quote` as well:
'(eq? (value '(car (quote (a b c))))
  'a)

;; will evaluate `quote` first, leaving the rest untouched:
'(equal? (value '(quote (car (quote (a b c)))))
  '(car (quote (a b c))))

'(= (value '(add1 6)) '7)

'(= (value '6) '6)

;; an undefined symbol (not found in env) has no value
'(eq? (value '(quote nothing)) ?)
'(eq? (value 'nothing) ?)

;; functions are evaluated after the argument gets eval’d
'(equal? (value '((lambda (nothing)
                    (cons nothing (quote ())))
                  (quote
                   (from nothing comes something))))
  '((from nothing comes something)))

'(eq? (value '((lambda (nothing)
                 (cond
                  (nothing (quote something))
                  (else (quote nothing))))
               #t))
  'something)

;; built-in functions like `car` evaluate to their Scheme primitives
'(equal? (value 'car) '(primitive car))


;; Expression types
;; - symbols/numbers/booleans referring to Scheme primitives are type `*const`
;; - symbols referring to the environment are type `*identifier`
;; - special forms are typed by their symbol name
;; - applications are type `*application`

'(eq? (type '6) '*const)

'(eq? (type '#f) '*const)
;; booleans evaluate to themselves
'(eq? (value '#f) #f)

;; type of symbols from built-in functions is `*const`
'(eq? (type 'cons) '*const)

;; type of an unknown symbol is `*identifier`
'(eq? (type 'nothing) '*identifier)

'(eq? (type '(quote nothing)) '*quote)

'(eq? (type '(lambda (x y) (cons x y)))
  '*lambda)

'(eq? (type '((lambda (nothing)
                (cond
                 (nothing (quote something))
                 (else (quote nothing))))
              #t))
  '*application)

'(eq? (type '(cond
              (nothing (quote something))
              (else (quote nothing))))
  '*cond)


;; We represent types as functions (“actions”)
;; - the meaning of an expression is what it evaluates to (what it “does”)
;; - `value` finds the type of the input expression
;;   and chooses the action that is associated with that type
;; - the action returns the value of the expression, given an environment

(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          (else (build 'primitive e)))))

;; `(quote e)` is always a pair
(define text-of snd)
;; `e` is a list/pair so no need to call `quote`
(define *quote
  (lambda (e table)
    (text-of e)))

(define initial-table
  (lambda (name)
    (car '())))
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))
;; help functions:
(define table-of fst)
(define formals-of snd)
(define body-of trd)

'(equal? (meaning '(lambda (x) (cons x y))
                  (((y z) ((8) 9))))
  '(non-primitive
    ((((y z) ((8) 9))) (x) (cons x y))))

(define question-of fst)
(define answer-of snd)
(define else?
  (lambda (x) (cond ((atom? x) (eq? 'else x))
                    (else #f))))
(define evcon
  (lambda (lines table)
    (cond ((else?   (question-of (car lines)))
           (meaning (answer-of (car lines)) table))
          ((meaning (question-of (car lines)) table)
           (meaning (answer-of (car lines)) table))
          (else (evcon (cdr lines) table)))))

(define cond-lines-of cdr)
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

'(equal? (*cond '(cond (coffee klatsch) (else party))
                '(((coffee) (#t))
                  ((klatsch party) (5 (6)))))
  5)

;; kinds of functions:
;; - primitive -> (primitive <primitive-name>)
;; - non-primitive -> (non-primitive (<table> <formals> <body>))
;;   - the list (<table> <formals> <body>) is called a closure record

(define -primitive?
  (lambda (e)
    (eq? (fst e) 'primitive)))

(define non-primitive?
  (lambda (e)
    (eq? (fst e) 'non-primitive)))

(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name 'cons)    (cons (fst vals) (snd vals)))
          ((eq? name 'car)     (car (fst vals)))
          ((eq? name 'cdr)     (cdr (fst vals)))
          ((eq? name 'null?)   (null? (fst vals)))
          ((eq? name 'eq?)     (eq? (fst vals) (snd vals)))
          ((eq? name 'atom?)   (atom? (fst vals)))
          ((eq? name 'zero?)   (zero? (fst vals)))
          ((eq? name 'add1)    (add1 (fst vals)))
          ((eq? name 'sub1)    (sub1 (fst vals)))
          ((eq? name 'number?) (number? (fst vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))

;; approximation of `apply` in Scheme/Lisp
(define -apply
  (lambda (fun vals)
    (cond ((-primitive? fun)    (apply-primitive (snd fun) vals))
          ((non-primitive? fun) (apply-closure (snd fun) vals)))))

(equal? (apply-closure '((((u v w) (1 2 3))
                          ((x y z) (4 5 6)))
                         (x y)
                         (cons z x))
                       '((a b c) (d e f)))
        ;; new table:
        ;; '(((x y)
        ;;    ((a b c) (d e f)))
        ;;   ((u v w)
        ;;    (1 2 3))
        ;;   ((x y z)
        ;;    (4 5 6)))
        ;; evaluation:
        ;; (cons 6 (a b c))
        '(6 a b c))

(define evlis
  (lambda (args table)
    (cond ((null? args) '())
          (else (cons (meaning (car args) table)
                      (evlis (cdr args) table))))))

(define function-of car)
(define arguments-of cdr)
(define *application
  (lambda (e table)
    (-apply (meaning (function-of e) table)
            (evlis (arguments-of e) table))))


;; all types:
`(types `(*const
          *quote
          *identifier
          *lambda
          *application
          *cond))


(define atom-to-action
  (lambda (e)
    (cond ((or (number? e)
               (eq? e #t)
               (eq? e #f)
               (eq? e 'cons)
               (eq? e 'car)
               (eq? e 'cdr)
               (eq? e 'null?)
               (eq? e 'eq?)
               (eq? e 'atom?)
               (eq? e 'zero?)
               (eq? e 'add1)
               (eq? e 'sub1)
               (eq? e 'number?)) *const)
          (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ((eq? (car e) 'quote)  *quote)
                 ((eq? (car e) 'lambda) *lambda)
                 ((eq? (car e) 'cond)   *cond)
                 (else *application)))
          (else *application))))

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; `value` together with all the functions it uses is an interpreter
;; - it approximates the function `eval` in Scheme and Lisp
;; - we pass an empty table to `meaning` which will eventually be filled
;;   with entries as we interpret the input expression
(define value
  (lambda (e)
    (meaning e '())))
