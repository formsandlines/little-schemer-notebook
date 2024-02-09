;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 1

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; All atoms and lists are s-expressions

(atom? 'nil)
(atom? 'a)

;; Lists are collections of s-expressions

(list? '())
(list? '(a b))
(list? '(a (b c)))

'() ;; <- null or empty list

(null? '())
(atom? '()) ;; <- not an atom (maybe…)


(let ((l '(a b c)))
  (car l))

(let ((l '((a b c) x y z)))
  (car l))

;; Atoms cannot be deconstructed:

;; (car 'hotdog)  ;; <- not a list
;; (car "hotdog")  ;; <- not a list

(list? "foo")

;; The empty list is not an atom, but it has no first element
;; (car '())  ;; <- not a pair

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   *The Law of Car*                                                         ;;
;;                                                                            ;;
;; The primitive `car` is defined only for non-empty lists.                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(car '(((hotdogs)) (and) (pickle) relish))

(car (car '(((hotdogs)) (and))))

(cdr '(a b c))

(cdr '((a b c) x y z))

(cdr '(hamburger))

(cdr '((x) t r))

;; (cdr 'hotdogs)  ;; <- no cdr of atom!

;; (cdr '())  ;; <- empty list has no first element!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   *The Law of Cdr*                                                         ;;
;;                                                                            ;;
;; The primitive `cdr` is defined only for non-empty lists.                   ;;
;; The `cdr` of any non-empty list is always another list.                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(car (cdr '((b) (x y) ((c)))))

(cdr (cdr '((b) (x y) ((c)))))

;; (cdr (car '(a (b (c)) d)))  ;; <- car is an atom!

(cons 'peanut '(butter and jelly))

(cons '(banana and) '(peanut butter and jelly))

(cons '((help) this) '(is very ((hard) to learn)))

(cons '(a b (c)) '())

(cons 'a '())

;; in the book, this has no answer, because second arg must be a list
(cons '((a b c)) 'b) ;; => (((a b c)) . b)  <- cons cell! (pair)
(cons 'a 'b) ;; => (a . b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   *The Law of Cons*                                                        ;;
;;                                                                            ;;
;; The primitive `cons` takes two arguments.                                  ;;
;; The second argument to `cons` must be a list. The result is a list.        ;;
;; (not true for `cons` definition in Scheme)                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cons 'a (car '((b) c d)))

(cons 'a (cdr '((b) c d)))

(null? '())

(null? '(a b c))

(null? 'spaghetti) ;; => #f (no answer according to the book)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   *The Law of Null?*                                                       ;;
;;                                                                            ;;
;; The primitive `null?` is defined only for lists.                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comment: why are functions called primitives?

(atom? 'Harry)

(atom? '(Harry had a heap of apples))

(atom? (car '(Harry had a heap of apples)))

(atom? (cdr '(Harry had a heap of apples)))

(atom? (cdr '(Harry)))

(atom? (car (cdr '(swing low sweet cherry oat))))

(atom? (car (cdr '(swing (low sweet) cherry oat))))

(eq? 'Harry 'Harry) ;; => #t (referential equality!)

(eq? 'margarine 'butter)

(eq? '(a) '(a)) ;; => #f (only works with atoms!)

(let ((x '(a)))
  (eq? x x)) ;; => #t (referential equality!)

(eq? '() '(strawberry)) ;; => #f (no answer according to the book)

(eq? 6 7) ;; => #f (no answer according to the book)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   *The Law of Eq?*                                                         ;;
;;                                                                            ;;
;; The primitive `eq?` takes two arguments. Each must be a non-numeric atom.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eq? (car '(Mary had a little lamb chop)) 'Mary)

(eq? (cdr '(soured milk)) 'milk) ;=> #f (no answer according to the book)

(let ((l '(beans beans we need jelly beans)))
  (eq? (car l) (car (cdr l))))
