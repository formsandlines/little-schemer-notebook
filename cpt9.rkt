;; -*- geiser-scheme-implementation: racket -*-

;; Chapter 9

;; my own first draft
;; - assumes that each n in lat <= length lat
;; - assumes that references form a finite chain
(define looking
  (lambda (a lat)
    (define loop
      (lambda (x)
        (cond ((number? x) (loop (pick x lat)))
              (else (eq? a x)))))
    (cond ((empty? lat) #f)
          ((number? (car lat)) (loop (pick (car lat) lat)))
          (else (looking a (cdr lat))))))

(eq? (looking 'caviar '(6 2 4 caviar 5 7 3))
     #t)

(eq? (looking 'caviar '(6 2 grits caviar 5 7 3))
     #f)

(define keep-looking
  (lambda (a sorn lat) ; sorn -> symbol or number
    (cond ((number? sorn) (keep-looking a (pick sorn lat) lat))
          (else (eq? a sorn)))))

;; as defined in the book
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(eq? (looking 'caviar '(6 2 4 caviar 5 7 3))
     #t)

(eq? (looking 'caviar '(6 2 grits caviar 5 7 3))
     #f)

;; infinite loop (-> function is not total):
;; (looking 'caviar '(7 1 2 caviar 5 6 3))


;; `shift` shifts a root-level pair in a tree to exclude its first element
;; and include the next root-level s-expression

;; assumes first element of `pair` is a pair
(define shift
  (lambda (pair)
    (build (fst (fst pair))
           (build (snd (fst pair))
                  (snd pair)))))

(equal? (shift '((a b) c))
        '(a (b c)))

(equal? (shift '((a b) (c d)))
        '(a (b (c d))))

;; `align` provides no guarantee that it makes progress
;; because the input list changes in recursion due to `shift`
;; and hence cannot be considered a subpart according to the 7. commandment
(define align
  (lambda (pora) ; pora -> pair or atom
    (cond
     ((atom? pora) pora)
     ((a-pair? (fst pora)) (align (shift pora)))
     (else (build (fst pora)
                  (align (snd pora)))))))

(equal? (align '((a b) c))
        '(a (b c)))
(equal? (align '((a b) ((c d) e)))
        '(a (b (c (d e)))))
(equal? (align '((a b) (c (d e))))
        '(a (b (c (d e)))))
(equal? (align '((a (b c)) (d e)))
        '(a (b (c (d e)))))
(equal? (align '(((((((a b) c) d) e) f) g) h))
        '(a (b (c (d (e (f (g h))))))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (length* (fst pora))
                    (length* (snd pora)))))))

(= (length* '((a (b c)) (d e)))
   5)

;; alternative (not in the book) to make use of aligned pairs in `length*`
(define length*
  (lambda (pora)
    (define aux
      (lambda (pora)
        (cond ((atom? (snd pora)) 2)
              (else (add1 (length* (snd pora)))))))
    (aux (align pora))))

(= (length* '((a (b c)) (d e)))
   5)

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (× (weight* (fst pora)) 2)
                    (weight* (snd pora)))))))

;; aligned pairs are lighter because `weight*` counts first elem. twice
;; so the weight of `align`s argument gets successively smaller
(= (weight* '((a (b c)) (d e)))
   13)
(= (weight* (align '((a (b c)) (d e))))
   9)

(= (weight* '((a b) c))
   7)
(= (weight* '(a (b c)))
   5)

;; hence `align` is a total function since it yields a value
;; for every argument (every pair has a weight)

;; this function is just like `align` but with `revpair` intead of `shift`
;; note: `revpair` only does flat reverse of pair elements
(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (fst pora)) (shuffle (revpair pora)))
          (else (build (fst pora)
                       (shuffle (snd pora)))))))

(equal? (shuffle '(a ((b c) d)))
        '(a (d (b c))))

(equal? (shuffle '(a (b c)))
        '(a (b c)))

(equal? (shuffle '(a b))
        '(a b))

;; infinite recursion because first elem. of pair remains a pair:
;; (equal? (shuffle '((a b) (c d))))

;; therefore `shuffle` is not a total function

(define C ; -> Collatz conjecture
  (lambda (n)
    (cond ((one? n) 1)
          (else
           (cond ((even? n) (C (÷ n 2)))
                 (else (C (add1 (× 3 n)))))))))

;; doesn’t yield a value for 0
;; otherwise nobody knows if its total ?
(= (C 1) 1)
(= (C 2) 1)
(= (C 3) 1)
(= (C 4) 1)
(= (C 5) 1)
(= (C 6) 1)
(= (C 7) 1)
(= (C 8) 1)
(= (C 9) 1)

(define A ; Ackermann function
  (lambda (n m)
    (cond ((zero? n) (add1 m))
          ((zero? m) (A (sub1 n) 1))
          (else (A (sub1 n)
                   (A n (sub1 m)))))))

;; the arguments for `A` do not necessarily decrease in recursion
;; however, `A` is still a total function

(= (A 1 0) 2)
(= (A 1 1) 3)
(= (A 2 2) 7)

(= (A 1 10) 12)
;; too much recursion?
;; (= (A 4 3))


;; Halting Problem

;; This function should check if a function stops:
(define will-stop?
  (lambda (f)
    (or #t #f))) ; whatever we do here

;; never actually call that
(define eternity
  (lambda (x)
    (eternity x)))

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

;; if it returns #f, `and` will short-circuit, so it must return #t
;; if it returns #t, `eternity` cannot stop, so it must return #f
;; therefore, we have a contradiction
(will-stop? last-try)

;; This means:
;; In the reflexive case, where `will-stop?` occurs in the same function that
;; it is trying to check, it cannot, in some cases, give a consistent answer,
;; even if we just look at how the interface of `will-stop?` is defined,
;; without regards to the implementation.
;;
;; Does this mean that it cannot be defined? For the general case, yes, but
;; what if we only allow functions that do not call `will-stop?`?
;; However, as the authors say, it makes this function the first case where
;; we cannot define it, even though we can describe it precisely (we can
;; describe what it should do, but not define the function in a logically
;; consistent way).


;; What if we didn’t have `define`?
;; We need it to provide an identifier for the function to call itself,
;; so is it possible to define recursive functions without an identifier?

;; This was `length` with define:
(define length
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (length (cdr l)))))))

;; Without `define`, it only works on the empty list:
;; -> `eternity` is a proxy here for “no answer”, since it never halts
(lambda (l)
  (cond ((null? l) 0)
        (else (add1 (eternity (cdr l))))))
;; we could name it `length_0` (but we can’t)
(define length_0 identity)

;; What would `length_<=1` be?
;; just like `length_0`, but call it again:
(lambda (l)
  (cond ((null? l) 0)
        (else (add1 (length_0 (cdr l))))))
;; not allowed, so we could replace `length_0` with its function body:
(lambda (l)
  (cond ((null? l) 0)
        (else (add1 ((lambda (l)
                       (cond ((null? l) 0)
                             (else (add1 (eternity (cdr l))))))
                     (cdr l))))))

;; For `length_<=2` and so on, we just replace `eternity` with `length_0`
;; again and again, but we can only write a finite function this way
;; and it would be very painful to write `length_<=10000`.

;; There is a common pattern repeated over and over again, so what if
;; we abstract it? (-> ninth commandment)

;; This function creates `length_0` (as demonstrated):
(((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l)))))))
  eternity)
 '())

;; Here, we first create `length_0` as before and use the result to
;; create `length_<=1`:
;; -> like function composition: `(f (g eternity))`
;; -> parameter names don’t matter (see De Bruijn index in lambda calculus)
(((lambda (f)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (f (cdr l)))))))
  ((lambda (g)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (g (cdr l)))))))
   eternity))
 '(a))

;; However, we still create repetitions and there is just more indirection.

;; We can give the function that creates a function that looks like `length`
;; a name, such as `make-length`:
(((lambda (make-length)
    (make-length eternity))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '())

;; How does that solve our repetition problem?
;; Well, we can just call this function on itself, via its name, to obtain
;; the `length_<=1`:
(((lambda (make-length)
    (make-length
     (make-length eternity)))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(a))

;; This now saves us SO MUCH repetition:
;; (it may be similar to a continuation pattern)
(((lambda (make-length)
    (make-length
     (make-length
      (make-length
       (make-length eternity)))))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(a b c))

;; But, we we still cannot replace `length` this way, not unless we get rid
;; of our `eternity` proxy.

;; Here, recursion seems like an infinite tower of applications of the
;; same pattern/function to an arbitrary function (like `eternity`) that
;; must never be called.
;; If we still have to call it, we need more applications of the pattern.
;; But how many is enough?

;; Since `eternity` is just an arbitrary function, we could as well pass
;; `make-length` to `make-length` and still get `length_0`:
(((lambda (make-length)
    (make-length make-length))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '())

;; Using `make-length` instead of `length` as the function parameter reminds
;; us that we actually pass that name to the first call:
(((lambda (make-length)
    (make-length make-length))
  (lambda (make-length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (make-length (cdr l))))))))
 '())

;; But now our abstraction looks different; we no longer call the next
;; `length` function with `cdr l` but the function that makes those functions.
;; So lets actually call it on `eternity` to make `length_0` initially,
;; which creates an additional recursion:
(((lambda (make-length)
    (make-length make-length))
  (lambda (make-length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 ((make-length eternity)
                         (cdr l))))))))
 '(apples))


;; Now instead of just calling the next continuation, we call a constructor
;; for a continuation that takes `eternity` for its continuation.
;; To understand this now very abstract construct, lets break it down.

;; First, `make-length` gets applied to itself:
(((lambda (make-length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 ((make-length eternity)
                         (cdr l)))))))
  (lambda (make-length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 ((make-length eternity)
                         (cdr l))))))))
 '(apples))

;; Which looks like this:
((lambda (l)
   (cond ((null? l) 0)
         (else (add1 (((lambda (make-length)
                         (lambda (l)
                           (cond ((null? l) 0)
                                 (else (add1 ((make-length eternity)
                                              (cdr l)))))))
                       eternity)
                      (cdr l))))))
 '(apples))

;; Next, we apply our list argument:
(cond ((null? '(apples)) 0)
      (else (add1 (((lambda (make-length)
                      (lambda (l)
                        (cond ((null? l) 0)
                              (else (add1 ((make-length eternity)
                                           (cdr l)))))))
                    eternity)
                   (cdr '(apples))))))

;; Then we apply `eternity` to `make-length` in the lambda, constructing
;; our next continuation (where the next one should never be constructed):
(cond ((null? '(apples)) 0)
      (else (add1 ((lambda (l)
                     (cond ((null? l) 0)
                           (else (add1 ((eternity eternity)
                                        (cdr l))))))
                   (cdr '(apples))))))

;; Further evaluation yields the result:
(cond ((null? '(apples)) 0)
      (else (add1 (cond ((null? (cdr '(apples))) 0)
                        (else (add1 ((eternity eternity)
                                     (cdr (cdr '(apples))))))))))
(add1 (cond ((null? (cdr '(apples))) 0)
            (else (add1 ((eternity eternity)
                         (cdr (cdr '(apples))))))))
(add1 (cond (#t 0)
            (else (add1 ((eternity eternity)
                         (cdr (cdr '(apples))))))))
(add1 0)
1

;; The intermediate result of that application looks just like our first
;; length_<=1, except we now call `eternity` on itself, which doesn’t
;; really matter though:
((lambda (l)
   (cond ((null? l) 0)
         (else (add1 ((lambda (l)
                        (cond ((null? l) 0)
                              (else (add1 ((eternity eternity) (cdr l))))))
                      (cdr l))))))
 '(a))


;; Now what if the next continuation constructor always itself gets the
;; next continuation constructor without ever having to call `eternity`?

;; This is like crossing an infinite chasm by building a bridge with pillars
;; that we construct and deconstruct as we move along, always taking the
;; previous as the next pillar with us and never running out of resources.

;; The resulting function is equivalent to `length`, even though we don’t have
;; to define it by name, at least not directly:

(((lambda (make-length)
    (make-length make-length))
  (lambda (make-length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 ((make-length make-length) (cdr l))))))))
 '(a b c d e))

;; As we see in the analysis, the recursive call on itself makes sure that the
;; `length` constructor is always kept in “memory”, leaving behind an endless
;; application trail that creates `length_n`:

((lambda (make-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 ((make-length make-length) (cdr l)))))))
 (lambda (make-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 ((make-length make-length) (cdr l))))))))

(lambda (l)
  (cond ((null? l) 0)
        (else (add1 (((lambda (make-length)
                        (lambda (l)
                          (cond ((null? l) 0)
                                (else (add1 ((make-length make-length)
                                             (cdr l)))))))
                      (lambda (make-length)
                        (lambda (l)
                          (cond ((null? l) 0)
                                (else (add1 ((make-length make-length)
                                             (cdr l))))))))
                     (cdr l))))))

;; (the next function would only be produced when the list is not null)
(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1 ((lambda (l)
             (cond
              ((null? l) 0)
              (else
               (add1 (((lambda (make-length)
                         (lambda (l)
                           (cond
                            ((null? l) 0)
                            (else
                             (add1 ((make-length make-length) (cdr l)))))))
                       (lambda (make-length)
                         (lambda (l)
                           (cond
                            ((null? l) 0)
                            (else
                             (add1 ((make-length make-length) (cdr l))))))))
                      (cdr l))))))
           (cdr l))))))


;; How can we retain the original `length` function?

;; We could try to abstract away the `(make-length make-length)` call,
;; since it actually produces our `length` functions:

'((lambda (make-length)
    (make-length make-length))
  (lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (make-length make-length))))

;; Let’s analyse this:

'((lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (make-length make-length)))
  (lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (make-length make-length))))

'((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l)))))))
  ((lambda (make-length)
     ((lambda (length)
        (lambda (l)
          (cond ((null? l) 0)
                (else (add1 (length (cdr l)))))))
      (make-length make-length)))
   (lambda (make-length)
     ((lambda (length)
        (lambda (l)
          (cond ((null? l) 0)
                (else (add1 (length (cdr l)))))))
      (make-length make-length)))))

;; Well, now this keeps producing `length` functions, but will never apply
;; any of them because it’s stuck producing.

;; This is because we need to call the recursion eagerly – it is not delayed
;; inside the nested lambdas and `cond` calls anymore.

;; Where we returned a function before, we now always return an application –
;; which must be evaluated before passing the list argument.

'((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))
   ((lambda (make-length)
      ((lambda (length)
         (lambda (l)
           (cond ((null? l) 0)
                 (else (add1 (length (cdr l)))))))
       (make-length make-length)))
    (lambda (make-length)
      ((lambda (length)
         (lambda (l)
           (cond ((null? l) 0)
                 (else (add1 (length (cdr l)))))))
       (make-length make-length))))))


;; So we need to pass a function instead, which would return the same
;; application, thereby delaying its evaluation:

(((lambda (make-length)
    (make-length make-length))
  (lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (lambda (x)
       ((make-length make-length) x)))))
 '(a b c d e))

'((lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (lambda (x)
       ((make-length make-length) x))))
  (lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     (lambda (x)
       ((make-length make-length) x)))))

'((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l)))))))
  (lambda (x)
    (((lambda (make-length)
        ((lambda (length)
           (lambda (l)
             (cond ((null? l) 0)
                   (else (add1 (length (cdr l)))))))
         (lambda (x)
           ((make-length make-length) x))))
      (lambda (make-length)
        ((lambda (length)
           (lambda (l)
             (cond ((null? l) 0)
                   (else (add1 (length (cdr l)))))))
         (lambda (x)
           ((make-length make-length) x)))))
     x)))

'(lambda (l)
   (cond ((null? l) 0)
         (else (add1 ((lambda (x)
                        (((lambda (make-length)
                            ((lambda (length)
                               (lambda (l)
                                 (cond ((null? l) 0)
                                       (else (add1 (length (cdr l)))))))
                             (lambda (x)
                               ((make-length make-length) x))))
                          (lambda (make-length)
                            ((lambda (length)
                               (lambda (l)
                                 (cond ((null? l) 0)
                                       (else (add1 (length (cdr l)))))))
                             (lambda (x)
                               ((make-length make-length) x)))))
                         x))
                      (cdr l))))))


;; Now we can extract the function that looks like `length` and give it
;; a name (this is like the original `make-length` function):
(((lambda (le)
    ((lambda (make-length)
       (make-length make-length))
     (lambda (make-length)
       (le (lambda (x)
             ((make-length make-length) x))))))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(a b c d e))

;; The function that makes `length` is actually known as the
;; “applicative-order Y combinator”.

;; It passes the given function a constructor of its own continuation,
;; to reconstruct itself in itself, while moving that constructor along
;; to be able to repeat the process indefinitely.

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; Fixed-point combinator in lambda calculus:
;;   Y ≡ λf.(λx.f(xx))(λx.f(xx))
;;   such that ∀F F(YF) = YF.  (where F is a lambda-term)

(lambda (f)
  ((lambda (x) (f (x x)))
   (lambda (x) (f (x x)))))
;; =>
(lambda (f)
  (f ((lambda (x) (f (x x)))
      (lambda (x) (f (x x))))))
;; =>
(lambda (f)
  (f (f ((lambda (x) (f (x x)))
         (lambda (x) (f (x x)))))))
;; =>
(lambda (f)
  (f (f (f ((lambda (x) (f (x x)))
            (lambda (x) (f (x x))))))))
;; …

;; Compared with the Y-definition above (param names adjusted):
(lambda (f)
  ((lambda (x) (x x))
   (lambda (x)
     (f (lambda (arg) ((x x) arg))))))

(lambda (f)
  ((lambda (x)
     (f (lambda (arg) ((x x) arg))))
   (lambda (x)
     (f (lambda (arg) ((x x) arg))))))

(lambda (f)
  (f (lambda (arg)
       (((lambda (x)
           (f (lambda (arg) ((x x) arg))))
         (lambda (x)
           (f (lambda (arg) ((x x) arg)))))
        arg))))

(lambda (f)
  (f (lambda (arg)
       ((f (lambda (arg)
             (((lambda (x)
                 (f (lambda (arg) ((x x) arg))))
               (lambda (x)
                 (f (lambda (arg) ((x x) arg)))))
              arg)))
        arg))))

(lambda (f)
  (f (lambda (arg)
       ((f (lambda (arg)
             ((f (lambda (arg)
                   (((lambda (x)
                       (f (lambda (arg) ((x x) arg))))
                     (lambda (x)
                       (f (lambda (arg) ((x x) arg)))))
                    arg)))
              arg)))
        arg))))



((Y (lambda (length)
      (lambda (l)
        (cond ((null? l) 0)
              (else (add1 (length (cdr l))))))))
 '(a b c d e))

(let ((-length (lambda (length)
                 (lambda (l)
                   (cond ((null? l) 0)
                         (else (add1 (length (cdr l)))))))))
  ((-length (Y -length))
   '(a b c d e)))

;; Applying `Y` to itself would (possibly) reconstruct and re-apply this
;; recursion building process again and again, creating an infinite tower
;; of recursive functions.

'(Y Y)

;; This is how the application would unfold (we could say `le` = `Y`):

'((lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x))))))
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

'((lambda (f) (f f))
  (lambda (f)
    ((lambda (le)
       ((lambda (f) (f f))
        (lambda (f)
          (le (lambda (x) ((f f) x))))))
     (lambda (x) ((f f) x)))))

'((lambda (f)
    ((lambda (le)
       ((lambda (f) (f f))
        (lambda (f)
          (le (lambda (x) ((f f) x))))))
     (lambda (x) ((f f) x))))
  (lambda (f)
    ((lambda (le)
       ((lambda (f) (f f))
        (lambda (f)
          (le (lambda (x) ((f f) x))))))
     (lambda (x) ((f f) x)))))

;; It seems to create new applications, so it doesn’t ever terminate:

'((lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x))))))
  (lambda (x) (((lambda (f)
                  ((lambda (le)
                     ((lambda (f) (f f))
                      (lambda (f)
                        (le (lambda (x) ((f f) x))))))
                   (lambda (x) ((f f) x))))
                (lambda (f)
                  ((lambda (le)
                     ((lambda (f) (f f))
                      (lambda (f)
                        (le (lambda (x) ((f f) x))))))
                   (lambda (x) ((f f) x))))) x)))

'((lambda (f) (f f))
  (lambda (f)
    ((lambda (x) (((lambda (f)
                     ((lambda (le)
                        ((lambda (f) (f f))
                         (lambda (f)
                           (le (lambda (x) ((f f) x))))))
                      (lambda (x) ((f f) x))))
                   (lambda (f)
                     ((lambda (le)
                        ((lambda (f) (f f))
                         (lambda (f)
                           (le (lambda (x) ((f f) x))))))
                      (lambda (x) ((f f) x))))) x))
     (lambda (x) ((f f) x)))))

'((lambda (f)
    ((lambda (x) (((lambda (f)
                     ((lambda (le)
                        ((lambda (f) (f f))
                         (lambda (f)
                           (le (lambda (x) ((f f) x))))))
                      (lambda (x) ((f f) x))))
                   (lambda (f)
                     ((lambda (le)
                        ((lambda (f) (f f))
                         (lambda (f)
                           (le (lambda (x) ((f f) x))))))
                      (lambda (x) ((f f) x))))) x))
     (lambda (x) ((f f) x))))
  (lambda (f)
    ((lambda (x) (((lambda (f)
                     ((lambda (le)
                        ((lambda (f) (f f))
                         (lambda (f)
                           (le (lambda (x) ((f f) x))))))
                      (lambda (x) ((f f) x))))
                   (lambda (f)
                     ((lambda (le)
                        ((lambda (f) (f f))
                         (lambda (f)
                           (le (lambda (x) ((f f) x))))))
                      (lambda (x) ((f f) x))))) x))
     (lambda (x) ((f f) x)))))

'((lambda (x) (((lambda (f)
                  ((lambda (le)
                     ((lambda (f) (f f))
                      (lambda (f)
                        (le (lambda (x) ((f f) x))))))
                   (lambda (x) ((f f) x))))
                (lambda (f)
                  ((lambda (le)
                     ((lambda (f) (f f))
                      (lambda (f)
                        (le (lambda (x) ((f f) x))))))
                   (lambda (x) ((f f) x))))) x))
  (lambda (x) (((lambda (f)
                  ((lambda (x) (((lambda (f)
                                   ((lambda (le)
                                      ((lambda (f) (f f))
                                       (lambda (f)
                                         (le (lambda (x) ((f f) x))))))
                                    (lambda (x) ((f f) x))))
                                 (lambda (f)
                                   ((lambda (le)
                                      ((lambda (f) (f f))
                                       (lambda (f)
                                         (le (lambda (x) ((f f) x))))))
                                    (lambda (x) ((f f) x))))) x))
                   (lambda (x) ((f f) x))))
                (lambda (f)
                  ((lambda (x) (((lambda (f)
                                   ((lambda (le)
                                      ((lambda (f) (f f))
                                       (lambda (f)
                                         (le (lambda (x) ((f f) x))))))
                                    (lambda (x) ((f f) x))))
                                 (lambda (f)
                                   ((lambda (le)
                                      ((lambda (f) (f f))
                                       (lambda (f)
                                         (le (lambda (x) ((f f) x))))))
                                    (lambda (x) ((f f) x))))) x))
                   (lambda (x) ((f f) x))))) x)))
