#lang racket
(require (except-in rackunit fail))
(provide (all-defined-out))

;; So it seems like, if we build the 1+arg version based on
;; conj2/disj2 primitives, we can implement them with:
;;
;; - macros or varargs
;; - left recursive or right recursive
;; - regular or flipped (verify that this latter is actually providing reasonable behavior)
;;
;; That points to 8 different possibilties (at least, might be still
;; more axes of variation than I’ve considered.)
;;
;; All four of the varags versions seem to require apply, at least in
;; the built-over-conj2/disj2 versions.
;;
;; - How many of those can have the conj2/disj2 substituted through and simplified away?
;;
;; - When you do that, what do they look like?
;;
;; Which one is our version?



(module* macros-1+-left-assoc #f
  (provide (all-defined-out))

  (define-syntax conj
    (syntax-rules ()
      ((conj g) g)
      ((conj g g1 gs ...) (conj (conj2 g g1) gs ...))))

  (define-syntax disj
    (syntax-rules ()
      ((disj g) g)
      ((disj g g1 gs ...) (disj (disj2 g g1) gs ...))))

  )

(module* macros-1+-right-assoc #f
  (provide (all-defined-out))

  (define-syntax conj
    (syntax-rules ()
      ((conj g) g)
      ((conj g g1 gs ...) (conj2 g (conj g1 gs ...)))))

  (define-syntax disj
    (syntax-rules ()
      ((disj g) g)
      ((disj g g1 gs ...) (disj2 g (disj g1 gs ...)))))

  )

(module* macros-1+-left-assoc-flip #f
  (provide (all-defined-out))

  (define-syntax conj
    (syntax-rules ()
      ((conj g) g)
      ((conj g g1 gs ...) (conj (conj2 g1 g) gs ...))))

  (define-syntax disj
    (syntax-rules ()
      ((disj g) g)
      ((disj g g1 gs ...) (disj (disj2 g1 g) gs ...))))

  )

(module* macros-1+-right-assoc-flip #f
  (provide (all-defined-out))

  (define-syntax conj
    (syntax-rules ()
      ((conj g) g)
      ((conj g g1 gs ...) (conj2 (conj g1 gs ...) g))))

  (define-syntax disj
    (syntax-rules ()
      ((disj g) g)
      ((disj g g1 gs ...) (disj2 (disj g1 gs ...) g))))

  )

(module* varargs-1+-left-assoc #f
  (provide (all-defined-out))

  (define (conj g . gs)
    (cond
      ((null? gs) g)
      (else (apply conj (cons (conj2 g (car gs)) (cdr gs))))))

  (define (disj g . gs)
    (cond
      ((null? gs) g)
      (else (apply disj (cons (disj2 g (car gs)) (cdr gs))))))

  )

(module* varargs-conj-left-disj-right #f
  (provide (all-defined-out))

  (define (conj g . gs)
    (cond
      ((null? gs) g)
      (else (apply conj (cons (conj2 g (car gs)) (cdr gs))))))

  (define (disj g . gs)
    (cond
      ((null? gs) g)
      (else (disj2 g (apply disj (cons (car gs) (cdr gs)))))))

  )

(module* varargs-1+-right-assoc #f
  (provide (all-defined-out))

  (define (conj g . gs)
    (cond
      ((null? gs) g)
      (else (conj2 g (apply conj (cons (car gs) (cdr gs)))))))

  (define (disj g . gs)
    (cond
      ((null? gs) g)
      (else (disj2 g (apply disj (cons (car gs) (cdr gs)))))))

  )

(module* varargs-1+-left-assoc-flip #f
  (provide (all-defined-out))

  (define (conj g . gs)
    (cond
      ((null? gs) g)
      (else (apply conj (cons (conj2 (car gs) g) (cdr gs))))))

  (define (disj g . gs)
    (cond
      ((null? gs) g)
      (else (apply disj (cons (disj2 (car gs) g) (cdr gs))))))

  )

(module* varargs-1+-right-assoc-flip #f
  (provide (all-defined-out))

  (define (conj g . gs)
    (cond
      ((null? gs) g)
      (else (conj2 (apply conj (cons (car gs) (cdr gs))) g))))

  (define (disj g . gs)
    (cond
      ((null? gs) g)
      (else (disj2 (apply disj (cons (car gs) (cdr gs))) g))))

  )

(module* varargs-conj-left-disj-right-flip #f
  (provide (all-defined-out))

  (define (conj g . gs)
    (cond
      ((null? gs) g)
      (else (apply conj (cons (conj2 g (car gs)) (cdr gs))))))

  (define (disj g . gs)
    (cond
      ((null? gs) g)
      (else (disj2 (apply disj (cons (car gs) (cdr gs))) g))))

  )

;; Implementation basis, for subsequent testing.

(define (var x) x)
(define (var? x) (number? x))

(define (find u s)
  (let ((pr (and (var? u) (assv u s))))
    (if pr (find (cdr pr) s) u)))

(define (ext-s x u s)
  (cond
    ((occurs? x u s) #f)
    (else `((,x . ,u) . ,s))))

(define (occurs? x u s)
  (cond
    ((var? u) (eqv? x u))
    ((pair? u) (or (occurs? x (find (car u) s) s)
                   (occurs? x (find (cdr u) s) s)))
    (else #f)))

(define (unify u v s)
  (cond
    ((eqv? u v) s)
    ((var? u) (ext-s u v s))
    ((var? v) (unify v u s))
    ((and (pair? u) (pair? v))
     (let ((s (unify (find (car u) s) (find (car v) s) s)))
       (and s (unify (find (cdr u) s) (find (cdr v) s) s))))
    (else #f)))

(define (== u v)
  (lambda (st)
    (let ((s (state->σ st)))
      (let ((s (unify (find u s) (find v s) s)))
        (if s (return s (state->≠ st) (state->ct st))
            '())))))

(define (invalid? s d)
  (ormap (lambda (pr) (equal? (unify (find (car pr) s) (find (cdr pr) s) s) s)) d)) ;; type kludge

(define (return s d c) (if (invalid? s d) '() (list (state s d c))))

(define (=/= u v)
  (lambda (st)
    (return (state->σ st) (cons `(,u . ,v) (state->≠ st)) (state->ct st))))

(struct state (>σ >≠ >ct) #:transparent)

(define empty-state
  (state '() '() 0))

(define (call/initial-state n g)
  (take n (pull (g empty-state))))

(define ((disj2 g1 g2) s/c)
  ($append (g1 s/c) (g2 s/c)))

(define ((conj2 g1 g2) s/c)
  ($append-map g2 (g1 s/c)))

(define succeed (λ (st) (list st)))
(define fail (λ (st) (list)))

(define ($append $1 $2)
  (cond
    ((null? $1) $2)
    ((promise? $1) (delay/name ($append $2 (force $1))))
    (else (cons (car $1) ($append (cdr $1) $2)))))

(define ($append-map g $)
  (cond
    ((null? $) `())
    ((promise? $) (delay/name ($append-map g (force $))))
    (else ($append (g (car $)) ($append-map g (cdr $))))))

(define-syntax-rule (define-relation (defname . args) g)
  (define ((defname . args) st) (delay/name (g st))))

(define (take n $)
  (cond
    ((null? $) '())
    ((and n (zero? (- n 1))) (list (car $)))
    (else (cons (car $)
                (take (and n (- n 1)) (pull (cdr $)))))))

(define (pull $) (if (promise? $) (pull (force $)) $))

(define ((call/fresh f) st)
  (let ((c (state->ct st)))
    ((f (var c)) (state (state->σ st)
                        (state->≠ st)
                        (+ c 1)))))

(define ((ifte g0 g1 g2) st)
  (let loop (($ (g0 st)))
    (cond
      ((null? $) (g2 st))
      ((promise? $) (delay/name (loop (force $))))
      (else ($append-map g1 $)))))

(define ((once g) st)
  (let loop (($ (g st)))
    (cond
      ((null? $) '())
      ((promise? $) (delay/name (loop (force $))))
      (else (list (car $))))))
