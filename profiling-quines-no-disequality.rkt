#lang racket
(require (except-in rackunit fail))
(require (for-syntax syntax/parse))
(require "./logical-combinator-function-definitions.rkt")

;; Testing different implementations of underlying logical combinators
;; for miniKanren quines without disequality constraints.

;; left  associative conjunction ((((a & b) & c) & d) & e)
;; right associative conjunction (a & (b & (c & (d & e))))
;;
;; execute goals from leftmost argument to rightmost argument
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-left-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-right-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-left-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-right-assoc))
;;
;; execute goals from rightmost argument to leftmost argument
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-left-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-right-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-left-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-right-assoc-flip))
;;
;; mixed
(require (submod "./logical-combinator-function-definitions.rkt" varargs-conj-left-disj-right))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-conj-left-disj-right-flip))

;; External Interface
;; ---------------------------------------------------------------------------------------------------

;; We use this definition of fresh to doubly ensure we have the right
;; ’cond’ and call it right here.

(define-syntax fresh
  (λ (stx)
    (syntax-parse stx
      [(_ (x ...) g)
       (let ((n (length (syntax->list #'(x ...)))))
         #`(λ (st)
             (let* ((c (state->ct st))
                    (nc (+ #,n c)))
               ((apply
                 (λ (x ...) g)
                 (range c nc))
                (state (state->σ st) (state->≠ st) nc)))))])))

(define-syntax-rule (run* (q) g0 g ...)
  (call/initial-state
   -1
   (fresh (q) g0 g ...)))

(define-syntax-rule (run n (q) g0 g ...)
  (call/initial-state
   n
   (fresh (q) g0 g ...)))

;; Interpreter Implementation
;; ---------------------------------------------------------------------------------------------------

;; Peano numbers w/1 cons cell saved
(define-relation (nat? o)
  (disj
   (== o '())
   (fresh (n)
     (conj
       (== o `(s . ,n))
       (nat? n)))))

;; Terms
(define-relation (expr? o)
  (disj
   (fresh (n)
     (conj
       (== o `(x . ,n))
       (nat? n)))
   (== o 'quote) ;; why isn't this a special form?
   (fresh (n t)
     (conj
       (== o `(λ (x . ,n) ,t))
       (nat? n)
       (expr? t)))
   (fresh (t₁ t₂)
     (conj
       (== o `(,t₁ ,t₂))
       (expr? t₁)
       (expr? t₂)))
   (fresh (t₁ t₂)
     (conj
       (== o `(list₂ ,t₁ ,t₂))
       (expr? t₁)
       (expr? t₂)))))

;; Values
(define-relation (val? o)
  (disj
   (fresh (e n t)
     (conj
       (== `(closure ,e ,n ,t) o)
       (env? e)
       (nat? n)
       (expr? t)))
   (fresh (t)
     (conj
       (== `(code ,t) o)
       (expr? t)))))

;; Environment
(define-relation (env? o)
  (disj
   (== o '())
   (fresh (n v e)
     (conj
       (== o `((,n . ,v) . ,e))
       (nat? n)
       (val? v)
       (env? e)))))

;;; Helpers

;; Disequality for Peano Numbers
(define-relation (=/= n₁ n₂)
  (disj
   (fresh (pn₂)
          (conj
           (== n₂ `(s . ,pn₂))
           (== n₁ '())))
   (fresh (pn₁)
          (conj
           (== n₁ `(s . ,pn₁))
           (== n₂ '())))
   (fresh (pn₁ pn₂)
          (conj
           (== n₁ `(s . ,pn₁))
           (== n₂ `(s . ,pn₂))
           (=/= pn₁ pn₂)))))

;; Environment Lookup (where keys are indices)
(define-relation (lookup e i v)
  (fresh (j vj er)
    (conj
     (== e `((,j . ,vj) . ,er))
     (disj
      (conj (== i j)
            (== v vj))
      (conj (=/= i j)
            (lookup er i v))))))

;; Evaluation
(define-relation (valof e t v)
  (disj
   (fresh (x)
          (conj
           (== t `(x . ,x))
           (lookup e x v)))
   (fresh (x t0)
          (conj
           (== t `(λ (x . ,x) ,t0))
           (== v `(closure ,e ,x ,t0))))
   (fresh (t0)
          (conj
           (== t `(quote ,t0))
           (== v `(code ,t0))))
   (fresh (t₁ t₂ e0 x0 t0 v₂)
          (conj
           (== t `(,t₁ ,t₂))
           (valof e t₁ `(closure ,e0 ,x0 ,t0))
           (valof e t₂ v₂)
           (valof `((,x0 . ,v₂) . ,e0) t0 v)))
   (fresh (t₁ t₂ c₁ c₂)
          (conj
           (== t `(list₂ ,t₁ ,t₂))
           (valof e t₁ `(code ,c₁))
           (valof e t₂ `(code ,c₂))
           (== v `(code (,c₁ ,c₂)))))))


;; Correctness Tests
;; ---------------------------------------------------------------------------------------------------
(run 1 (q)
  (valof '()
      '(((λ (x) (λ (x) (x))) (quote quote)) (quote λ))
      q))

(run 1 (q)
  (valof '()
      '(((λ (x) (λ (x s) (x))) (quote λ)) (quote quote))
      q))

(let ((res (run 1 (q) (valof '() q '(code (I love you))))))
  (and (not (null? res)) (null? (cdr res))))

;; Quine verification.
(define quine
  '((λ (x) (list₂ (x) (list₂ (quote quote) (x))))
    (quote (λ (x) (list₂ (x) (list₂ (quote quote) (x)))))))

(let ((res (run* (q) (valof '() quine `(code ,quine)))))
  (and (not (null? res)) (null? (cdr res))))


;; Benchmarking tests
;; ---------------------------------------------------------------------------------------------------

(define 40quines
  (thunk
   (void (run 40 (q) (valof '() q `(code ,q))))))


(define 5twines
 (thunk
  (void
   (run 5 (q)
        (fresh (a b)
               (conj
                (== q `(,a ,b))
                (valof '() a `(code ,b))
                (valof '() b `(code ,a))))))))

(define 1thrine
  (thunk
   (void
    (run 1 (q)
         (fresh (a b c)
                (conj
                 (== q `(,a ,b ,c))
                 (valof '() a `(code ,b))
                 (valof '() b `(code ,c))
                 (valof '() c `(code ,a))))))))

;; varargs-conj-left-disj-right
;; 40quines cpu time: 82475 real time: 83550 gc time: 3859
;; 5twines  cpu time: 301890 real time: 304926 gc time: 13996
;; 1thrine  cpu time: 50114 real time: 50652 gc time: 4291

(time (40quines))
(time (5twines))
(time (1thrine))

;; varargs-1+-right-assoc
;; 40quines cpu time: 134356 real time: 135632 gc time: 13496
;; 5twines  cpu time: 363616 real time: 366425 gc time: 35026
;; 1thrine  cpu time: 200798 real time: 201933 gc time: 43823

(module+ norm
  (require (only-in rnrs/base-6 assert))
  ;;; Normalization (of terms, so that they can be valofuated in Scheme)
  ;; Our language is a subset of Scheme, except that we need to turn
  ;; our peano-encoded variables into Scheme symbols.

  ;; Normalizes the list (s ...(s <x>)) to s...s<x>,
  ;; where s can be applied 0 or more times to the symbol <x>,
  ;; which will be either z or an unbound logic variable such as _.0.
  (define normalize-var-name
    (λ (n)
      (if (and (list? n) (eq? 's (car n)) (null? (cddr n)))
          (string->symbol (string-append
                           (symbol->string (car n))
                           (symbol->string (normalize-var-name (cadr n)))))
          (begin
            (assert (symbol? n))
            n))))

  ;; Normalizes all occurrences of (x <peano>) to a symbol.
  (define normalize
    (λ (t)
      (if (list? t)
          (if (and (eq? 'x (car t)) (null? (cddr t)))
              (normalize-var-name (cadr t))
              (map normalize t))
          (begin
            (assert (not (eq? 'x t)))
            t)))))
