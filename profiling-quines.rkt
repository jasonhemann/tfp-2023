#lang racket
(require (except-in rackunit fail))
(require (for-syntax syntax/parse))
(require "./logical-combinator-function-definitions.rkt")

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

;; Testing different implementations of underlying logical combinators
;; for miniKanren quines, with and without disequality constraints.


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




;; Peano numbers w/1 cons cell saved
(defrel (nat? o)
  (disj
   (== o '())
   (fresh (n)
     (conj
       (== o `(s . ,n))
       (nat? n)))))

;; Terms
(defrel (expr? o)
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
(defrel (val? o)
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
(defrel (env? o)
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
#|
(define =/=
  (λ (n1 n2)
    (conde
      ((== n1 'z)
       (fresh (n2-1)
         (== n2 `(s ,n2-1))))
      ((== n2 'z)
       (fresh (n1-1)
         (== n1 `(s ,n1-1))))
      ((fresh (n1-1 n2-1)
         (== n1 `(s ,n1-1))
         (== n2 `(s ,n2-1)))))))
|#

(defrel (=/= n₁ n₂)
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
(defrel (lookup e i v)
  (fresh (j vj er)
    (conj
     (== e `((,j . ,vj) . ,er))
     (disj
      (conj (== i j)
            (== v vj))
      (conj (=/= i j)
            (lookup er i v))))))

;;; Evaluation
(defrel (valof e t v)
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

;;; Normalization (of terms, so that they can be valofuated in Scheme)
;; Our language is a subset of Scheme, except that we need to turn
;; our peano-encoded variables into Scheme symbols.

;; Normalizes the list (s ...(s <x>)) to s...s<x>,
;; where s can be applied 0 or more times to the symbol <x>,
;; which will be either z or an unbound logic variable such as _.0.
;; (define normalize-var-name
;;   (λ (n)
;;     (if (and (list? n) (eq? 's (car n)) (null? (cddr n)))
;;         (string->symbol (string-append
;;                          (symbol->string (car n))
;;                          (symbol->string (normalize-var-name (cadr n)))))
;;         (begin
;;           (assert (symbol? n))
;;           n))))

;; Normalizes all occurrences of (x <peano>) to a symbol.
;; (define normalize
;;   (λ (t)
;;     (if (list? t)
;;         (if (and (eq? 'x (car t)) (null? (cddr t)))
;;             (normalize-var-name (cadr t))
;;             (map normalize t))
;;         (begin
;;           (assert (not (eq? 'x t)))
;;           t))))

;;; Tests

;; (define ok
;;   (λ (r)
;;     (assert (not (null? r)))
;;     r))

;; Quine verification.
(define quine
  '((λ (x) (list₂ (x) (list₂ (quote quote) (x))))
    (quote (λ (x) (list₂ (x) (list₂ (quote quote) (x)))))))

(define validate-quine
  (thunk
   (void (run* (q) (valof '() quine `(code ,quine))))))

;; Quine generation.
(define 40quines
  (thunk
   (void (run 40 (q) (valof '() q `(code ,q))))))

;; cpu time: 301890 real time: 304926 gc time: 13996
(define 5twines
 (thunk
  (void
   (run 5 (q)
        (fresh (a b)
               (conj
                (== q `(,a ,b))
                (valof '() a `(code ,b))
                (valof '() b `(code ,a))))))))

;; cpu time: 50114 real time: 50652 gc time: 4291
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

(define 2thrines
  (thunk
   (void
    (run 2 (q)
         (fresh (a b c)
                (conj
                 (== q `(,a ,b ,c))
                 (valof '() a `(code ,b))
                 (valof '() b `(code ,c))
                 (valof '() c `(code ,a))))))))

;;; shadowing tests
(run 1 (q)
  (valof '()
      '(((λ (x) (λ (x) (x))) (quote quote)) (quote lambda))
      q))

(run 1 (q)
  (valof '()
      '(((λ (x) (λ (x s) (x))) (quote lambda)) (quote quote))
      q))


(run 1 (q)
  (valof '()
      q
      '(code (I love you))))

(test-equal?
 "Works with a single goal"
 (with-output-to-string
   (λ ()
     ((conj (λ (s)
              (displayln "first")
              (list s)))
      'cat)))
 "first\n")
