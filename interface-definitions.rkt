#lang racket
(require "./logical-combinator-function-definitions.rkt")
(require (for-syntax syntax/parse))
(provide run conde fresh == =/= define-relation)
;; left  associative conjunction ((((a & b) & c) & d) & e)
;; right associative conjunction (a & (b & (c & (d & e))))
;;
;; execute goals from leftmost argument to rightmost argument
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-left-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-right-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-left-assoc))
;; THE BAD ONE
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-right-assoc))
;;
;; execute goals from rightmost argument to leftmost argument
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-left-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-1+-right-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-left-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-1+-right-assoc-flip))
;;
;; mixed
;; THE GOOD ONE
(require (submod "./logical-combinator-function-definitions.rkt" varargs-conj-left-disj-right))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-conj-left-disj-right-flip))

(define-syntax-rule (run n (q) g0 g ...)
  (call/initial-state
   n
   (fresh (q) g0 g ...)))

(define-syntax-rule (conde (g00 g01 ...) (gn0 gn1 ...) ...)
  (disj
    (conj g00 g01 ...)
    (conj gn0 gn1 ...)
    ...))

(define-syntax fresh
  (λ (stx)
    (syntax-parse stx
      [(_ (x ...) g0 g ...)
       (let ((n (length (syntax->list #'(x ...)))))
         #`(λ (st)
             (let* ((c (state->ct st))
                    (nc (+ #,n c)))
               ((apply
                 (λ (x ...) (conj g0 g ...))
                 (range c nc))
                (state (state->σ st) (state->≠ st) nc)))))])))

(module+ test
  (require (except-in rackunit fail))
  (run 1 (q) (== q 'cat))
  (run 1 (q) (fresh (x y z) (== q (list 'x y z))))
  (run 2 (q) (conde ((== q 'cat)) ((== q 'dog)))))
