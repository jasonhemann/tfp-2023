#lang racket
(require rackunit)
;; left to right
(require (submod "./logical-combinator-function-definitions.rkt" macros-2+-left-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-2+-right-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-2+-left-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-2+-right-assoc))
;;
;; right to left
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-2+-left-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-2+-right-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-2+-left-assoc-flip))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-2+-right-assoc-flip))

;; Testing different implementations of underlying logical combinators
;; for miniKanren.
;;
;; I want to test that these actually _work_ as intended, and confirm
;; the order of conjunctions---that they actually behave as expected.
;;
;; One way to test _correctness_ is to test a miniKanren over them.
;; However, this is difficult because these definitions sit in the
;; middle of an mK implementation.
;;
;; I want to test several versions each using the same underlying
;; implementation portion, and same structure above, and I don’t know
;; how to do that just yet. So, what is the right way to organize that
;; code style.

;; (module+ test-alternative-arities

;;   (test-equal?
;;    "Supports a list of no goals"
;;    (with-output-to-string
;;      (λ ()
;;        ((conj)
;;         'cat)))
;;    "")

;;   (test-equal?
;;    "Works with a single goal"
;;    (with-output-to-string
;;      (λ ()
;;        ((conj (λ (s)
;;                 (displayln "first")
;;                 (list s)))
;;         'cat)))

;;    "first\n"))

(define output-string
  (with-output-to-string
    (λ ()
      ((conj
        (λ (s)
          (displayln "first")
          (list s))
        (λ (s)
          (displayln "second")
          (list s))
        (λ (s)
          (displayln "third")
          (list s))
        (λ (s)
          (displayln "fourth")
          (list s))
        (λ (s)
          (displayln "fifth")
          (list s)))
       'cat))))


(match output-string
  ["first\nsecond\nthird\nfourth\nfifth\n" (printf "conjunctions evaluate from left to right")]
  ["fifth\nfourth\nthird\nsecond\nfirst\n" (printf "conjunctions evaluate from right to left")]
  [else (printf "conjunctions neither go left to right nor right to left instead~n ~s ~n") output-string])
