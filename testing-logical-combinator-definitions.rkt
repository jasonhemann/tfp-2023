#lang racket
(require (except-in rackunit fail))
(require "./logical-combinator-function-definitions.rkt")
;; left to right
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-2+-left-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" macros-2+-right-assoc))
;; (require (submod "./logical-combinator-function-definitions.rkt" varargs-2+-left-assoc))
(require (submod "./logical-combinator-function-definitions.rkt" varargs-2+-right-assoc))
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

(define conj-output-string
  (with-output-to-string
    (λ ()
      (begin
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
         'cat)
       (void)))))

(match conj-output-string
  ["first\nsecond\nthird\nfourth\nfifth\n" (printf "conjunctions evaluate from left to right\n")]
  ["fifth\nfourth\nthird\nsecond\nfirst\n" (printf "conjunctions evaluate from right to left\n")]
  [else (printf "conjunctions neither go left to right nor right to left instead~n ~s ~n" conj-output-string)])


(define disj-output-string
  (with-output-to-string
    (λ ()
      ((disj
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

(match disj-output-string
  ["first\nsecond\nthird\nfourth\nfifth\n" (printf "disjunctions evaluate from left to right\n")]
  ["fifth\nfourth\nthird\nsecond\nfirst\n" (printf "disjunctions evaluate from right to left\n")]
  [else (printf "disjunctions neither go left to right nor right to left instead~n ~s ~n") disj-output-string])

(define disj-result-string
  ((disj
    (λ (s)
      (list 'first))
    (λ (s)
      (list 'second))
    (λ (s)
      (list 'third))
    (λ (s)
      (list 'fourth))
    (λ (s)
      (list 'fifth)))
   'cat))

(printf "disj result string is ~s ~n" disj-result-string)

(define-relation (answer-of arg)
  (disj (λ (s/c) (list arg))
        (answer-of arg)))

(define result-stream
  (call/initial-state 10000
   (disj
    (answer-of 'first)
    (answer-of 'second)
    (answer-of 'third)
    (answer-of 'fourth)
    (answer-of 'fifth))))

(define firsts (length (filter (curry eqv? 'first) result-stream)))
(define seconds (length (filter (curry eqv? 'second) result-stream)))
(define thirds (length (filter (curry eqv? 'third) result-stream)))
(define fourths (length (filter (curry eqv? 'fourth) result-stream)))
(define fifths (length (filter (curry eqv? 'fifth) result-stream)))

(test-=
 (string-append "test that last element gets approximately half the time \n"
                "failure means last element does NOT get approximately half the time")
 (/ (* seconds (expt 2. 3)) fifths)
 1
 .01)

(test-=
 (string-append "test that last element gets approximately half the time \n"
                "failure means last element does NOT get approximately half the time")
 (/ (* fourths (expt 2. 3)) firsts)
 1
 .01)
