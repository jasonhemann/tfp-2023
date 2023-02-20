#lang racket
(require (except-in rackunit fail))
(require "./logical-combinator-function-definitions.rkt")

;; This file is for testing the basic correctness of multiple
;; reimplementations of disj and conj.
;;
;; These implementations exclude the base case, and so avoid an
;; avoidable null? test. We also know as a result the nullary case is
;; in no way affecting the results.
;;
;; left  associative conjunction ((((a & b) & c) & d) & e)
;; right associative conjunction (a & (b & (c & (d & e))))
;;
;; Trying different versions is very low tech: comment and uncomment
;; requires

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

(define leftmost-conjunct-evaluated-first-rightmost-conjunct-evaluated-last?
  (equal? conj-output-string "first\nsecond\nthird\nfourth\nfifth\n"))
(define rightmost-conjunct-evaluated-first-leftmost-conjunct-evaluated-last?
  (equal? conj-output-string "fifth\nfourth\nthird\nsecond\nfirst\n"))

(when leftmost-conjunct-evaluated-first-rightmost-conjunct-evaluated-last?
  (printf "conjunctions evaluate from left to right\n"))

(when rightmost-conjunct-evaluated-first-leftmost-conjunct-evaluated-last?
  (printf "conjunctions evaluate from right to left\n"))

(when (not (or leftmost-conjunct-evaluated-first-rightmost-conjunct-evaluated-last?
               rightmost-conjunct-evaluated-first-leftmost-conjunct-evaluated-last?))
  (printf "conjunctions neither go left to right nor right to left instead~n ~s ~n" conj-output-string))

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
  [else (printf "disjunctions neither go left to right nor right to left instead~n ~s ~n" disj-output-string)])

(define disj-result
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

(define disj-result-left-to-right?
  (equal? disj-result '(first second third fourth fifth)))
(define disj-result-right-to-left?
  (equal? disj-result '(fifth fourth third second first)))

(when disj-result-left-to-right?
  (printf "disj answer stream comes from goals in left to right order\n"))

(when disj-result-right-to-left?
  (printf "disj answer stream comes from goals in right to left order\n"))

(defrel (answer-of arg)
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

(define (is-within-of n epsilon target)
  (<= (- target epsilon) n (+ target epsilon)))

(define last-disjunct-apprx-half?
  (is-within-of (/ (* seconds (expt 2. 3)) fifths) .01 1))
(define first-disjunct-apprx-half?
  (is-within-of (/ (* fourths (expt 2. 3)) firsts) .01 1))

(when first-disjunct-apprx-half?
  (printf "First disjunct gets approximately half the time.\n"))
(when last-disjunct-apprx-half?
  (printf "Last disjunct gets approximately half the time.\n"))

(test-equal?
 "Works with a single goal"
 (with-output-to-string
   (λ ()
     ((conj (λ (s)
              (displayln "first")
              (list s)))
      'cat)))
 "first\n")

(test-true
 "This combination of logical operators give give the properties we want"
 (and
  disj-result-left-to-right?
  first-disjunct-apprx-half?))
