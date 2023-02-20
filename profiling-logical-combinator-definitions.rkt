#lang racket
(require profile)
(require profile/render-graphviz)
(require (rename-in "./logical-combinator-function-definitions.rkt"
                    ($append append∞)
                    ($append-map append-map∞)))

;; This file attempts to show the additional cost of a null? test to
;; check for no goals.

(defrel (unpro1)
  (disj-variant-1 (unpro1)))

(defrel (unpro2)
  (disj-variant-2 (unpro2)))

  (define ((disj-variant-1 g . gs) s)
    (D (g s) gs s))

  (define ((disj-variant-2 gs) s)
    (cond
      ((null? gs) (succeed s))
      (else (D ((car gs) s) (cdr gs) s))))

  (define (D s∞ gs s)
    (cond
      ((null? gs) s∞)
      (else
       (append∞ s∞
         (D ((car gs) s) (cdr gs) s)))))

(define prof1 (profile-thunk (λ () (pull ((unpro1) '(() . 0))))))
(define prof2 (profile-thunk (λ () (pull ((unpro2) '(() . 0))))))
