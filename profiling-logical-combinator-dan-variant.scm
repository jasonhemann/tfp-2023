;; Chez Scheme file.
;;
;; I know how to use the hotspot profiler for Chez, so that’s what I’m
;; doing.
;;
;; This file attempts to show the additional cost of a null? test to
;; check for no goals.

(define ($append $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () ($append $2 (force $1))))
   (else (cons (car $1) ($append (cdr $1) $2)))))

(define-syntax define-relation
  (syntax-rules ()
	[(_ (defname . args) g)
	 (define (defname . args) (lambda (s/c) (lambda () (g s/c))))]))

(define (pull $) (if (procedure? $) (pull (force $)) $))

(define-relation (unpro2)
  (disj-variant-2 (unpro2)))

(define success (lambda (s) (list s)))

(define (disj-variant-2 . gs)
  (lambda (s)
	(cond
     ((null? gs) (success s))
     (else (D ((car gs) s) (cdr gs) s)))))

(define (D s-inf gs s)
  (cond
   ((null? gs) s-inf)
   (else
    ($append s-inf
			 (D ((car gs) s) (cdr gs) s)))))

(define e2 (make-engine (lambda () (pull ((unpro2) '(() . 0))))))

;; (parameterize ([compile-profile 'source]) (load "profiling-logical-combinator-dan-variant.scm"))
;; (e2 100000000 (lambda (_) (error 'disj-test "you finished an infinite loop")) (lambda (_) #t))
;; (profile-dump-html)
