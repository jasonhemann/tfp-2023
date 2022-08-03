
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

(define ($append-map g $)
  (cond
   ((null? $) `())
   ((procedure? $) (lambda () ($append-map g (force $))))
   (else ($append (g (car $)) ($append-map g (cdr $))))))

(define-syntax define-relation
  (syntax-rules ()
	[(_ (defname . args) g)
	 (define (defname . args) (lambda (s/c) (lambda () (g s/c))))]))

(define (pull $) (if (procedure? $) (pull (force $)) $))

(define-relation (unpro1)
  (disj-variant-1 (unpro1)))

(define-relation (unpro2)
  (disj-variant-2 (unpro2)))

(define (disj-variant-1 g . gs)
  (lambda (s)
	(D (g s) gs s)))

(define (disj-variant-2 gs)
  (lambda (s)
	(cond
     ((null? gs) (succeed s))
     (else (D ((car gs) s) (cdr gs) s)))))

(define (D s∞ gs s)
  (cond
   ((null? gs) s∞)
   (else
    ($append s∞
			 (D ((car gs) s) (cdr gs) s)))))

(define e1 (make-engine (lambda () (pull ((unpro1) '(() . 0))))))
(define e2 (make-engine (lambda () (pull ((unpro2) '(() . 0))))))

;; (e1 10 (lambda (_) (error 'disj-test "you finished an infinite loop")) (lambda (_) (printf "finished with this")))
