#lang racket
(require minikanren (only-in racket [define define-relation]))
;; (require "../interface-definitions.rkt")
(require "./functional-graph-split.rkt")
(require (prefix-in australia: "./australia.rkt"))
(require (prefix-in america: "./america.rkt"))
(require (prefix-in canada: "./canada.rkt"))
(require (prefix-in middle-earth: "./middle-earth.rkt"))
(require (prefix-in mexico: "./mexico.rkt"))
(require (prefix-in iberia: "./iberia.rkt"))
(require (prefix-in south-america: "./south-america.rkt"))
(require (prefix-in kazakhstan: "./kazakhstan.rkt"))

(define-relation (membero x l)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car))
	   ((membero x cdr)))))

(define-relation (not-membero x l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (=/= x car)
	    (not-membero x cdr)))))

(define-relation (appendo xs ys zs)
  (conde ((== xs '()) (== ys zs))
	 ((fresh (x-head x-tail z-tail)
	    (== xs `(,x-head . ,x-tail))
	    (== zs `(,x-head . ,z-tail))
	    (appendo x-tail ys z-tail)))))

(define-relation (selecto x l l-x)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car)
	    (== l-x cdr))
	   ((fresh (cdr-x)
	      (== l-x `(,car . ,cdr-x))
	      (selecto x cdr cdr-x))))))

(define-relation (mapo p l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (p car)
	    (mapo p cdr)))))

;; (define-relation (mapo p l [acc (== 'cat 'cat)])
;;   (conde
;;    [(== l '())
;;     acc]
;;    [(fresh (car cdr)
;; 	   (== l `(,car . ,cdr))
;; 	   (mapo p cdr (fresh ()
;; 			      acc
;; 			      (p car))))]))

(define-relation (mapo2 p t l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (p t car)
	    (mapo2 p t cdr)))))

;; (define-relation (mapo2 p t l [acc (== 'cat 'cat)])
;;   (conde
;;    [(== l '())
;;     acc]
;;    [(fresh (car cdr)
;; 	   (== l `(,car . ,cdr))
;; 	   (mapo2 p t cdr (fresh ()
;; 			      acc
;; 			      (p t car))))]))

(define-relation (assoco key table value)
  (fresh (car table-cdr)
    (== table `(,car . ,table-cdr))
    (conde ((== `(,key . ,value) car))
	   ((assoco key table-cdr value)))))

(define-relation (same-lengtho l1 l2)
  (conde ((== l1 '()) (== l1 '()))
	 ((fresh (car1 cdr1 car2 cdr2)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (same-lengtho cdr1 cdr2)))))

(define-relation (make-assoc-tableo l1 l2 table)
  (conde ((== l1 '()) (== l1 '()) (== table '()))
	 ((fresh (car1 cdr1 car2 cdr2 cdr3)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (== table `((,car1 . ,car2) . ,cdr3))
	    (make-assoc-tableo cdr1 cdr2 cdr3)))))

(define-relation (coloro x)
  (membero x '(red green blue yellow)))

(define-relation (different-colors table constraint)
  (fresh (x y x-color y-color)
    (== constraint `(,x ,y))
    (assoco x table x-color)
    (assoco y table y-color)
    (=/= x-color y-color)))

;; (define (my-mapo p l i)
;;   ;; This has to be done in a depth first search!
;;   ;; (display (make-list i '-)) (newline)
;;   (conde/dfs ((== l '()))
;; 	     ((fresh (car cdr)
;; 		(== l `(,car . ,cdr))
;; 		(p car)
;; 		(my-mapo p cdr (+ i 1))))))

(define-relation (color states edges colors)
  ;; This is a simple constrained generate and test solver
  ;; The interesting part was the graph reduction preprocessing
  ;; stage.
  (fresh (table)
    ;; make a list to hold the color of each state
    (make-assoc-tableo states colors table)

    ;; make sure each color is different to neighbours
    (mapo2 different-colors table edges)

    ;; brute force search for a valid coloring
    (mapo coloro colors)))

(define (do-australia)
  (let ((nodes (graph-good-ordering australia:nodes australia:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes australia:edges q))))

(define (do-canada)
  (let ((nodes (graph-good-ordering canada:nodes canada:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes canada:edges q))))

(define (do-iberia)
  (let ((nodes (graph-good-ordering iberia:nodes iberia:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes iberia:edges q))))

(define (do-south-america)
  (let ((nodes (graph-good-ordering south-america:nodes south-america:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes south-america:edges q))))

(define (do-kazakhstan)
  (let ((nodes (graph-good-ordering kazakhstan:nodes kazakhstan:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes kazakhstan:edges q))))

(define (do-mexico)
  (let ((nodes (graph-good-ordering mexico:nodes mexico:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes mexico:edges q))))

(define (do-middle-earth)
  (let ((nodes (graph-good-ordering middle-earth:nodes middle-earth:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes middle-earth:edges q))))

(define (do-america)
  (let ((nodes (graph-good-ordering america:nodes america:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes america:edges q))))


(module+ test

 (define loop-count 10000000)

 (define (test-loop-f f)
   (let loop ([i loop-count])
     (if (= i 0)
         (void)
         (let ([res (f)])
           (and res
                (loop (- i 1)))))))

 (define-syntax test-loop
   (syntax-rules () [(_ e) (test-loop-f (lambda () e))]))

 ;; (time (test-loop (do-australia)))
 ;; (time (test-loop (do-canada)))
 ;; (time (test-loop (do-america)))

 (time (do-iberia))
 (time (do-australia))
 (time (do-canada))
 (time (do-middle-earth))
 (time (do-south-america))
 (time (do-kazakhstan))
 (time (do-mexico))
 (time (do-america))


;; the bad one not on zoom
;; cpu time: 22 real time: 22 gc time: 7
;; cpu time: 82 real time: 83 gc time: 4
;; cpu time: 108114 real time: 110293 gc time: 81

;; the good one not on zoom
;; cpu time: 18 real time: 19 gc time: 7
;; cpu time: 68 real time: 69 gc time: 2
;; cpu time: 101372 real time: 103464 gc time: 64


;; SUSPICIOUS
;; the bad one on zoom
;; cpu time: 36 real time: 37 gc time: 7
;; cpu time: 174 real time: 179 gc time: 24
;; cpu time: 193304 real time: 202160 gc time: 161

;; the good one on zoom
;; cpu time: 31 real time: 31 gc time: 5
;; cpu time: 161 real time: 164 gc time: 8
;; cpu time: 212884 real time: 222584 gc time: 149



)
