#lang racket
;; (require minikanren (only-in racket [define defrel]))
(require "../interface-definitions.rkt")
(require "./functional-graph-split.rkt")
(require (prefix-in australia: "./australia.rkt"))
(require (prefix-in america: "./america.rkt"))
(require (prefix-in canada: "./canada.rkt"))
(require (prefix-in middle-earth: "./middle-earth.rkt"))
(require (prefix-in mexico: "./mexico.rkt"))
(require (prefix-in iberia: "./iberia.rkt"))
(require (prefix-in south-america: "./south-america.rkt"))
(require (prefix-in kazakhstan: "./kazakhstan.rkt"))
(require (prefix-in ireland: "./ireland.rkt"))

(defrel (membero x l)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car))
	   ((membero x cdr)))))

(defrel (not-membero x l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (=/= x car)
	    (not-membero x cdr)))))

(defrel (appendo xs ys zs)
  (conde ((== xs '()) (== ys zs))
	 ((fresh (x-head x-tail z-tail)
	    (== xs `(,x-head . ,x-tail))
	    (== zs `(,x-head . ,z-tail))
	    (appendo x-tail ys z-tail)))))

(defrel (selecto x l l-x)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car)
	    (== l-x cdr))
	   ((fresh (cdr-x)
	      (== l-x `(,car . ,cdr-x))
	      (selecto x cdr cdr-x))))))

(defrel (mapo p l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (p car)
	    (mapo p cdr)))))

;; (defrel (mapo p l [acc (== 'cat 'cat)])
;;   (conde
;;    [(== l '())
;;     acc]
;;    [(fresh (car cdr)
;; 	   (== l `(,car . ,cdr))
;; 	   (mapo p cdr (fresh ()
;; 			      acc
;; 			      (p car))))]))

(defrel (mapo2 p t l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (p t car)
	    (mapo2 p t cdr)))))

;; (defrel (mapo2 p t l [acc (== 'cat 'cat)])
;;   (conde
;;    [(== l '())
;;     acc]
;;    [(fresh (car cdr)
;; 	   (== l `(,car . ,cdr))
;; 	   (mapo2 p t cdr (fresh ()
;; 			      acc
;; 			      (p t car))))]))

(defrel (assoco key table value)
  (fresh (car table-cdr)
    (== table `(,car . ,table-cdr))
    (conde ((== `(,key . ,value) car))
	   ((assoco key table-cdr value)))))

(defrel (same-lengtho l1 l2)
  (conde ((== l1 '()) (== l1 '()))
	 ((fresh (car1 cdr1 car2 cdr2)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (same-lengtho cdr1 cdr2)))))

(defrel (make-assoc-tableo l1 l2 table)
  (conde ((== l1 '()) (== l1 '()) (== table '()))
	 ((fresh (car1 cdr1 car2 cdr2 cdr3)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (== table `((,car1 . ,car2) . ,cdr3))
	    (make-assoc-tableo cdr1 cdr2 cdr3)))))

(defrel (coloro x)
  (membero x '(red green blue yellow)))

(defrel (different-colors table constraint)
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

(defrel (color states edges colors)
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

(define (do-ireland)
  (let ((nodes (graph-good-ordering ireland:nodes ireland:edges)))
    (display nodes)(newline)
    (run 1 (q) (color nodes ireland:edges q))))

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

 (time (void (do-iberia)))
 (time (void (do-australia)))
 (time (void (do-canada)))
 (time (void (do-middle-earth)))
 (time (void (do-south-america)))
 (time (void (do-kazakhstan)))
 (time (void (do-ireland)))
 ;; (time (do-mexico))
 ;; (time (do-america))

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


;; the bad one augmented w/more tests
;; (spain portugal cerdagne andorra gibraltar)
;; cpu time: 4 real time: 4 gc time: 0
;; (south-australia western-australia northern-territory queensland new-south-wales victoria tasmania)
;; cpu time: 47 real time: 48 gc time: 10
;; (northwest-territories quebec ontario british-columbia manitoba alberta saskatchewan yukon nunavut newfoundland-and-labrador new-brunswick nova-scotia prince-edward-island)
;; cpu time: 3940 real time: 3992 gc time: 127
;; (rhun eriador rhovanion rohan mordor khand lindon forodwaith enedwaith gondor harad)
;; cpu time: 1354 real time: 1373 gc time: 23
;; (brazil argentina bolivia peru colombia chile paraguay uruguay venezuela guyana surinam french-guiana ecuador)
;; cpu time: 4228 real time: 4318 gc time: 66
;; (karaganda ulytau kostanay pavlodar aktobe akmola jambyl abai mangystau jetisu east-kazakhstan kyzylorda almaty west-kazakhstan north-kazakhstan turkistan atyrau)
;; cpu time: 51264 real time: 52201 gc time: 1883

;; the good one augmented w/more tests
;; (spain portugal cerdagne andorra gibraltar)
;; cpu time: 16 real time: 16 gc time: 11
;; (south-australia western-australia northern-territory queensland new-south-wales victoria tasmania)
;; cpu time: 58 real time: 58 gc time: 5
;; (northwest-territories quebec ontario british-columbia manitoba alberta saskatchewan yukon nunavut newfoundland-and-labrador new-brunswick nova-scotia prince-edward-island)
;; cpu time: 6768 real time: 6871 gc time: 159
;; (rhun eriador rhovanion rohan mordor khand lindon forodwaith enedwaith gondor harad)
;; cpu time: 2499 real time: 2563 gc time: 32
;; (brazil argentina bolivia peru colombia chile paraguay uruguay venezuela guyana surinam french-guiana ecuador)
;; cpu time: 8508 real time: 8725 gc time: 97
;; (karaganda ulytau kostanay pavlodar aktobe akmola jambyl abai mangystau jetisu east-kazakhstan kyzylorda almaty west-kazakhstan north-kazakhstan turkistan atyrau)
;; cpu time: 91346 real time: 93564 gc time: 1400


)
