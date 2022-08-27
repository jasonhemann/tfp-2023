#lang racket
(require "../interface-definitions.rkt")
;; (require minikanren (only-in racket [define define-relation]))
(provide (all-defined-out))

(define-syntax test-compare-unreified
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (begin
           (printf "Expected:~n")
           (pretty-print expected)
           (printf "Computed:~n")
           (pretty-print produced)))))))


(define-relation (appendo l s out)
  (conde
   [(== '() l) (== s out)]
   [(fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res))]))

(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons #t
         (build-num (quotient (- n 1) 2))))
      ((and (not (zero? n)) (even? n))
       (cons #f
         (build-num (quotient n 2))))
      ((zero? n) '()))))


(define-relation (zeroo n)
  (== '() n))


(define-relation (poso n)
  (fresh (a d)
         (== `(,a . ,d) n)))


(define-relation (>1o n)
  (fresh (a ad dd)
         (== `(,a ,ad . ,dd) n)))


(define-relation (full-addero b x y r c)
  (conde
   ((== #f b) (== #f x) (== #f y) (== #f r) (== #f c))
   ((== #t b) (== #f x) (== #f y) (== #t r) (== #f c))
   ((== #f b) (== #t x) (== #f y) (== #t r) (== #f c))
   ((== #t b) (== #t x) (== #f y) (== #f r) (== #t c))
   ((== #f b) (== #f x) (== #t y) (== #t r) (== #f c))
   ((== #t b) (== #f x) (== #t y) (== #f r) (== #t c))
   ((== #f b) (== #t x) (== #t y) (== #f r) (== #t c))
   ((== #t b) (== #t x) (== #t y) (== #t r) (== #t c))))


(define-relation (addero d n m r)
  (conde
   ((== #f d) (== '() m) (== n r))
   ((== #f d) (== '() n) (== m r)
              (poso m))
   ((== #t d) (== '() m)
              (addero #f n '(#t) r))
   ((== #t d) (== '() n) (poso m)
              (addero #f '(#t) m r))
   ((== '(#t) n) (== '(#t) m)
                 (fresh (a c)
                        (== `(,a ,c) r)
                        (full-addero d #t #t a c)))
   ((== '(#t) n) (gen-addero d n m r))
   ((== '(#t) m) (>1o n) (>1o r)
                 (addero d '(#t) n r))
   ((>1o n) (gen-addero d n m r))))


(define-relation (gen-addero d n m r)
  (fresh (a b c e x y z)
         (== `(,a . ,x) n)
         (== `(,b . ,y) m) (poso y)
         (== `(,c . ,z) r) (poso z)
         (full-addero d a b c e)
         (addero e x y z)))


(define-relation (pluso n m k)
  (addero #f n m k))


(define-relation (minuso n m k)
  (pluso m k n))


(define-relation (*o n m p)
  (conde
   ((== '() n) (== '() p))
   ((poso n) (== '() m) (== '() p))
   ((== '(#t) n) (poso m) (== m p))
   ((>1o n) (== '(#t) m) (== n p))
   ((fresh (x z)
           (== `(#f . ,x) n) (poso x)
           (== `(#f . ,z) p) (poso z)
           (>1o m)
           (*o x m z)))
   ((fresh (x y)
           (== `(#t . ,x) n) (poso x)
           (== `(#f . ,y) m) (poso y)
           (*o m n p)))
   ((fresh (x y)
           (== `(#t . ,x) n) (poso x)
           (== `(#t . ,y) m) (poso y)
           (odd-*o x n m p)))))


(define-relation (odd-*o x n m p)
  (fresh (q)
         (bound-*o q p n m)
         (*o x m q)
         (pluso `(#f . ,q) m p)))


(define-relation (bound-*o q p n m)
  (conde
   ((== '() q) (poso p))
   ((fresh (a0 a1 a2 a3 x y z)
           (== `(,a0 . ,x) q)
           (== `(,a1 . ,y) p)
           (conde
            ((== '() n)
             (== `(,a2 . ,z) m)
             (bound-*o x y z '()))
            ((== `(,a3 . ,z) n)
             (bound-*o x y z m)))))))


(define-relation (=lo n m)
  (conde
   ((== '() n) (== '() m))
   ((== '(#t) n) (== '(#t) m))
   ((fresh (a x b y)
           (== `(,a . ,x) n) (poso x)
           (== `(,b . ,y) m) (poso y)
           (=lo x y)))))


(define-relation (<lo n m)
  (conde
   ((== '() n) (poso m))
   ((== '(#t) n) (>1o m))
   ((fresh (a x b y)
           (== `(,a . ,x) n) (poso x)
           (== `(,b . ,y) m) (poso y)
           (<lo x y)))))


(define-relation (<=lo n m)
  (conde
   ((=lo n m))
   ((<lo n m))))


(define-relation (<o n m)
  (conde
   ((<lo n m))
   ((=lo n m)
    (fresh (x)
           (poso x)
           (pluso n x m)))))


(define-relation (<=o n m)
  (conde
   ((== n m))
   ((<o n m))))


(define-relation (/o n m q r)
  (conde
   ((== r n) (== '() q) (<o n m))
   ((== '(#t) q) (=lo n m) (pluso r m n)
                 (<o r m))
   ((<lo m n)
    (<o r m)
    (poso q)
    (fresh (nh nl qh ql qlm qlmr rr rh)
           (splito n r nl nh)
           (splito q r ql qh)
           (conde
            ((== '() nh)
             (== '() qh)
             (minuso nl r qlm)
             (*o ql m qlm))
            ((poso nh)
             (*o ql m qlm)
             (pluso qlm r qlmr)
             (minuso qlmr nl rr)
             (splito rr r '() rh)
             (/o nh m qh rh)))))))


(define-relation (splito n r l h)
  (conde
   ((== '() n) (== '() h) (== '() l))
   ((fresh (b n^)
           (== `(#f ,b . ,n^) n)
           (== '() r)
           (== `(,b . ,n^) h)
           (== '() l)))
   ((fresh (n^)
           (==  `(#t . ,n^) n)
           (== '() r)
           (== n^ h)
           (== '(#t) l)))
   ((fresh (b n^ a r^)
           (== `(#f ,b . ,n^) n)
           (== `(,a . ,r^) r)
           (== '() l)
           (splito `(,b . ,n^) r^ '() h)))
   ((fresh (n^ a r^)
           (== `(#t . ,n^) n)
           (== `(,a . ,r^) r)
           (== '(#t) l)
           (splito n^ r^ '() h)))
   ((fresh (b n^ a r^ l^)
           (== `(,b . ,n^) n)
           (== `(,a . ,r^) r)
           (== `(,b . ,l^) l)
           (poso l^)
           (splito n^ r^ l^ h)))))


(define-relation (logo n b q r)
  (conde
   ;; ((== '(#t) n) (poso b) (== '() q) (== '() r)) I don't think this is needed
   ((== '() q) (<o n b) (pluso r '(#t) n))
   ((== '(#t) q) (>1o b) (=lo n b) (pluso r b n))
   ((== '(#t) b) (poso q) (pluso r '(#t) n))
   ((== '() b) (poso q) (== r n))
   ((== '(#f #t) b)
    (fresh (a ad dd)
           (poso dd)
           (== `(,a ,ad . ,dd) n)
           (exp2 n '() q)
           (fresh (s)
                  (splito n dd r s))))
   ((fresh (a ad add ddd)
           (conde
            ((== '(#t #t) b))
            ((== `(,a ,ad ,add . ,ddd) b))))
    (<lo b n)
    (fresh (bw1 bw nw nw1 ql1 ql s)
           (exp2 b '() bw1)
           (pluso bw1 '(#t) bw)
           (<lo q n)
           (fresh (q1 bwq1)
                  (pluso q '(#t) q1)
                  (*o bw q1 bwq1)
                  (<o nw1 bwq1))
           (exp2 n '() nw1)
           (pluso nw1 '(#t) nw)
           (/o nw bw ql1 s)
           (pluso ql '(#t) ql1)
           (<=lo ql q)
           (fresh (bql qh s qdh qd)
                  (repeated-mul b ql bql)
                  (/o nw bw1 qh s)
                  (pluso ql qdh qh)
                  (pluso ql qd q)
                  (<=o qd qdh)
                  (fresh (bqd bq1 bq)
                         (repeated-mul b qd bqd)
                         (*o bql bqd bq)
                         (*o b bq bq1)
                         (pluso bq r n)
                         (<o n bq1)))))))


(define-relation (exp2 n b q)
  (conde
   ((== '(#t) n) (== '() q))
   ((>1o n) (== '(#t) q)
            (fresh (s)
                   (splito n b s '(#t))))
   ((fresh (q1 b2)
           (== `(#f . ,q1) q)
           (poso q1)
           (<lo b n)
           (appendo b `(#t . ,b) b2)
           (exp2 n b2 q1)))
   ((fresh (q1 nh b2 s)
           (== `(#t . ,q1) q)
           (poso q1)
           (poso nh)
           (splito n b s nh)
           (appendo b `(#t . ,b) b2)
           (exp2 nh b2 q1)))))


(define-relation (repeated-mul n q nq)
  (conde
   ((poso n) (== '() q) (== '(#t) nq))
   ((== '(#t) q) (== n nq))
   ((>1o q)
    (fresh (q1 nq1)
           (pluso q1 '(#t) q)
           (repeated-mul n q1 nq1)
           (*o nq1 n nq)))))


(define-relation (expo b q n)
  (logo n b q '()))


(define-relation (hot-dog ht)
  (conde
   ((== ht 'dog))
   ((fresh (d)
           (== `(hot . ,d) ht)
           (hot-dog d)))))

(test-compare-unreified "test 0"
  (run* (q) (== q 'dog))
  '(dog))

(test-compare-unreified "test 1"
  (run 10 (q) (hot-dog q))
  '(dog
    (hot . dog)
    (hot hot . dog)
    (hot hot hot . dog)
    (hot hot hot hot . dog)
    (hot hot hot hot hot . dog)
    (hot hot hot hot hot hot . dog)
    (hot hot hot hot hot hot hot . dog)
    (hot hot hot hot hot hot hot hot . dog)
    (hot hot hot hot hot hot hot hot hot . dog)))

(test-compare-unreified "test 1.5"
  (run* (q) (*o (build-num 2) (build-num 3) q))
  '((#f #t #t)))

(test-compare-unreified "test 2"
  (run* (q)
	(fresh (n m)
	  (*o n m (build-num 6))
	  (== `(,n ,m) q)))
  '(((#t) (#f #t #t)) ((#f #t #t) (#t)) ((#f #t) (#t #t)) ((#t #t) (#f #t))))

(test-compare-unreified "sums"
  (run 5 (q)
    (fresh (x y z)
      (pluso x y z)
      (== `(,x ,y ,z) q)))
  '((_.0 () _.0)
    (() (_.0 . _.1) (_.0 . _.1))
    ((#t) (#t) (#f #t))
    ((#t) (#f _.0 . _.1) (#t _.0 . _.1))
    ((#t) (#t #t) (#f #f #t))))

(test-compare-unreified "factors"
  (run* (q)
    (fresh (x y)
      (*o x y (build-num 24))
      (== `(,x ,y ,(build-num 24)) q)))
  '(((#t) (#f #f #f #t #t) (#f #f #f #t #t))
    ((#f #f #f #t #t) (#t) (#f #f #f #t #t))
    ((#f #t) (#f #f #t #t) (#f #f #f #t #t))
    ((#f #f #t) (#f #t #t) (#f #f #f #t #t))
    ((#f #f #f #t) (#t #t) (#f #f #f #t #t))
    ((#t #t) (#f #f #f #t) (#f #f #f #t #t))
    ((#f #t #t) (#f #f #t) (#f #f #f #t #t))
    ((#f #f #t #t) (#f #t) (#f #f #f #t #t))))


;; (test-compare-unreified "testc20.tex-4"
;; (run* (s)
;;   (fresh (x y)
;;     (bit-ando x y #t)
;;     (== `(,x ,y) s)))


;; `((#t #t))
;; )

;; (test-compare-unreified "testc20.tex-5"
;; (run* (r)
;;   (half-addero #t #t r #t))

;; (list #f))

;; (test-compare-unreified "testc20.tex-6"
;; (run* (s)
;;   (fresh (x y r c)
;;     (half-addero x y r c)
;;     (== `(,x ,y ,r ,c) s)))


;; `((#f #f #f #f)
;;  (#f #t #t #f)
;;  (#t #f #t #f)
;;  (#t #t #f #t))
;;   )


(test-compare-unreified "testc20.tex-7"
(run* (s)
  (fresh (r c)
    (full-addero #f #t #t r c)
    (== `(,r ,c) s)))

(list `(#f #t)))


(test-compare-unreified "testc20.tex-8"
(run* (s)
  (fresh (r c)
    (full-addero #t #t #t r c)
    (== `(,r ,c) s)))

(list `(#t #t)))

(test-compare-unreified "testc20.tex-9"
(run* (s)
  (fresh (b x y r c)
    (full-addero b x y r c)
    (== `(,b ,x ,y ,r ,c) s)))


`((#f #f #f #f #f)
 (#t #f #f #t #f)
 (#f #t #f #t #f)
 (#t #t #f #f #t)
 (#f #f #t #t #f)
 (#t #f #t #f #t)
 (#f #t #t #f #t)
 (#t #t #t #t #t))
)



(test-compare-unreified "testc20.tex-15"
(run* (q)
  (poso '(#f #t #t))
  (== #t q))

(list #t))

(test-compare-unreified "testc20.tex-16"
(run* (q)
  (poso '(#t))
  (== #t q))

(list #t))

(test-compare-unreified "testc20.tex-17"
(run* (q)
  (poso '())
  (== #t q))

`())

(test-compare-unreified "testc20.tex-18"
(run* (r)
  (poso r))

(list `(_.0 . _.1)))


(test-compare-unreified "testc20.tex-19"
(run* (q)
  (>1o '(#f #t #t))
  (== #t q))

(list #t))

(test-compare-unreified "testc20.tex-20"
(run* (q)
  (>1o '(#f #t))
  (== #t q))

`(#t))

(test-compare-unreified "testc20.tex-21"
(run* (q)
  (>1o '(#t))
  (== #t q))

`())

(test-compare-unreified "testc20.tex-22"
(run* (q)
  (>1o '())
  (== #t q))

`())

(test-compare-unreified "testc20.tex-23"
(run* (r)
  (>1o r))

(list
`(_.0 _.1 . _.2)
))


(test-compare-unreified "testc20.tex-24"
(run 3 (s)
  (fresh (x y r)
    (addero #f x y r)
    (== `(,x ,y ,r) s)))


`((_.0 () _.0)
 (() (_.0 . _.1) (_.0 . _.1))
 ((#t) (#t) (#f #t)))
 )

(test-compare-unreified "testc20.tex-25"
(run 22 (s)
  (fresh (x y r)
    (addero #f x y r)
    (== `(,x ,y ,r) s)))
'((_.0 () _.0)
  (() (_.0 . _.1) (_.0 . _.1))
  ((#t) (#t) (#f #t))
  ((#t) (#f _.0 . _.1) (#t _.0 . _.1))
  ((#t) (#t #t) (#f #f #t))
  ((#t) (#t #f _.0 . _.1) (#f #t _.0 . _.1))
  ((#f _.0 . _.1) (#t) (#t _.0 . _.1))
  ((#t) (#t #t #t) (#f #f #f #t))
  ((#t) (#t #t #f _.0 . _.1) (#f #f #t _.0 . _.1))
  ((#t #t) (#t) (#f #f #t))
  ((#f #t) (#f #t) (#f #f #t))
  ((#t) (#t #t #t #t) (#f #f #f #f #t))
  ((#t) (#t #t #t #f _.0 . _.1) (#f #f #f #t _.0 . _.1))
  ((#t #f _.0 . _.1) (#t) (#f #t _.0 . _.1))
  ((#t) (#t #t #t #t #t) (#f #f #f #f #f #t))
  ((#t) (#t #t #t #t #f _.0 . _.1) (#f #f #f #f #t _.0 . _.1))
  ((#t #t #t) (#t) (#f #f #f #t))
  ((#t) (#t #t #t #t #t #t) (#f #f #f #f #f #f #t))
  ((#t) (#t #t #t #t #t #f _.0 . _.1) (#f #f #f #f #f #t _.0 . _.1))
  ((#t #t #f _.0 . _.1) (#t) (#f #f #t _.0 . _.1))
  ((#t #t) (#f #t) (#t #f #t))
  ((#t) (#t #t #t #t #t #t #t) (#f #f #f #f #f #f #f #t))))





(test-compare-unreified "testc20.tex-26"
(run* (s)
  (gen-addero #t '(#f #t #t) '(#t #t) s))

(list `(#f #t #f #t)))

(test-compare-unreified "testc20.tex-27"
(run* (s)
  (fresh (x y)
    (addero #f x y '(#t #f #t))
    (== `(,x ,y) s)))


`(((#t #f #t) ())
 (() (#t #f #t))
 ((#t) (#f #f #t))
 ((#f #f #t) (#t))
 ((#t #t) (#f #t))
 ((#f #t) (#t #t)))
)


(test-compare-unreified "testc20.tex-28"
(run* (s)
  (fresh (x y)
    (pluso x y '(#t #f #t))
    (== `(,x ,y) s)))


`(((#t #f #t) ())
 (() (#t #f #t))
 ((#t) (#f #f #t))
 ((#f #f #t) (#t))
 ((#t #t) (#f #t))
 ((#f #t) (#t #t)))
)

(test-compare-unreified "testc20.tex-29"
(run* (q)
  (minuso '(#f #f #f #t) '(#t #f #t) q))


`((#t #t))
)

(test-compare-unreified "testc20.tex-30"
(run* (q)
  (minuso '(#f #t #t) '(#f #t #t) q))


`(())
)

(test-compare-unreified "testc20.tex-31"
(run* (q)
  (minuso '(#f #t #t) '(#f #f #f #t) q))


`()
)


(test-compare-unreified "testc21.tex-1"
(run 34 (t)
  (fresh (x y r)
    (*o x y r)
    (== `(,x ,y ,r) t)))
'((() _.0 ())
  ((_.0 . _.1) () ())
  ((#t) (_.0 . _.1) (_.0 . _.1))
  ((_.0 _.1 . _.2) (#t) (_.0 _.1 . _.2))
  ((#f #t) (_.0 _.1 . _.2) (#f _.0 _.1 . _.2))
  ((#f #f #t) (_.0 _.1 . _.2) (#f #f _.0 _.1 . _.2))
  ((#t _.0 . _.1) (#f #t) (#f #t _.0 . _.1))
  ((#f #f #f #t) (_.0 _.1 . _.2) (#f #f #f _.0 _.1 . _.2))
  ((#t _.0 . _.1) (#f #f #t) (#f #f #t _.0 . _.1))
  ((#f #t _.0 . _.1) (#f #t) (#f #f #t _.0 . _.1))
  ((#f #f #f #f #t) (_.0 _.1 . _.2) (#f #f #f #f _.0 _.1 . _.2))
  ((#t _.0 . _.1) (#f #f #f #t) (#f #f #f #t _.0 . _.1))
  ((#f #t _.0 . _.1) (#f #f #t) (#f #f #f #t _.0 . _.1))
  ((#f #f #t _.0 . _.1) (#f #t) (#f #f #f #t _.0 . _.1))
  ((#f #f #f #f #f #t) (_.0 _.1 . _.2) (#f #f #f #f #f _.0 _.1 . _.2))
  ((#t _.0 . _.1) (#f #f #f #f #t) (#f #f #f #f #t _.0 . _.1))
  ((#f #t _.0 . _.1) (#f #f #f #t) (#f #f #f #f #t _.0 . _.1))
  ((#f #f #t _.0 . _.1) (#f #f #t) (#f #f #f #f #t _.0 . _.1))
  ((#f #f #f #t _.0 . _.1) (#f #t) (#f #f #f #f #t _.0 . _.1))
  ((#f #f #f #f #f #f #t) (_.0 _.1 . _.2) (#f #f #f #f #f #f _.0 _.1 . _.2))
  ((#t _.0 . _.1) (#f #f #f #f #f #t) (#f #f #f #f #f #t _.0 . _.1))
  ((#f #t _.0 . _.1) (#f #f #f #f #t) (#f #f #f #f #f #t _.0 . _.1))
  ((#f #f #t _.0 . _.1) (#f #f #f #t) (#f #f #f #f #f #t _.0 . _.1))
  ((#f #f #f #t _.0 . _.1) (#f #f #t) (#f #f #f #f #f #t _.0 . _.1))
  ((#f #f #f #f #t _.0 . _.1) (#f #t) (#f #f #f #f #f #t _.0 . _.1))
  ((#t #t) (#t #t) (#t #f #f #t))
  ((#f #f #f #f #f #f #f #t)
   (_.0 _.1 . _.2)
   (#f #f #f #f #f #f #f _.0 _.1 . _.2))
  ((#t _.0 . _.1) (#f #f #f #f #f #f #t) (#f #f #f #f #f #f #t _.0 . _.1))
  ((#f #t _.0 . _.1) (#f #f #f #f #f #t) (#f #f #f #f #f #f #t _.0 . _.1))
  ((#f #f #t _.0 . _.1) (#f #f #f #f #t) (#f #f #f #f #f #f #t _.0 . _.1))
  ((#f #f #f #t _.0 . _.1) (#f #f #f #t) (#f #f #f #f #f #f #t _.0 . _.1))
  ((#t #t) (#t #f #t) (#t #t #t #t))
  ((#t #t) (#t #t #t) (#t #f #t #f #t))
  ((#f #f #f #f #t _.0 . _.1) (#f #f #t) (#f #f #f #f #f #f #t _.0 . _.1))))

(test-compare-unreified "testc21.tex-2"
(run* (p)
  (*o '(#f #t) '(#f #f #t) p))

(list `(#f #f #f #t)))


(test-compare-unreified "testc21.tex-3"
(run 1 (t)
  (fresh (n m)
    (*o n m '(#t))
    (== `(,n ,m) t)))

(list `((#t) (#t))))

(test-compare-unreified "testc21.tex-5"
(run 2 (t)
  (fresh (n m)
    (*o n m '(#t))
    (== `(,n ,m) t)))

`(((#t) (#t))))

(test-compare-unreified "testc21.tex-6"
(run* (p)
  (*o '(#t #t #t) '(#t #t #t #t #t #t) p))

(list `(#t #f #f #t #t #t #f #t #t)))

(test-compare-unreified "testc21.tex-7"
(run* (t)
  (fresh (w x y)
    (=lo `(#t ,w ,x . ,y) '(#f #t #t #f #t))
    (== `(,w ,x ,y) t)))

(list `(_.0 _.1 (_.2 #t))))

(test-compare-unreified "testc21.tex-8"
(run* (b)
  (=lo '(#t) `(,b)))

(list #t))

(test-compare-unreified "testc21.tex-9"
(run* (n)
  (=lo `(#t #f #t . ,n) '(#f #t #t #f #t)))

(list
`(_.0 #t)
))

(test-compare-unreified "testc21.tex-10"
(run 5 (t)
  (fresh (y z)
    (=lo `(#t . ,y) `(#t . ,z))
    (== `(,y ,z) t)))


`((() ())
 ((#t) (#t))
 ((_.0 #t) (_.1 #t))
 ((_.0 _.1 #t) (_.2 _.3 #t))
 ((_.0 _.1 _.2 #t) (_.3 _.4 _.5 #t)))
)

(test-compare-unreified "testc21.tex-11"
(run 5 (t)
  (fresh (y z)
    (=lo `(#t . ,y) `(#f . ,z))
    (== `(,y ,z) t)))


`(((#t) (#t))
 ((_.0 #t) (_.1 #t))
 ((_.0 _.1 #t) (_.2 _.3 #t))
 ((_.0 _.1 _.2 #t) (_.3 _.4 _.5 #t))
 ((_.0 _.1 _.2 _.3 #t) (_.4 _.5 _.6 _.7 #t)))
)

(test-compare-unreified "testc21.tex-12"
(run 5 (t)
  (fresh (y z)
    (=lo `(#t . ,y) `(#f #t #t #f #t . ,z))
    (== `(,y ,z) t)))


`(((_.0 _.1 _.2 #t) ())
 ((_.0 _.1 _.2 _.3 #t) (#t))
 ((_.0 _.1 _.2 _.3 _.4 #t) (_.5 #t))
 ((_.0 _.1 _.2 _.3 _.4 _.5 #t) (_.6 _.7 #t))
 ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 #t) (_.7 _.8 _.9 #t)))
)

(test-compare-unreified "testc21.tex-13"
(run 8 (t)
  (fresh (y z)
    (<lo `(#t . ,y) `(#f #t #t #f #t . ,z))
    (== `(,y ,z) t)))


`((() _.0)
 ((#t) _.0)
 ((_.0 #t) _.1)
 ((_.0 _.1 #t) _.2)
 ((_.0 _.1 _.2 #t) (_.3 . _.4))
 ((_.0 _.1 _.2 _.3 #t) (_.4 _.5 . _.6))
 ((_.0 _.1 _.2 _.3 _.4 #t) (_.5 _.6 _.7 . _.8))
 ((_.0 _.1 _.2 _.3 _.4 _.5 #t) (_.6 _.7 _.8 _.9 . _.10)))
)


(test-compare-unreified "testc21.tex-15"
(run 8 (t)
  (fresh (n m)
    (<=lo n m)
    (== `(,n ,m) t)))
'((() ())
  ((#t) (#t))
  (() (_.0 . _.1))
  ((#t) (_.0 _.1 . _.2))
  ((_.0 #t) (_.1 #t))
  ((_.0 #t) (_.1 _.2 _.3 . _.4))
  ((_.0 _.1 #t) (_.2 _.3 #t))
  ((_.0 _.1 #t) (_.2 _.3 _.4 _.5 . _.6)))
)

(test-compare-unreified "testc21.tex-16"
(run 1 (t)
  (fresh (n m)
    (<=lo n m)
    (*o n '(#f #t) m)
    (== `(,n ,m) t)))

(list `(() ())))

(test-compare-unreified "testc21.tex-17"
(run 10 (t)
  (fresh (n m)
    (<=lo n m)
    (*o n '(#f #t) m)
    (== `(,n ,m) t)))


`((() ())
 ((#t) (#f #t))
 ((#f #t) (#f #f #t))
 ((#t #t) (#f #t #t))
 ((#t _.0 #t) (#f #t _.0 #t))
 ((#f #f #t) (#f #f #f #t))
 ((#f #t #t) (#f #f #t #t))
 ((#t _.0 _.1 #t) (#f #t _.0 _.1 #t))
 ((#f #t _.0 #t) (#f #f #t _.0 #t))
 ((#f #f #f #t) (#f #f #f #f #t)))
)

(test-compare-unreified "testc21.tex-18"
(run 15 (t)
  (fresh (n m)
    (<=lo n m)
    (== `(,n ,m) t)))
'((() ())
  ((#t) (#t))
  (() (_.0 . _.1))
  ((#t) (_.0 _.1 . _.2))
  ((_.0 #t) (_.1 #t))
  ((_.0 #t) (_.1 _.2 _.3 . _.4))
  ((_.0 _.1 #t) (_.2 _.3 #t))
  ((_.0 _.1 #t) (_.2 _.3 _.4 _.5 . _.6))
  ((_.0 _.1 _.2 #t) (_.3 _.4 _.5 #t))
  ((_.0 _.1 _.2 #t) (_.3 _.4 _.5 _.6 _.7 . _.8))
  ((_.0 _.1 _.2 _.3 #t) (_.4 _.5 _.6 _.7 #t))
  ((_.0 _.1 _.2 _.3 #t) (_.4 _.5 _.6 _.7 _.8 _.9 . _.10))
  ((_.0 _.1 _.2 _.3 _.4 #t) (_.5 _.6 _.7 _.8 _.9 #t))
  ((_.0 _.1 _.2 _.3 _.4 #t) (_.5 _.6 _.7 _.8 _.9 _.10 _.11 . _.12))
  ((_.0 _.1 _.2 _.3 _.4 _.5 #t) (_.6 _.7 _.8 _.9 _.10 _.11 #t)))
)

(test-compare-unreified "testc21.tex-19"
(run* (q)
  (<o '(#t #f #t) '(#t #t #t))
  (== #t q))

(list #t))

(test-compare-unreified "testc21.tex-20"
(run* (q)
  (<o '(#t #t #t) '(#t #f #t))
  (== #t q))

`())

(test-compare-unreified "testc21.tex-21"
(run* (q)
  (<o '(#t #f #t) '(#t #f #t))
  (== #t q))

`())

(test-compare-unreified "lessthanequalo-1"
  (run* (q)
    (<=o '(#t #f #t) '(#t #f #t))
    (== #t q))

`(#t))

(test-compare-unreified "testc21.tex-22"
(run 6 (n)
  (<o n `(#t #f #t)))


`(() (#t) (_.0 #t) (#f #f #t))
)

(test-compare-unreified "testc21.tex-23"
(run 6 (m)
  (<o `(#t #f #t) m))


`((_.0 _.1 _.2 _.3 . _.4) (#f #t #t) (#t #t #t))
)

(test-compare-unreified "testc21.tex-25"
(run 6 (t)
  (fresh (n m q r)
    (/o n m q r)
    (== `(,n ,m ,q ,r) t)))


`((() (_.0 . _.1) () ())
 ((#t) (_.0 _.1 . _.2) () (#t))
 ((_.0 #t) (_.1 _.2 _.3 . _.4) () (_.0 #t))
 ((_.0 _.1 #t) (_.2 _.3 _.4 _.5 . _.6) () (_.0 _.1 #t))
 ((_.0 _.1 _.2 #t) (_.3 _.4 _.5 _.6 _.7 . _.8) () (_.0 _.1 _.2 #t))
 ((_.0 _.1 _.2 _.3 #t) (_.4 _.5 _.6 _.7 _.8 _.9 . _.10) () (_.0 _.1 _.2 _.3 #t)))
)

(test-compare-unreified "testc21.tex-26"
(run* (r)
  (logo '(#f #t #t #t) '(#f #t) '(#t #t) r))

(list `(#f #t #t)))

(test-compare-unreified "logo-test1"
  (run* (q) (logo (build-num 1) (build-num 2) (build-num 0) (build-num 0)))
  '(_.0))

(test-compare-unreified "logo-test2"
  (run 10 (q) (fresh (a b c d) (logo a b c d) (== `(,a ,b ,c ,d) q)))
  '((_.0 () (_.1 . _.2) _.0)
    ((#t) (#t) (_.0 . _.1) ())
    ((#t) (_.0 _.1 . _.2) () ())
    ((_.0 #t) (_.0 #t) (#t) ())
    ((#f #t) (_.0 _.1 _.2 . _.3) () (#t))
    ((#f #t) (#t) (_.0 . _.1) (#t))
    ((_.0 _.1 #t) (_.0 _.1 #t) (#t) ())
    ((#t #t) (_.0 _.1 _.2 . _.3) () (#f #t))
    ((#t _.0 . _.1) (#t) (_.2 . _.3) (#f _.0 . _.1))
    ((#t _.0 #t) (_.1 _.2 _.3 _.4 . _.5) () (#f _.0 #t))))

(time (test-compare-unreified "testc21.tex-27"
(run 9 (s)
  (fresh (b q r)
    (logo '(#f #f #t #f #f #f #t) b q r)
    (>1o q)
    (== `(,b ,q ,r) s)))
`((() (_.0 _.1 . _.2) (#f #f #t #f #f #f #t))
 ((#t) (_.0  _.1 . _.2) (#t #t #f #f #f #f #t))
 ((#f #t) (#f #t #t) (#f #f #t))
 ((#t #t) (#t #t) (#t #f #f #t #f #t))
 ((#f #f #t) (#t #t) (#f #f #t))
 ((#f #f #f #t) (#f #t) (#f #f #t))
 ((#t #f #t) (#f #t) (#t #t #f #t #f #t))
 ((#f #t #t) (#f #t) (#f #f #f #f #f #t))
 ((#t #t #t) (#f #t) (#t #t #f #f #t)))))

(time
 (test-compare-unreified "testc21.tex-28"
   (run* (t)
     (expo '(#t #t) '(#t #f #t) t))
   (list `(#t #t #f #f #t #t #t #t))))
