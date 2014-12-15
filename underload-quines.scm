;; generating quines in http://esolangs.org/wiki/Underload

(load "../../mk-implementations/scheme/mk-chicken.scm")

(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))])))

(define (revappo x y z)
  (conde
   [(== x '())
    (== y z)]
   [(fresh (p ps)
           (== x `(,p . ,ps))
           (revappo ps (cons p y) z))]))

(define (ul steps input stack output)
  (fresh (step)
         (== steps `(o . ,step))
  (fresh (x ys stack2)
  (conde
   [(== input `(dup . ,ys))
    (== stack `(,x . ,stack2))
    (ul step ys `(,x ,x . ,stack2) output)]
   [(== input `(wrap . ,ys))
    (== stack `(,x . ,stack2))
    (ul step ys `((,x) . ,stack2) output)]
   [(== input `(pop . ,ys))
    (== stack `(,x . ,stack2))
    (fresh (output2)
           ;;(== output `(,x . ,output2))
           (appendo x output2 output)
           (ul step ys stack2 output2))]
   [(== input `(get . ,ys))
    (== stack `(,x . ,stack2))
    (fresh (program)
           (appendo x ys program)
           (ul step program stack2 output))] 
   [(fresh (p q)
      (== input `((,p . ,q) . ,ys))
      (fresh (program)
             ;;(appendo `(,p . ,q) stack stack2)
             (== `((,p . ,q) . ,stack) stack2)
       (ul step ys stack2 output)))]
   [(== input '())
    (== output '())]))))

;; #;1> (for-each print (run 10 (q) (ul '(o o o o o o o o) q '() q)))
;; ()
;; ((dup wrap pop pop) dup wrap pop pop)
;; ((wrap dup pop get pop) wrap dup pop get pop)
;; ((dup wrap pop dup dup pop) dup wrap pop dup dup pop)
;; ((dup dup wrap pop dup pop) dup dup wrap pop dup pop)
;; ((dup dup dup wrap pop pop) dup dup dup wrap pop pop)
;; (((dup wrap pop get pop)) dup wrap pop get pop)
;; ((wrap pop (dup get pop)) dup get pop)
;; ((dup wrap pop pop (_.0 . _.1)) dup wrap pop pop (_.0 . _.1))
;; (((dup wrap pop get) pop) dup wrap pop get)

;; corresponds to the following quines
;; which I tested and they work..

;; (:aSS):aSS
;; (a:S^S)a:S^S
;; (:aS::S):aS::S
;; (::aS:S)::aS:S
;; (:::aSS):::aSS
;; ((:aS^S)):aS^S
;; (aS(:^S)):^S
;; (:aSS(_.0._.1)):aSS(_.0._.1)
;;  e.g.  (:aSS(quine)):aSS(quine)
;; ((:aS^)S):aS^

