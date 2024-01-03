#lang scheme
(require (planet dherman/memoize:3:1))

(define ((f q) x) (- 1 (/ x q)))

(define (fs q) (map (f q) (build-list 50 add1)))

(define/memo (pHits hits ps)
  (cond
    [(and (zero? hits) (empty? ps)) 1]
    [(empty? ps) 0]
    [(zero? hits) (* (- 1 (first ps)) (pHits 0 (rest ps)))]
    [else (+ (* (first ps) (pHits (sub1 hits) (rest ps)))
             (* (- 1 (first ps)) (pHits hits (rest ps))))]))

(define (p20 q) (pHits 20 (fs q)))

(define (find dst lower upper)
  (let* ([new-q (average lower upper)]
         [val (p20 new-q)])
    (cond
      [(= (exact->inexact val) dst) new-q]
      [(> val dst) (find dst new-q upper)]
      [(< val dst) (find dst lower new-q)])))

(define (average a b) (/ (+ a b) 2))

(exact->inexact (find 0.02 52 53))
;52.6494571953