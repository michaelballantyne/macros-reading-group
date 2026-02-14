#lang racket

(require "dsl.rkt")

(macrolog
 ([TC '()] [G (list '(1 2 #t) '(1 5 #f) '(2 3 #t) '(3 4 #t) '(4 5 #t) '(5 6 #t))])
 TC
 [(TC x y) <-- (G x y #t)]
 [(TC x z) <-- (TC x y) (TC (== y) z)])

#;(let ([G (make-table initial-G)]
        [TC (make-table)])
    (run
        (list G TC)
      (list
       (lambda () ;; TC(x,y) :- G(x,y,#t)
         (for ([row (in-table G)])
           (match row
             [(list x y #t)
              (add-row! TC (list x y))]
             [_ (void)])))
       (lambda () ;; TC(x,z) :- TC(x,y) TC(y,z)
         (for ([row1 (in-table TC)])
           (match row1
             [(list x y)
              (for ([row2 (in-table TC)])
                (match row2
                  [(list (== y) z)
                   (add-row! TC (list x z))]
                  [_ (void)]))]
             [_ (void)])))))

    (print-table TC))