#lang racket

(struct atom [predicate terms])

(define (vars atm)
  (match atm
    [(atom _ terms)
     (for/list ([term terms]
                #:when (symbol? term))
       term)]))

(vars (atom 'R (list 'x 1)))