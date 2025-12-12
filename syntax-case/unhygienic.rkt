#lang racket

(struct constant (val) #:transparent)
(struct variable (name) #:transparent)
(struct application (proc args) #:transparent)
(struct symbolic-data (expr) #:transparent)
(struct function (param body) #:transparent)
(struct macro-application (macro args) #:transparent)

(struct transformer (proc) #:transparent)
(struct var-binding () #:transparent)
(struct special-form () #:transparent)

(define (parse expr env)
  (match expr
    [(or (? number?) (? boolean?) (? string?) '())
     (constant expr)]
    [(? symbol? s) #:when (var-binding? (hash-ref env s #f))
                   (variable s)]
    [`(,e1 . ,e*) #:when (not (symbol? e1))
                  (application e1 e*)]
    [`(,(? symbol? s) . ,e*) #:when (var-binding? (hash-ref env s #f))
                             (application s e*)]
    [`(,(? symbol? s) . ,e*) #:when (transformer? (hash-ref env s #f))
                             (macro-application s e*)]
    [`(quote ,e) #:when (special-form? (hash-ref env 'quote #f))
                 (symbolic-data e)]
    [`(lambda (,param) ,body) #:when (special-form? (hash-ref env 'lambda #f))
                              (function param body)]))

(define (expand expr env)
  (match (parse expr env)
    [(constant c) (symbolic-data c)]
    [(variable s) (variable s)]
    [(application e1 e*) (application (expand e1 env) (for/list ([e e*]) (expand e env)))]
    [(symbolic-data e) (symbolic-data e)]
    [(function s e) (function s (expand e (hash-set env s (var-binding))))]
    [(macro-application s e*)
     (match-define (transformer t) (hash-ref env s))
     (expand (t `(,s . ,e*)) env)]))

(define initial-env
  (hash 'quote (special-form)
        'lambda (special-form)))

(define (as-surface-syntax expr)
  (match expr
    [(constant c) c]
    [(variable s) s]
    [(application e1 e*) `(,(as-surface-syntax e1) . ,(map as-surface-syntax e*))]
    [(symbolic-data e) e]
    [(function s e) `(lambda (,s) ,(as-surface-syntax e))]))

(module+ test
  (require rackunit)
  
  (define or-transformer
    (transformer
     (lambda (stx)
       (match stx
         [`(,_ ,e1 ,e2)
          `((lambda (t)
              (if t t ,e2))
            ,e1)]))))

  (check-equal?
   (as-surface-syntax
    (expand
     '((lambda (t)
         (or #f t))
       #t)
     (hash-set
      (hash-set initial-env 'or or-transformer)
      'if (var-binding))))
   '((lambda (t)
       ((lambda (t)
          (if t t t)) #f))
     #t)))