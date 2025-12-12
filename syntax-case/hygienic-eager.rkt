#lang racket

(require racket/hash)

(struct constant (val) #:transparent)
(struct variable (name) #:transparent)
(struct application (proc args) #:transparent)
(struct symbolic-data (expr) #:transparent)
(struct function (param body) #:transparent)
(struct macro-application (macro args) #:transparent)

(struct transformer (proc) #:transparent)
(struct var-binding () #:transparent)
(struct special-form (form) #:transparent)

(define gensym-counter (make-parameter #f))

(define (gensym [sym 'g])
  (gensym-counter (+ 1 (gensym-counter)))
  (string->symbol (format "~a~a" sym (gensym-counter))))

(struct identifier [symbol marks quoted] #:transparent)

(define (quoted stx)
  (match stx
    [(or (? number?) (? boolean?) (? string?) '())
     stx]
    [(? identifier?)
     (identifier-quoted stx)]
    [(cons s1 s2)
     (cons (quoted s1) (quoted s2))]))

(define (resolve id)
  (identifier-symbol id))

(define (fresh-mark)
  (gensym 'm))

(define (mark-id id m)
  (match id
    [(identifier sym marks quoted)
     (define new-marks
       (if (member m marks)
           (remove m marks)
           (cons m marks)))
     (identifier sym new-marks quoted)]))

(define (mark-syntax stx m)
  (match stx
    [(or (? number?) (? boolean?) (? string?) '())
     stx]
    [(? identifier?)
     (mark-id stx m)]
    [(cons s1 s2)
     (cons (mark-syntax s1 m) (mark-syntax s2 m))]))

(define (subst stx id sym)
  (match stx
    [(or (? number?) (? boolean?) (? string?) '())
     stx]
    [(? identifier?)
     (if (equal? id stx)
         sym
         stx)]
    [(cons s1 s2)
     (cons (subst s1 id sym) (subst s2 id sym))]))

(define (parse expr env)
  (match expr
    [(or (? number?) (? boolean?) (? string?) '())
     (constant expr)]
    [(? identifier? i) #:when (var-binding? (hash-ref env (resolve i) #f))
     (variable i)]
    [`(,e1 . ,e*) #:when (not (identifier? e1))
     (application e1 e*)]
    [`(,(? identifier? i) . ,e*) #:when (var-binding? (hash-ref env (resolve i) #f))
     (application i e*)]
    [`(,(? identifier? i) . ,e*) #:when (transformer? (hash-ref env (resolve i) #f))
     (macro-application i e*)]
    [`(,quote-id ,e)
     #:when (equal? (special-form 'quote) (hash-ref env (resolve quote-id) #f))
     (symbolic-data (quoted e))]
    [`(,lambda-id (,param) ,body)
     #:when (equal? (special-form 'lambda) (hash-ref env (resolve lambda-id) #f))
     (function param body)]))

(define (expand expr env)
  (match (parse expr env)
    [(constant c) (symbolic-data c)]
    [(variable i) (variable i)]
    [(application e1 e*) (application (expand e1 env) (for/list ([e e*]) (expand e env)))]
    [(symbolic-data e) (symbolic-data e)]
    [(function i e)
     (define i^ (gensym (identifier-symbol i)))
     (function i^ (expand (subst e i (identifier i^ '() (identifier-quoted i)))
                          (hash-set env i^ (var-binding))))]
    [(macro-application i e*)
     (match-define (transformer t) (hash-ref env (resolve i)))
     (define m (fresh-mark))
     (expand (mark-syntax (t (mark-syntax `(,i . ,e*) m)) m) env)]))

(define initial-env
  (hash 'quote (special-form 'quote)
        'lambda (special-form 'lambda)))

(define (datum->syntax d)
  (match d
    [(or (? number?) (? boolean?) (? string?) '()) d]
    [(? symbol?) (identifier d '() d)]
    [(? identifier?) d] ; so we can use in a transformer where inputs are already syntax
    [(cons d1 d2) (cons (datum->syntax d1) (datum->syntax d2))]))

(define (as-surface-syntax expr)
  (match expr
    [(constant c) c]
    [(variable (identifier s _ _)) s]
    [(application e1 e*) `(,(as-surface-syntax e1) . ,(map as-surface-syntax e*))]
    [(symbolic-data (or (? number? c) (? boolean? c) (? string? c))) c]
    [(symbolic-data e) `',e]
    [(function s e) `(lambda (,s) ,(as-surface-syntax e))]))

(define (expand-top e [extra-bindings '()])
  (parameterize ([gensym-counter 0])
    (as-surface-syntax
     (expand (datum->syntax e)
             (hash-union initial-env extra-bindings)))))

(module+ test
  (require rackunit)
  
  (define or-transformer
    (transformer
     (lambda (stx)
       (match stx
         [`(,_ ,e1 ,e2)
          (datum->syntax
           `((lambda (t)
               (if t t ,e2))
             ,e1))]))))

  (check-equal?
   (expand-top
    '((lambda (t)
        (or #f t))
      'x)
    (hash 'or or-transformer
          'if (var-binding)))
   '((lambda (t1)
       ((lambda (t3)
          (if t3 t3 t1))
        #f))
     'x)))
