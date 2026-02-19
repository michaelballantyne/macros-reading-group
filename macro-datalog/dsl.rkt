#lang racket

(provide (all-defined-out))

(require (for-syntax syntax/parse))

(define changed #f)

(define (merge-new-rows tables)
  (for ([t tables])
    (match-define (table existing new) t)
    (set-union! existing new)
    (set-clear! new)))

(struct table [existing new] #:transparent)

(define (make-table [initial '()])
  (table (apply mutable-set initial)
         (mutable-set)))

(define (in-table t)
  (table-existing t))

(define (add-row! t row)
  (match-define (table existing new) t)
  (when (and (not (set-member? new row)) (not (set-member? existing row)))
    (set! changed #t)
    (set-add! new row)))

(define (run tables rules)
  (set! changed #f)
  (for ([rule rules])
    (rule))
  (merge-new-rows tables)
  (when changed
    (run tables rules)))

(define (print-table t)
  (pretty-print (table-existing t)))

(define-syntax match-atom
  (lambda (stx)
    (syntax-parse stx
      [(_ (t pat ...)
          body)
       #'(for ([row (in-table t)])
           (match row
             [(list pat ...)
              body]
             [_ (void)]))])))

(define-syntax rule
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (<--)
      [(_ (t e ...) <-- body ...)
       #'(lambda ()
           (match-body body ...
                     (add-row! t (list e ...))))])))

(define-syntax match-body
  (lambda (stx)
    (syntax-parse stx
      [(_ head-action)
       #'head-action]
      [(_ atom body ... head-action)
       #'(match-atom atom
           (match-body body ...
                       head-action))])))

(define-syntax macrolog
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (<--)
      [(_ ([table-name init] ...)
          table-to-print
          [head <-- body ...]
          ...)
       #'(let ([table-name (make-table init)] ...)
           (run
             (list table-name ...)
             (list (rule head <-- body ...)
                   ...))
           (print-table table-to-print))])))

   