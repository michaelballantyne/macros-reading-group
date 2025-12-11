#lang racket


〚(let ([y 6])
   (define-syntax m (syntax-rules ()
                      ((_ arg) (define arg (lambda () y)))))
   (m x)
   (define y 5)
   (x))〛
;; substitute fresh name for binder
(let ([y1 6])
  〚(define-syntax m (syntax-rules ()
                      ((_ arg) (define arg (lambda () y1)))))〛
  〚(m x)〛
  〚(define y1 5)〛
  〚(x)〛)
;; substitute a fresh name for `m` throughout its scope
(let ([y1 6])
  (define-syntax m1 (syntax-rules ()
                      ((_ arg) (define arg (lambda () y1)))))
  〚(m1 x)〛
  〚(define y1 5)〛
  〚(x)〛)
;; apply the transformer and mark
(let ([y1 6])
  (define-syntax m1 (syntax-rules ()
                      ((_ arg) (define arg (lambda () y1)))))
  〚(define x (lambda () y1ᵐ¹))〛
  〚(define y1 5)〛
  〚(x)〛)
;; substitute a fresh name for `x` throughout its scope
(let ([y1 6])
  (define-syntax m1 (syntax-rules ()
                      ((_ arg) (define arg (lambda () y1)))))
  (define x1 (lambda () y1ᵐ¹))
  〚(define y1 5)〛
  〚(x1)〛)
;; substitute a fresh name for `y1` throughout its scope
(let ([y1 6])
  (define-syntax m1 (syntax-rules ()
                      ((_ arg) (define arg (lambda () y2)))))
  (define x1 (lambda () y1ᵐ¹)) ;; problem: already marked! should we rename?
  (define y2 5)
  〚(x1)〛)

;; The problem is that if `y` was inserted by a macro defined
;; outside of the local definition context, we want the mark to
;; prevent it from being renamed by the local binding.

;; But here, the `y` reference originated in the definition context.
;; If the definition of `y` came first, it would have been renamed!
;; But now it's been marked.

;; The solution is for definitions to *mutate* a shared renaming for the
;; definition context. Then it doesn't matter that I found it later!