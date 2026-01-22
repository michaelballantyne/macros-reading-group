#lang racket

;; Scheme and Racket have mutually-recursive definitions:
(let ([y 6])
  (define x (lambda () y))
  (define y 5)
  (x))
=> 5

;; This leads to tricky cases with macros.

;;
;; Definition after use
;; 

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
;; ...
;; apply the transformer and mark
(let ([y1 6])
  (define-syntax m1 (syntax-rules ()
                      ((_ arg) (define arg (lambda () y1)))))
  〚(define x (lambda () y1ᵐ¹))〛
  〚(define y1 5)〛
  〚(x)〛)
;; ...
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


〚(let ([y 6])
   (define-syntax m (syntax-rules ()
                      ((_ arg) (define arg (lambda () y)))))
   (m x)
   (define y 5)
   (x))〛
;; Push down a renaming onto the syntax
r1 = [y_1 / y]
(let ([y_1 6])
  〚(define-syntax mʳ¹ (syntax-rules ()
                        ((_ arg) (define arg (lambda () yʳ¹)))))〛
  〚(mʳ¹ xʳ¹)〛
  〚(define yʳ¹ 5)〛
  〚(xʳ¹)〛)

;; Push down a renaming onto all syntax in the definition context
r1 = [y_1 / y]
r2 = []
(let ([y_1 6])
  〚(define-syntax mʳ¹ʳ² (syntax-rules ()
                      ((_ arg) (define arg (lambda () yʳ¹ʳ²)))))〛
  〚(mʳ¹ʳ² xʳ¹ʳ²)〛
  〚(define yʳ¹ʳ² 5)〛
  〚(xʳ¹ʳ²)〛)

;; The define-syntax mutates the definition context's renaming rib to add an additional renaming.
r1 = [y_1 / y]
r2 = [m_1 / mʳ¹ʳ²]
(let ([y_1 6])
  (define-syntax mʳ¹ʳ² (syntax-rules ()
                         ((_ arg) (define arg (lambda () yʳ¹ʳ²)))))
  〚(mʳ¹ʳ² xʳ¹ʳ²)〛
  〚(define yʳ¹ʳ² 5)〛
  〚(xʳ¹ʳ²)〛)

;; apply the transformer and mark
r1 = [y_1 / y]
r2 = [m_1 / mʳ¹ʳ²]
(let ([y_1 6])
  (define-syntax mʳ¹ʳ² (syntax-rules ()
                         ((_ arg) (define arg (lambda () yʳ¹ʳ²)))))
  〚(define xʳ¹ʳ² (lambda () yʳ¹ʳ²ᵐ¹))〛
  〚(define yʳ¹ʳ² 5)〛
  〚(xʳ¹ʳ²)〛)

;; The define mutates the definition context's renaming rib to add an additional renaming.
r1 = [y_1 / y]
r2 = [m_1 / mʳ¹ʳ², x_1 / xʳ¹ʳ²]
(let ([y_1 6])
  (define-syntax mʳ¹ʳ² (syntax-rules ()
                         ((_ arg) (define arg (lambda () yʳ¹ʳ²)))))
  (define x_1 〚(lambda () yʳ¹ʳ²ᵐ¹)〛)
  〚(define yʳ¹ʳ² 5)〛
  〚(xʳ¹ʳ²)〛)

;; The define mutates the definition context's renaming rib to add an additional renaming.
r1 = [y_1 / y]
r2 = [m_1 / mʳ¹ʳ², x_1 / xʳ¹ʳ², y_2 / yʳ¹ʳ²]
(let ([y_1 6])
  (define-syntax mʳ¹ʳ² (syntax-rules ()
                         ((_ arg) (define arg (lambda () yʳ¹ʳ²)))))
  (define x_1 〚(lambda () yʳ¹ʳ²ᵐ¹)〛)
  (define y_2 〚5〛)
  〚(xʳ¹ʳ²)〛)

;; Resolve the reference by applying the marks and renamings
r1 = [y_1 / y]
r2 = [m_1 / mʳ¹ʳ², x_1 / xʳ¹ʳ², y_2 / yʳ¹ʳ²]
(let ([y_1 6])
  (define-syntax mʳ¹ʳ² (syntax-rules ()
                         ((_ arg) (define arg (lambda () yʳ¹ʳ²)))))
  (define x_1 〚(lambda () yʳ¹ʳ²ᵐ¹)〛)
  (define y_2 〚5〛)
  (x_1))

;; Resolve the reference by applying the marks and renamings
r1 = [y_1 / y]
r2 = [m_1 / mʳ¹ʳ², x_1 / xʳ¹ʳ², y_2 / yʳ¹ʳ²]
(let ([y_1 6])
  (define-syntax mʳ¹ʳ² (syntax-rules ()
                         ((_ arg) (define arg (lambda () yʳ¹ʳ²)))))
  (define x_1 (lambda () y_2))
  (define y_2 5)
  (x_1))


;;
;; Macro-introduced definitions
;;
〚(let ([x 6])
   (define-syntax m (syntax-rules ()
                      ((_ arg)
                       (begin
                         (define x 5)
                         (displayln x)))))
   (m)
   (displayln x))〛
;; Substitution for x; create definition context rib; add substitution for m
r1 = [x_1 / x]
r2 = [m_1 / m]
(let ([x_1 6])
  (define-syntax m_1 (syntax-rules ()
                     ((_ arg)
                      (begin
                        (define xʳ¹ʳ² 5)
                        (displayln xʳ¹ʳ²)))))
  〚(mʳ¹ʳ²)〛
  〚(displayln xʳ¹ʳ²)〛)
;; Apply transformer and mark the result
r1 = [x_1 / x]
r2 = [m_1 / m]
(let ([x_1 6])
  (define-syntax m (syntax-rules ()
                     ((_ arg)
                      (begin
                        (define xʳ¹ʳ² 5)
                        (displayln xʳ¹ʳ²)))))
  〚(define xʳ¹ʳ²ᵐ¹ 5)〛
  〚(displayln xʳ¹ʳ²ᵐ¹)〛
  〚(displayln xʳ¹ʳ²)〛)

;; Add substitution for x
r1 = [x_1 / x]
r2 = [m_1 / m, x_2 / xʳ¹ʳ²ᵐ¹]
(let ([x_1 6])
  (define-syntax m (syntax-rules ()
                     ((_ arg)
                      (begin
                        (define xʳ¹ʳ² 5)
                        (displayln xʳ¹ʳ²)))))
  (define x_2 5)
  〚(displayln xʳ¹ʳ²ᵐ¹)〛 ;; Problem: the m1 mark is not beneath the r2 renaming, so the substitution doesn't apply!
  〚(displayln xʳ¹ʳ²)〛)


;; Solution: expansion in a definition context has to re-paint the rib on every expansion!

r1 = [x_1 / x]
r2 = [m_1 / m, x_2 / xʳ¹ʳ²ᵐ¹]
(let ([x_1 6])
  (define-syntax m (syntax-rules ()
                     ((_ arg)
                      (begin
                        (define xʳ¹ʳ² 5)
                        (displayln xʳ¹ʳ²)))))
  (define x_2 5)
  〚(displayln xʳ¹ʳ²ᵐ¹ʳ²)〛 ;; Now there is a copy of r2 on top of m1.
  〚(displayln xʳ¹ʳ²)〛)

