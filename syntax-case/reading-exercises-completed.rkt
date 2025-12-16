#lang racket

;;
;; A worked example
;;

;; Below I step through an expansion of the `or` macro,
;; showing the substitution steps performed for binding forms
;; and the marking steps performed for macro applications.
;;
;; Syntax outside of the〚〛brackets is fully-expanded, while
;; syntax within them is not yet expanded. The notation `tᵐ¹`
;; indicates an identifier with symbol `t` and one mark `m1`.

(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      [( e1 e2)
       #'(let ([t e1])
           (if t t e2))])))

〚(let ([t #t])
   (or #f t))〛
;; -> substitute let binder
(let ([t1 #t])
  〚(or #f t1)〛)
;; -> transformer application marks introduced names
(let ([t1 #t])
  〚(let ([tᵐ¹ #f])
     (if tᵐ¹ tᵐ¹ t1))〛)
;; -> substitute let binder (marked names are distinct from unmarked names when substituting)
(let ([t1 #t])
  (let ([t2 #f])
    〚(if t2 t2 t1)〛))

;;
;; Exercises
;;


;; 1. Page 13 of "Syntactic Abstraction in Scheme" presents the `mark` and `subst` constructors
;;    and the `resolve` operation. Using these constructors, what is the representation of the
;;    `t` identifier that originated in the `(or #f t)` expression, after all of the marks
;;    and substitutions from the expansion steps are applied to it?

subst(mark(mark(subst(t, t, t1), m1), m1), mark(t, m1), t2)

;; 2. Step through the `resolve` operation on the identifier from (1).

resolve(subst(mark(mark(subst(t, t, t1), m1), m1), mark(t, m1), t2))
   marksof(mark(mark(subst(t, t, t1), m1), m1))
    = marksof(mark(subst(t, t, t1), m1) exclusive union {m1}
    = {m1} exclusive union {m1}
    = ∅  ;; marks cancel
;; the outer subst doesn't apply because marksof(mark(mark(subst(t, t, t1), m1), m1)) is {}
= resolve(mark(mark(subst(t, t, t1), m1), m1))
= resolve(mark(subst(t, t, t1), m1))
= resolve(subst(t, t, t1))
= t1

;; 3. Step through the hygienic expansion of this program, like I did above for the `or` program:

〚(let ([add1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1 x))]))])
    (let ([add1 0])
      (increment add1))))〛
;; -> substitute let, letrec-syntax, and let binders
(let ([add1_1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment_1
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1_1 x))]))])
    (let ([add1_2 0])
      〚(increment add1_2)〛)))
;; -> transformer application marks introduced names
(let ([add1_1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment_1
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1_1 x))]))])
    (let ([add1_2 0])
      〚(set!ᵐ¹ add1_2 (add1_1ᵐ¹ add1_2))〛)))
;; -> resolve references by dropping marks
(let ([add1_1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment_1
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1_1 x))]))])
    (let ([add1_2 0])
      (set! add1_2 (add1_1 add1_2)))))

;; 4. The first hygiene algorithm, from "Hygienic Macro Expansion"
;;    by Kohlbecker, Friedman, Felleisen, and Duba is simpler than
;;    the syntax-case algorithm. Rather than interleave the processes
;;    of marking and substituting, it marks while expanding but leaves the
;;    task of substituting a fresh name for each binder until a final step
;;    when expansion is complete. If a name is not substituted, any marks
;;    on it are dropped, as in the `resolve` function in the syntax-case
;;    algorithm.
;;
;;    Try out this approach on the `increment` program---what goes wrong?

〚(let ([add1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1 x))]))])
    (let ([add1 0])
      (increment add1))))〛
;; -> transformer application marks introduced names
(let ([add1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment_1
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1 x))]))])
    (let ([add1 0])
      (set!ᵐ¹ add1 (add1ᵐ¹ add1)))))
;; -> substitute let, letrec-syntax, and let binders
(let ([add1_1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment_1
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1_1 x))]))])
    (let ([add1_2 0])
      〚(set!ᵐ¹ add1_2 (add1ᵐ¹ add1_2))〛)))
;; -> resolve references by dropping marks
(let ([add1_1 (lambda (x) (+ x 1))])
  (letrec-syntax ([increment_1
                   (lambda (x)
                     (syntax-case x ()
                       [(_ x)
                        #'(set! x (add1_1 x))]))])
    (let ([add1_2 0])
      (set! add1_2 (add1 add1_2)))))

;; Because the macro-introduced add1 had the mark attached,
;; neither substitution captured it, so it ended up unbound.
;; The fundamental problem is that if we don't substitute before
;; expansion, after expansion we don't know which of the surrounding
;; add1 bindings was the one in the macro's definition site.