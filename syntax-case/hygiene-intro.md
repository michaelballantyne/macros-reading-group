Starting at the beginning---"Hygienic macro expansion" by Kohlbecker, Friedman, Felleisen, and Duba in 1986. ("KFFD")

In λ-calculus when we write

```
(λv.b) e -->_n b[e/v]
```

we implicitly assume a "hygiene condition": no bound variable of b is a free variable of e.

So,

```
(λx.λy.x) -/->_n λy.y
```

Instead, we first α-rename:

(λx.λy.x) y α= (λx.λz.x) y

and then reduce:

(λx.λz.x) -/->_n λz.y

We might also say we think "modulo α-equivalence".

KFFD's idea was to somehow apply this notion to macros.

At first it looks straightforward. In the `or` example:

```
(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      [( e1 e2)
       #'(let ([t e1])
           (if t t e2))])))

(let ([t #t])
   (or #f t))
```

we could α-rename the macro template:

```
(let ([t e1])
  (if t t e2))
α=
(let ([t2 e1])
  (if t2 t2 e2))
```

and then expand, producing the result we want:

```
(let ([t #t])
  (or #f t))
-->
(let ([t #t])
  (let ([t2 #t])
    (if t2 t2 t)))
```

But in general it's not so simple!

- What if `let` itself is a macro?

  ```
  (let ([x e]) b) --> ((λ (x) b) e)
  ```

  Then we can't understand the template.

- What if `or` is a procedural macro?
  
  The transformation is opaque and may combine syntax in arbitrary ways. We don't get to look at the "template" until we've already expanded!

So, what can we do?

1. Annotate syntax during expansion with extra information about its origin.
2. Use that information when α-renaming later.

KFFD had one, simplistic strategy for this. "Syntactic abstraction in Scheme" provides a more sophisticated strategy that interleavings marking and substitution steps, supporing a greater variety of macros.