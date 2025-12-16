## syntax-case

The `syntax-case` system was the first to effectively integrate procedural macros, hygiene, and a pattern matching and templating language.

Pages 1-6 of "Syntactic Abstraction: The syntax-case expander" provides the best introduction to the system and the problems it is trying to solve (https://guenchi.github.io/Scheme/doc/Syntactic%20Abstraction%20the%20Syntax-Case%20Expander.pdf).

Then, page 9 onwards of "Syntactic Abstraction in Scheme" provides the best explanation of the hygiene algorithm (https://scholarworks.iu.edu/iuswrrest/api/core/bitstreams/3f729d08-c698-4430-9d04-6a782c5b9300/content).

[reading-exercises.rkt](reading-exercises.rkt) provides some examples to work through.

[reading-exercises-completed.rkt](reading-exercises-completed.rkt) shows my solutions.

[hygiene-intro.md](hygiene-intro.md) illustrates the hygiene problem and explains the connection to alpha-equivalence.

[unhygienic.rkt](unhygienic.rkt) implements the unhygienic expander from Figures 1 and 2.

[hygienic-eager.rkt](hygienic-eager.rkt) partially implements the hygienic expansion algorithm from Figures 3 and 4.