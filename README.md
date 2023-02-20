# TFP 2023 Paper Directory

Contains both re-implementations of some logical and extra-logical
operators for a microKanren, as well as experimental results
bench-marking those variations on some small programs.

## Implementation

We structure the re-implementations through several files.

| Purpose                                                           | File                                              |
|-------------------------------------------------------------------|---------------------------------------------------|
| Underlying implementation + disj/conj re-implementations          | [](./logical-combinator-function-definitions.rkt) |
| Minimal correctness tests for re-implementations                  | [](./testing-logical-combinator-definitions.rkt)  |
| Interface macros, to facilitate comparisons w/miniKanren programs | [](./interface-definitions.rkt)                   |
|                                                                   |                                                   |

These files are tested against Racket 8.7.


## Paper

| Purpose    | File           |
|------------|----------------|
| tex source | [](./tfp.tex)  |
| bib source | [](./tfp.bib)  |
| Makefile   | [](./Makefile) |
|            |                |

In order to build the paper, you should have `pygments`, `xelatex`,
`biber` installed and available on path. You should then be able to
type `make`, as the default target is the paper.

