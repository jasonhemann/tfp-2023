# TFP 2023 Paper Directory

Contains both re-implementations of some logical and extra-logical
operators for a microKanren, as well as experimental results
bench-marking those variations on some small programs.

## Implementation

We structure the re-implementations through several files.

| File & Purpose                                                                                            |
|-----------------------------------------------------------------------------------------------------------|
| [Underlying implementation + disj/conj re-implementations](./logical-combinator-function-definitions.rkt) |
| [Minimal correctness tests for re-implementations](./testing-logical-combinator-definitions.rkt)          |
| [Interface macros, to facilitate comparisons w/miniKanren programs](./interface-definitions.rkt)          |
| [Demonstrate potential cost of null? checks in base case](./profiling-logical-combinator-definitions.rkt) |



These files are tested against Racket 8.7.


## Paper

| Purpose                 |
|-------------------------|
| [tex source](./tfp.tex) |
| [bib source](./tfp.bib) |
| [Makefile](./Makefile)  |
|                         |


In order to build the paper, you should have `pygments`, `xelatex`,
`biber` installed and available on path. You should then be able to
type `make`, as the default target is the paper. 

## Examples

The [examples](./examples) sub-directory describes some of the work
testing examples suggested to see a big performance wrt left
associating conjuction. This was a more complicated story than had
seemed at first, where rather than uniformly reordering association,
instead the improvement was from locally adjusting that search
strategy by use of higher-order goals.

With the logo example, we also saw very little improvement from
uniformly re-associating conjunctions.

With the quines example, we did see some reasonable improvements over
the baseline. However this was using Nada Amin's unification-only
quines interpreter, which means deBruijn indices for variables. So
this too has some confounding factors. Outstanding work: completely
integrate disequality constraints into the microKanren versions, so
that at least we do not need deBruijn indices, and then truly isolate
the performance impact on quines.

These tests in their current state are:

| File & Purpose                                                                                   |
|--------------------------------------------------------------------------------------------------|
| [](./profiling-quines.rkt)                                                                       |
| [Incipient program for quines interp w/o disequality](./profiling-quines-no-disequality.rkt)     |
| [Incipient program for disequality based quines interp](./profiling-quines-with-disequality.rkt) |
| [Incipient program to reify a constraint store](./constraint-store-to-program.rkt)               |
| useful to ensure correct output                                                                  |
|                                                                                                  |
