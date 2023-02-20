
# Examples

The color-graph example was a tricky and perhaps unfair benchmark
comparison. In this example, due to Michael Ballantyne, Orchid, and
Greg Rosenblatt, they used higher-order goals in order to adjust a
local part of the search to force a non-standard search strategy. 

This gave a *dramatic* improvement. It is also very much like using a
local DFS, and so does not (at least not directly) show the impact of
associating conjunctions.

## Zzz

There’s the possibility that the Zzzs are making an important
impact--cf our version w/Orchid’s. One more axis for comparison. But I
suspect not.

There’s the possibility that the delays in the cdr are a big impact.
One more axis for comparison.

Also need to run tests where we shuffle the ordering of edges and
nodes and left-right of the edge pairs, to kinda avoid around that.

See also [separate repo benchmarking orchid’s results w/various
options](https://github.com/jasonhemann/benchmark-orchid-experiment/)
