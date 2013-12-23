This is a simple implementation of a memoized edit-distance algorithm (shortest
path through the edit graph) along with [Operational
Transformation](http://en.wikipedia.org/wiki/Operational_Transformation) (OT).
OT is essentially `git rebase` for string edits: starting from a common base,
two independent edits can be "merged" by transforming one to come after the
other. This transformation occurs by shifting its indices according to how the
first edit shifts the text. Intuitively, imagine two editors open on the same
text, with two cursors, and two people typing at two keyboards. OT merges are
always conflict-free (there is always a defined final state) but not
necessarily commutative. OT is perhaps well-known as a mechanism underlying the
now-defunct Google Wave's realtime document synchronization.

This toy was both a means to learn how to take advantage of lazy evaluation in
Haskell to do memoization in a pure function, and also to explore OT a bit.
