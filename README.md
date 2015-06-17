# glens

A Clojure library designed to provide something similar to lenses (or zippers)
for Graphs (hence the name: Graph+LENS). However, it's neither lenses or
zippers in the formal sense.

## Usage

Don't.

But the general idea is this:

```clojure
(use 'glens.core)

(-> null    ; An empty graph
    edges   ; Accessors symbolic lens/zippers/cursors in to the graph.
    (conj [:x :y] [:y :z])  ; These types implement standard interfaces.
    graph)  ; And you can navigate back around with them.
;=> #graph [#{:y :z :x} {:x #{:y}, :y #{:z :x}, :z #{:y}}]
; Where this is ^nodes^ and ^adjacency^, but representation will change.
```

The hope is that the only extra interfaces you'll need in the general case are
`null`, `graph`, `nodes`, and `edges`; plus the usual Clojure suspects.

I have no idea if this is actually a good idea; nor how far to push it. Could
get crazy, for example representing attributed graphs with metadata which is
a custom map implementation that similarly tracks being part of the graph.

Let me know via Twitter if this idea is at all interesting to you.

## License

Copyright © 2015 Brandon Bloom

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
