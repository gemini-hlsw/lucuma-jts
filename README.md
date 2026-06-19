# lucuma-jts

This projects contains a port of [JTS](https://github.com/locationtech/jts) to scala.
The rationale is the need to use it in both scala and scala.js.

The conversion was done using IntelliJ but the conversion is far from perfect and manual tweaking is required.
For the same reasons bugs are expected. Ideally more tests should be included

*Note: Only the minimal amount of code to use JTS in gsp-math was ported, about 150 classes*

## Potential issues on the conversion to scala

* Lots of code use non generic java util collections. They need to be adjusted manually
* Ports of for loops can be messy, especially if they include *break/continue*
* Field initialization can be messy, in some cases fields are read before they are initialized
* Some classes use `TreeMap` which is not supported in scala.js. Those need to be converted to `scala.TreeMap`

## Tests

Only one test has been included to fix a bug. Ideally more should be converted.
They don't need to run on java but may need to be adjusted to work with the scala code

## AWT
A small awt package is also included but that is just a copy of the awt package as in the original java code
It is only meant to be used on the JVM obviously

## History

The bulk of the port tracks JTS **1.18.0**, commit `16023a11a6b96940fefc811399501296fe00f541`:
https://github.com/locationtech/jts/commit/16023a11a6b96940fefc811399501296fe00f541

Since then, selected pieces have been brought forward to the JTS **1.20.0** tag,
commit `6e95fe82feb986a7aa657f4ffa406d8c290af509`, as self-contained vertical
slices (a full 1.18→1.20 sync is not viable — the original port was never a
complete snapshot). What was ported from 1.20.0:

* `operation.relateng` — the RelateNG relate engine (upstream PR #1052). It now
  backs every `Geometry` binary predicate (`contains`, `intersects`, `covers`,
  …) and `relate()`, which therefore also accepts `GeometryCollection` operands.
  This replaces the legacy topology-graph-based relate path and is significantly
  faster for predicate evaluation.
* Supporting dependencies: `index.hprtree` (packed Hilbert R-tree),
  `algorithm.PolygonNodeTopology`, and `noding.MCIndexSegmentSetMutualIntersector`.
* Two correctness fixes surfaced by porting the upstream RelateNG test suite:
  DD-precision line `Intersection`, and WKT `MULTIPOINT` EMPTY-element handling.

So: most classes are at 1.18.0; the relate engine and its dependencies are at 1.20.0.
