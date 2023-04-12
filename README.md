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
Original port done up to commit 16023a11a6b96940fefc811399501296fe00f541 of JTS corresponding to version 1.18.0
https://github.com/locationtech/jts/commit/16023a11a6b96940fefc811399501296fe00f541
