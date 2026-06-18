# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Scala port of the Java Topology Suite (JTS) geometric library, designed to work on both JVM and JavaScript platforms. The port enables computational geometry operations for the Lucuma project ecosystem while maintaining compatibility with Scala.js.

### Key Characteristics

- **Cross-platform**: Supports both JVM and JavaScript via Scala.js
- **Legacy Java port**: Converted from Java JTS 1.18.0, retaining much of the original structure
- **Computational geometry**: Provides geometric algorithms, spatial operations, and coordinate systems
- **Minimal subset**: Only ~150 classes from the full JTS library were ported

## Build System

### Project Structure

```
lucuma-jts/
├── modules/
│   ├── jts/           # Main JTS library (cross-compiled for JVM/JS)
│   ├── jts-awt/       # AWT-specific functionality (JVM only)
│   └── tests/         # Java test suite
├── build.sbt          # Main build configuration
└── project/           # sbt build metadata
```

### Essential Build Commands

The repo ships a `flake.nix`; sbt runs inside the Nix dev shell. Either drop
into `nix develop` first, or wrap each call as
`nix develop --command sbt '<tasks>'`, e.g.
`nix develop --command sbt 'jtsJVM/compile; jtsJS/compile; tests/test'`. The
bare `sbt` tasks below assume you are already inside `nix develop`.

```bash
# Start sbt shell (recommended)
sbt

# Cross-compilation commands
compile                 # Compile for both JVM and JS
jts/compile            # Compile main library
tests/compile          # Compile test suite

# Testing
test                   # Run all tests
tests/test             # Run tests specifically
testOnly *ClassName    # Run specific test class

# Platform-specific
jtsJVM/compile         # JVM-only compilation
jtsJS/compile          # JavaScript-only compilation
```

### Build Configuration Details

- **Scala 3.3.7**: Single Scala version for both platforms (see `crossScalaVersions` in `build.sbt`)
- **Cross-compilation**: `CrossType.Pure` enables shared source between JVM/JS
- **Legacy compatibility**: `tlFatalWarnings := false` due to ported Java code
- **JUnit integration**: Tests use JUnit via `junit-interface`
- **sbt-typelevel**: Build is managed by sbt-typelevel (`tlCrossRootProject`, `tlBaseVersion`, `tlCiReleaseBranches`, `tlVersionIntroduced`). Versioning and CI releases are TL-driven; master is the release branch.

### Modules

- `jts` — main library, cross-compiled JVM/JS (`jtsJVM`, `jtsJS`).
- `jts_awt` — JVM-only AWT helpers; depends on `jts.jvm`; carries its own EPL/EDL header license.
- `tests` — JUnit test suite, depends on `jts.jvm`, `NoPublishPlugin`.

### Lint / Format

```bash
sbt scalafmtAll       # format all sources (.scalafmt.conf + .scalafmt-common.conf)
sbt scalafmtCheckAll  # CI-style check
sbt scalafixAll       # apply scalafix rules (.scalafix.conf + .scalafix-common.conf)
```

## Architecture

### Core Package Structure

```
org.locationtech.jts/
├── algorithm/          # Geometric algorithms (convex hull, centroids, etc.)
├── geom/              # Basic geometry types (Point, Line, Polygon)
├── geomgraph/         # Graph structures for geometric operations
├── index/             # Spatial indexing (R-tree, KD-tree)
├── io/                # Input/output (WKT format)
├── math/              # Mathematical utilities (vectors, matrices)
├── noding/            # Node and edge management
├── operation/         # High-level geometric operations
│   ├── buffer/        # Geometry buffering
│   ├── overlay/       # Boolean operations (union, intersection)
│   ├── overlayng/     # Next-generation overlay algorithms
│   └── relate/        # Spatial relationship testing
├── precision/         # Precision model handling
└── util/              # General utilities
```

### Key Design Patterns

1. **Immutable geometries**: Core geometric objects follow functional patterns
2. **Factory pattern**: `GeometryFactory` creates geometry instances
3. **Visitor pattern**: Geometry traversal and filtering operations
4. **Algorithm isolation**: Computational algorithms separated from data structures

### Platform Considerations

- **Scala.js limitations**: `java.util.TreeMap` replaced with `scala.collection.mutable.TreeMap`
- **AWT dependencies**: Isolated in separate `jts-awt` module (JVM only)
- **Collection compatibility**: Java collections manually converted to Scala equivalents

## Java to Scala Conversion Issues

### Common Port Artifacts

When working with this codebase, be aware of conversion artifacts from the original Java:

- **Non-generic collections**: Java `ArrayList`, `HashMap` may need Scala type annotations
- **Loop constructs**: Ported `for` loops may retain imperative patterns instead of functional equivalents
- **Field initialization**: Some fields may be accessed before initialization due to Java constructor patterns
- **Null handling**: Legacy Java null-checking rather than Scala `Option` types

### Development Guidelines

- **Functional refactoring**: When modifying code, prefer Scala collections and functional patterns
- **Platform compatibility**: Ensure changes work on both JVM and JavaScript
- **Preserve algorithms**: Geometric algorithms should maintain mathematical correctness
- **Minimal dependencies**: Avoid adding dependencies that don't work with Scala.js

## Testing

### Test Structure

- **Java tests**: Test suite remains in Java (`modules/tests/src/test/java/`)
- **JUnit framework**: Uses `junit-interface` for sbt integration
- **Limited coverage**: Only minimal tests were ported from JTS
- **Test data**: WKT geometry files in `modules/tests/src/test/resources/testdata/`

### Running Tests

```bash
# All tests
sbt test

# Specific test class
sbt "testOnly *GeometryImplTest"

# Test categories
sbt "tests/testOnly org.locationtech.jts.algorithm.*"
sbt "tests/testOnly org.locationtech.jts.geom.*"
```

## Cross-Platform Gotchas

- **TreeMap usage**: Use `scala.collection.mutable.TreeMap` instead of `java.util.TreeMap`
- **Array operations**: JavaScript arrays have different performance characteristics
- **Math precision**: Floating-point precision may differ between platforms
- Always verify changes with `jtsJS/compile` in addition to JVM compile.

## History

Original port done up to JTS commit `16023a11a6b96940fefc811399501296fe00f541` (JTS 1.18.0). See `README.md`.
Selected pieces have since been brought up to JTS 1.20.0 as vertical slices (see `docs/relateng-port-plan.md`): the `operation.relateng` package (RelateNG now backs all `Geometry` binary predicates and `relate()`, which therefore supports GeometryCollection operands), `index.hprtree`, `algorithm.PolygonNodeTopology`, `noding.MCIndexSegmentSetMutualIntersector`, DD-precision `Intersection`, and WKT EMPTY-element handling.
