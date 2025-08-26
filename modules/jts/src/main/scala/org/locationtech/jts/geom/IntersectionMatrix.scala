// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Vivid Solutions.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.geom

/**
 * Models a <b>Dimensionally Extended Nine-Intersection Model (DE-9IM)</b> matrix. DE-9IM matrices
 * (such as "212FF1FF2") specify the topological relationship between two {link Geometry}s. This
 * class can also represent matrix patterns (such as "T*T******") which are used for matching
 * instances of DE-9IM matrices.
 *
 * Methods are provided to: <UL> <LI> set and query the elements of the matrix in a convenient
 * fashion <LI> convert to and from the standard string representation (specified in SFS Section
 * 2.1.13.2). <LI> test to see if a matrix matches a given pattern string. </UL> <P>
 *
 * For a description of the DE-9IM and the spatial predicates derived from it, see the <i><A
 * HREF="http://www.opengis.org/techno/specs.htm">OGC 99-049 OpenGIS Simple Features Specification
 * for SQL</A></i>, as well as <i>OGC 06-103r4 OpenGIS Implementation Standard for Geographic
 * information - Simple feature access - Part 1: Common architecture</i> (which provides some
 * further details on certain predicate specifications). <p> The entries of the matrix are defined
 * by the constants in the {link Dimension} class. The indices of the matrix represent the
 * topological locations that occur in a geometry (Interior, Boundary, Exterior). These are provided
 * as constants in the {link Location} class.
 *
 * @version 1.7
 */
object IntersectionMatrix {

  /**
   * Tests if the dimension value matches <tt>TRUE</tt> (i.e. has value 0, 1, 2 or TRUE).
   *
   * @param actualDimensionValue
   *   a number that can be stored in the <code>IntersectionMatrix</code> . Possible values are
   *   <code>{TRUE, FALSE, DONTCARE, 0, 1, 2}</code>. return true if the dimension value matches
   *   TRUE
   */
  def isTrue(actualDimensionValue: Int): Boolean = {
    if (actualDimensionValue >= 0 || actualDimensionValue == Dimension.TRUE) return true
    false
  }

  /**
   * Tests if the dimension value satisfies the dimension symbol.
   *
   * @param actualDimensionValue
   *   a number that can be stored in the <code>IntersectionMatrix</code> . Possible values are
   *   <code>{TRUE, FALSE, DONTCARE, 0, 1, 2}</code>.
   * @param requiredDimensionSymbol
   *   a character used in the string representation of an <code>IntersectionMatrix</code>. Possible
   *   values are <code>{T, F, * , 0, 1, 2}</code>. return true if the dimension symbol matches the
   *   dimension value
   */
  def matches(actualDimensionValue: Int, requiredDimensionSymbol: Char): Boolean = {
    if (requiredDimensionSymbol == Dimension.SYM_DONTCARE) return true
    if (
      requiredDimensionSymbol == Dimension.SYM_TRUE && (actualDimensionValue >= 0 || actualDimensionValue == Dimension.TRUE)
    ) return true
    if (requiredDimensionSymbol == Dimension.SYM_FALSE && actualDimensionValue == Dimension.FALSE)
      return true
    if (requiredDimensionSymbol == Dimension.SYM_P && actualDimensionValue == Dimension.P)
      return true
    if (requiredDimensionSymbol == Dimension.SYM_L && actualDimensionValue == Dimension.L)
      return true
    if (requiredDimensionSymbol == Dimension.SYM_A && actualDimensionValue == Dimension.A)
      return true
    false
  }

  /**
   * Tests if each of the actual dimension symbols in a matrix string satisfies the corresponding
   * required dimension symbol in a pattern string.
   *
   * @param actualDimensionSymbols
   *   nine dimension symbols to validate. Possible values are <code>{T, F, * , 0, 1, 2}</code>.
   * @param requiredDimensionSymbols
   *   nine dimension symbols to validate against. Possible values are <code>{T, F, * , 0, 1,
   *   2}</code>. return true if each of the required dimension symbols encompass the corresponding
   *   actual dimension symbol
   */
  def matches(actualDimensionSymbols: String, requiredDimensionSymbols: String): Boolean = {
    val m = new IntersectionMatrix(actualDimensionSymbols)
    m.matches(requiredDimensionSymbols)
  }
}

class IntersectionMatrix()

/**
 * Creates an <code>IntersectionMatrix</code> with <code>FALSE</code> dimension values.
 */
    extends Cloneable {

  /**
   * Internal representation of this <code>IntersectionMatrix</code>.
   */
  private val matrix = Array.ofDim[Int](3, 3)

  setAll(Dimension.FALSE)

  /**
   * Creates an <code>IntersectionMatrix</code> with the given dimension symbols.
   *
   * @param elements
   *   a String of nine dimension symbols in row major order
   */
  def this(elements: String) = {
    this()
    set(elements)
  }

  /**
   * Creates an <code>IntersectionMatrix</code> with the same elements as <code>other</code>.
   *
   * @param other
   *   an <code>IntersectionMatrix</code> to copy
   */
  def this(other: IntersectionMatrix) = {
    this()
    matrix(Location.INTERIOR)(Location.INTERIOR) =
      other.matrix(Location.INTERIOR)(Location.INTERIOR)
    matrix(Location.INTERIOR)(Location.BOUNDARY) =
      other.matrix(Location.INTERIOR)(Location.BOUNDARY)
    matrix(Location.INTERIOR)(Location.EXTERIOR) =
      other.matrix(Location.INTERIOR)(Location.EXTERIOR)
    matrix(Location.BOUNDARY)(Location.INTERIOR) =
      other.matrix(Location.BOUNDARY)(Location.INTERIOR)
    matrix(Location.BOUNDARY)(Location.BOUNDARY) =
      other.matrix(Location.BOUNDARY)(Location.BOUNDARY)
    matrix(Location.BOUNDARY)(Location.EXTERIOR) =
      other.matrix(Location.BOUNDARY)(Location.EXTERIOR)
    matrix(Location.EXTERIOR)(Location.INTERIOR) =
      other.matrix(Location.EXTERIOR)(Location.INTERIOR)
    matrix(Location.EXTERIOR)(Location.BOUNDARY) =
      other.matrix(Location.EXTERIOR)(Location.BOUNDARY)
    matrix(Location.EXTERIOR)(Location.EXTERIOR) =
      other.matrix(Location.EXTERIOR)(Location.EXTERIOR)
  }

  /**
   * Adds one matrix to another. Addition is defined by taking the maximum dimension value of each
   * position in the summand matrices.
   *
   * @param im
   *   the matrix to add
   */
  def add(im: IntersectionMatrix): Unit = {
    var i = 0
    while (i < 3) {
      var j = 0
      while (j < 3) {
        setAtLeast(i, j, im.get(i, j))
        j += 1
      }
      i += 1
    }
  }

  /**
   * Changes the value of one of this <code>IntersectionMatrix</code>s elements.
   *
   * @param row
   *   the row of this <code>IntersectionMatrix</code>, indicating the interior, boundary or
   *   exterior of the first <code>Geometry</code>
   * @param column
   *   the column of this <code>IntersectionMatrix</code>, indicating the interior, boundary or
   *   exterior of the second <code>Geometry</code>
   * @param dimensionValue
   *   the new value of the element
   */
  def set(row: Int, column: Int, dimensionValue: Int): Unit = matrix(row)(column) = dimensionValue

  /**
   * Changes the elements of this <code>IntersectionMatrix</code> to the dimension symbols in
   * <code>dimensionSymbols</code>.
   *
   * @param dimensionSymbols
   *   nine dimension symbols to which to set this <code>IntersectionMatrix</code> s elements.
   *   Possible values are <code>{T, F, * , 0, 1, 2}</code>
   */
  def set(dimensionSymbols: String): Unit = {
    var i = 0
    while (i < dimensionSymbols.length) {
      val row = i / 3
      val col = i % 3
      matrix(row)(col) = Dimension.toDimensionValue(dimensionSymbols.charAt(i))
      i += 1
    }
  }

  /**
   * Changes the specified element to <code>minimumDimensionValue</code> if the element is less.
   *
   * @param row
   *   the row of this <code>IntersectionMatrix</code> , indicating the interior, boundary or
   *   exterior of the first <code>Geometry</code>
   * @param column
   *   the column of this <code>IntersectionMatrix</code> , indicating the interior, boundary or
   *   exterior of the second <code>Geometry</code>
   * @param minimumDimensionValue
   *   the dimension value with which to compare the element. The order of dimension values from
   *   least to greatest is <code>{DONTCARE, TRUE, FALSE, 0, 1, 2}</code>.
   */
  def setAtLeast(row: Int, column: Int, minimumDimensionValue: Int): Unit =
    if (matrix(row)(column) < minimumDimensionValue) matrix(row)(column) = minimumDimensionValue

  /**
   * If row &gt;= 0 and column &gt;= 0, changes the specified element to
   * <code>minimumDimensionValue</code> if the element is less. Does nothing if row &lt;0 or column
   * &lt; 0.
   *
   * @param row
   *   the row of this <code>IntersectionMatrix</code> , indicating the interior, boundary or
   *   exterior of the first <code>Geometry</code>
   * @param column
   *   the column of this <code>IntersectionMatrix</code> , indicating the interior, boundary or
   *   exterior of the second <code>Geometry</code>
   * @param minimumDimensionValue
   *   the dimension value with which to compare the element. The order of dimension values from
   *   least to greatest is <code>{DONTCARE, TRUE, FALSE, 0, 1, 2}</code>.
   */
  def setAtLeastIfValid(row: Int, column: Int, minimumDimensionValue: Int): Unit =
    if (row >= 0 && column >= 0) setAtLeast(row, column, minimumDimensionValue)

  /**
   * For each element in this <code>IntersectionMatrix</code>, changes the element to the
   * corresponding minimum dimension symbol if the element is less.
   *
   * @param minimumDimensionSymbols
   *   nine dimension symbols with which to compare the elements of this
   *   <code>IntersectionMatrix</code>. The order of dimension values from least to greatest is
   *   <code>{DONTCARE, TRUE, FALSE, 0, 1, 2}</code> .
   */
  def setAtLeast(minimumDimensionSymbols: String): Unit = {
    var i = 0
    while (i < minimumDimensionSymbols.length) {
      val row = i / 3
      val col = i % 3
      setAtLeast(row, col, Dimension.toDimensionValue(minimumDimensionSymbols.charAt(i)))
      i += 1
    }
  }

  /**
   * Changes the elements of this <code>IntersectionMatrix</code> to <code>dimensionValue</code> .
   *
   * @param dimensionValue
   *   the dimension value to which to set this <code>IntersectionMatrix</code> s elements. Possible
   *   values <code>{TRUE, FALSE, DONTCARE, 0, 1, 2}</code> .
   */
  def setAll(dimensionValue: Int): Unit = {
    var ai = 0
    while (ai < 3) {
      var bi = 0
      while (bi < 3) {
        matrix(ai)(bi) = dimensionValue
        bi += 1
      }
      ai += 1
    }
  }

  /**
   * Returns the value of one of this matrix entries. The value of the provided index is one of the
   * values from the {link Location} class. The value returned is a constant from the {link
   * Dimension} class.
   *
   * @param row
   *   the row of this <code>IntersectionMatrix</code>, indicating the interior, boundary or
   *   exterior of the first <code>Geometry</code>
   * @param column
   *   the column of this <code>IntersectionMatrix</code>, indicating the interior, boundary or
   *   exterior of the second <code>Geometry</code> return the dimension value at the given matrix
   *   position.
   */
  def get(row: Int, column: Int): Int = matrix(row)(column)

  /**
   * Returns <code>true</code> if this <code>IntersectionMatrix</code> is FF*FF****.
   *
   * return <code>true</code> if the two <code>Geometry</code>s related by this
   * <code>IntersectionMatrix</code> are disjoint
   */
  def isDisjoint: Boolean =
    matrix(Location.INTERIOR)(Location.INTERIOR) == Dimension.FALSE && matrix(Location.INTERIOR)(
      Location.BOUNDARY
    ) == Dimension.FALSE && matrix(Location.BOUNDARY)(
      Location.INTERIOR
    ) == Dimension.FALSE && matrix(Location.BOUNDARY)(Location.BOUNDARY) == Dimension.FALSE

  /**
   * Returns <code>true</code> if <code>isDisjoint</code> returns false.
   *
   * return <code>true</code> if the two <code>Geometry</code>s related by this
   * <code>IntersectionMatrix</code> intersect
   */
  def isIntersects: Boolean = !isDisjoint

  /**
   * Returns <code>true</code> if this <code>IntersectionMatrix</code> is FT*******, F**T***** or
   * F***T****.
   *
   * @param dimensionOfGeometryA
   *   the dimension of the first <code>Geometry</code>
   * @param dimensionOfGeometryB
   *   the dimension of the second <code>Geometry</code> return <code>true</code> if the two
   *   <code>Geometry</code> s related by this <code>IntersectionMatrix</code> touch; Returns false
   *   if both <code>Geometry</code>s are points.
   */
  def isTouches(dimensionOfGeometryA: Int, dimensionOfGeometryB: Int): Boolean = {
    if (dimensionOfGeometryA > dimensionOfGeometryB) { // no need to get transpose because pattern matrix is symmetrical
      return isTouches(dimensionOfGeometryB, dimensionOfGeometryA)
    }
    if (
      (dimensionOfGeometryA == Dimension.A && dimensionOfGeometryB == Dimension.A) || (dimensionOfGeometryA == Dimension.L && dimensionOfGeometryB == Dimension.L) || (dimensionOfGeometryA == Dimension.L && dimensionOfGeometryB == Dimension.A) || (dimensionOfGeometryA == Dimension.P && dimensionOfGeometryB == Dimension.A) || (dimensionOfGeometryA == Dimension.P && dimensionOfGeometryB == Dimension.L)
    )
      return matrix(Location.INTERIOR)(Location.INTERIOR) == Dimension.FALSE && (IntersectionMatrix
        .isTrue(matrix(Location.INTERIOR)(Location.BOUNDARY)) || IntersectionMatrix.isTrue(
        matrix(Location.BOUNDARY)(Location.INTERIOR)
      ) || IntersectionMatrix.isTrue(matrix(Location.BOUNDARY)(Location.BOUNDARY)))
    false
  }

  /**
   * Tests whether this geometry crosses the specified geometry. <p> The <code>crosses</code>
   * predicate has the following equivalent definitions: <ul> <li>The geometries have some but not
   * all interior points in common. <li>The DE-9IM Intersection Matrix for the two geometries is
   * <ul> <li>T*T****** (for P/L, P/A, and L/A situations) <li>T*****T** (for L/P, L/A, and A/L
   * situations) <li>0******** (for L/L situations) </ul> </ul> For any other combination of
   * dimensions this predicate returns <code>false</code>. <p> The SFS defined this predicate only
   * for P/L, P/A, L/L, and L/A situations. JTS extends the definition to apply to L/P, A/P and A/L
   * situations as well. This makes the relation symmetric.
   *
   * @param dimensionOfGeometryA
   *   the dimension of the first <code>Geometry</code>
   * @param dimensionOfGeometryB
   *   the dimension of the second <code>Geometry</code> return <code>true</code> if the two
   *   <code>Geometry</code>s related by this <code>IntersectionMatrix</code> cross.
   */
  def isCrosses(dimensionOfGeometryA: Int, dimensionOfGeometryB: Int): Boolean = {
    if (
      (dimensionOfGeometryA == Dimension.P && dimensionOfGeometryB == Dimension.L) || (dimensionOfGeometryA == Dimension.P && dimensionOfGeometryB == Dimension.A) || (dimensionOfGeometryA == Dimension.L && dimensionOfGeometryB == Dimension.A)
    )
      return IntersectionMatrix.isTrue(
        matrix(Location.INTERIOR)(Location.INTERIOR)
      ) && IntersectionMatrix.isTrue(matrix(Location.INTERIOR)(Location.EXTERIOR))
    if (
      (dimensionOfGeometryA == Dimension.L && dimensionOfGeometryB == Dimension.P) || (dimensionOfGeometryA == Dimension.A && dimensionOfGeometryB == Dimension.P) || (dimensionOfGeometryA == Dimension.A && dimensionOfGeometryB == Dimension.L)
    )
      return IntersectionMatrix.isTrue(
        matrix(Location.INTERIOR)(Location.INTERIOR)
      ) && IntersectionMatrix.isTrue(matrix(Location.EXTERIOR)(Location.INTERIOR))
    if (dimensionOfGeometryA == Dimension.L && dimensionOfGeometryB == Dimension.L)
      return matrix(Location.INTERIOR)(Location.INTERIOR) == 0
    false
  }

  /**
   * Tests whether this <code>IntersectionMatrix</code> is T*F**F***.
   *
   * return <code>true</code> if the first <code>Geometry</code> is within the second
   */
  def isWithin: Boolean = IntersectionMatrix.isTrue(
    matrix(Location.INTERIOR)(Location.INTERIOR)
  ) && matrix(Location.INTERIOR)(Location.EXTERIOR) == Dimension.FALSE && matrix(Location.BOUNDARY)(
    Location.EXTERIOR
  ) == Dimension.FALSE

  /**
   * Tests whether this <code>IntersectionMatrix</code> is T*****FF*.
   *
   * return <code>true</code> if the first <code>Geometry</code> contains the second
   */
  def isContains: Boolean = IntersectionMatrix.isTrue(
    matrix(Location.INTERIOR)(Location.INTERIOR)
  ) && matrix(Location.EXTERIOR)(Location.INTERIOR) == Dimension.FALSE && matrix(Location.EXTERIOR)(
    Location.BOUNDARY
  ) == Dimension.FALSE

  /**
   * Returns <code>true</code> if this <code>IntersectionMatrix</code> is <code>T*****FF*</code> or
   * <code>*T****FF*</code> or <code>***T**FF*</code> or <code>****T*FF*</code>
   *
   * return <code>true</code> if the first <code>Geometry</code> covers the second
   */
  def isCovers: Boolean = {
    val hasPointInCommon =
      IntersectionMatrix.isTrue(matrix(Location.INTERIOR)(Location.INTERIOR)) || IntersectionMatrix
        .isTrue(matrix(Location.INTERIOR)(Location.BOUNDARY)) || IntersectionMatrix.isTrue(
        matrix(Location.BOUNDARY)(Location.INTERIOR)
      ) || IntersectionMatrix.isTrue(matrix(Location.BOUNDARY)(Location.BOUNDARY))
    hasPointInCommon && matrix(Location.EXTERIOR)(Location.INTERIOR) == Dimension.FALSE && matrix(
      Location.EXTERIOR
    )(Location.BOUNDARY) == Dimension.FALSE
  }

  /**
   * Returns <code>true</code> if this <code>IntersectionMatrix</code> is <code>T*F**F***</code> or
   * <code>*TF**F***</code> or <code>**FT*F***</code> or <code>**F*TF***</code>
   *
   * return <code>true</code> if the first <code>Geometry</code> is covered by the second
   */
  def isCoveredBy: Boolean = {
    val hasPointInCommon =
      IntersectionMatrix.isTrue(matrix(Location.INTERIOR)(Location.INTERIOR)) || IntersectionMatrix
        .isTrue(matrix(Location.INTERIOR)(Location.BOUNDARY)) || IntersectionMatrix.isTrue(
        matrix(Location.BOUNDARY)(Location.INTERIOR)
      ) || IntersectionMatrix.isTrue(matrix(Location.BOUNDARY)(Location.BOUNDARY))
    hasPointInCommon && matrix(Location.INTERIOR)(Location.EXTERIOR) == Dimension.FALSE && matrix(
      Location.BOUNDARY
    )(Location.EXTERIOR) == Dimension.FALSE
  }

  /**
   * Tests whether the argument dimensions are equal and this <code>IntersectionMatrix</code>
   * matches the pattern <tt>T*F**FFF*</tt>. <p> <b>Note:</b> This pattern differs from the one
   * stated in <i>Simple feature access - Part 1: Common architecture</i>. That document states the
   * pattern as <tt>TFFFTFFFT</tt>. This would specify that two identical <tt>POINT</tt>s are not
   * equal, which is not desirable behaviour. The pattern used here has been corrected to compute
   * equality in this situation.
   *
   * @param dimensionOfGeometryA
   *   the dimension of the first <code>Geometry</code>
   * @param dimensionOfGeometryB
   *   the dimension of the second <code>Geometry</code> return <code>true</code> if the two
   *   <code>Geometry</code>s related by this <code>IntersectionMatrix</code> are equal; the
   *   <code>Geometry</code>s must have the same dimension to be equal
   */
  def isEquals(dimensionOfGeometryA: Int, dimensionOfGeometryB: Int): Boolean = {
    if (dimensionOfGeometryA != dimensionOfGeometryB) return false
    IntersectionMatrix.isTrue(matrix(Location.INTERIOR)(Location.INTERIOR)) && matrix(
      Location.INTERIOR
    )(Location.EXTERIOR) == Dimension.FALSE && matrix(Location.BOUNDARY)(
      Location.EXTERIOR
    ) == Dimension.FALSE && matrix(Location.EXTERIOR)(
      Location.INTERIOR
    ) == Dimension.FALSE && matrix(Location.EXTERIOR)(Location.BOUNDARY) == Dimension.FALSE
  }

  /**
   * Returns <code>true</code> if this <code>IntersectionMatrix</code> is <UL> <LI> T*T***T** (for
   * two points or two surfaces) <LI> 1*T***T** (for two curves) </UL>.
   *
   * @param dimensionOfGeometryA
   *   the dimension of the first <code>Geometry</code>
   * @param dimensionOfGeometryB
   *   the dimension of the second <code>Geometry</code> return <code>true</code> if the two
   *   <code>Geometry</code>s related by this <code>IntersectionMatrix</code> overlap. For this
   *   function to return <code>true</code>, the <code>Geometry</code>s must be two points, two
   *   curves or two surfaces.
   */
  def isOverlaps(dimensionOfGeometryA: Int, dimensionOfGeometryB: Int): Boolean = {
    if (
      (dimensionOfGeometryA == Dimension.P && dimensionOfGeometryB == Dimension.P) || (dimensionOfGeometryA == Dimension.A && dimensionOfGeometryB == Dimension.A)
    )
      return IntersectionMatrix.isTrue(
        matrix(Location.INTERIOR)(Location.INTERIOR)
      ) && IntersectionMatrix.isTrue(
        matrix(Location.INTERIOR)(Location.EXTERIOR)
      ) && IntersectionMatrix.isTrue(matrix(Location.EXTERIOR)(Location.INTERIOR))
    if (dimensionOfGeometryA == Dimension.L && dimensionOfGeometryB == Dimension.L)
      return matrix(Location.INTERIOR)(Location.INTERIOR) == 1 && IntersectionMatrix.isTrue(
        matrix(Location.INTERIOR)(Location.EXTERIOR)
      ) && IntersectionMatrix.isTrue(matrix(Location.EXTERIOR)(Location.INTERIOR))
    false
  }

  /**
   * Returns whether the elements of this <code>IntersectionMatrix</code> satisfies the required
   * dimension symbols.
   *
   * @param requiredDimensionSymbols
   *   nine dimension symbols with which to compare the elements of this
   *   <code>IntersectionMatrix</code>. Possible values are <code>{T, F, * , 0, 1, 2}</code>. return
   *   <code>true</code> if this <code>IntersectionMatrix</code> matches the required dimension
   *   symbols
   */
  def matches(requiredDimensionSymbols: String): Boolean = {
    if (requiredDimensionSymbols.length != 9)
      throw new IllegalArgumentException("Should be length 9: " + requiredDimensionSymbols)
    var ai = 0
    while (ai < 3) {
      var bi = 0
      while (bi < 3) {
        if (
          !IntersectionMatrix.matches(matrix(ai)(bi), requiredDimensionSymbols.charAt(3 * ai + bi))
        ) return false
        bi += 1
      }
      ai += 1
    }
    true
  }

  /**
   * Transposes this IntersectionMatrix.
   *
   * return this <code>IntersectionMatrix</code> as a convenience
   */
  def transpose: IntersectionMatrix = {
    var temp = matrix(1)(0)
    matrix(1)(0) = matrix(0)(1)
    matrix(0)(1) = temp
    temp = matrix(2)(0)
    matrix(2)(0) = matrix(0)(2)
    matrix(0)(2) = temp
    temp = matrix(2)(1)
    matrix(2)(1) = matrix(1)(2)
    matrix(1)(2) = temp
    this
  }

  /**
   * Returns a nine-character <code>String</code> representation of this
   * <code>IntersectionMatrix</code> .
   *
   * return the nine dimension symbols of this <code>IntersectionMatrix</code> in row-major order.
   */
  override def toString: String = {
    val builder = new StringBuilder("123456789")
    var ai      = 0
    while (ai < 3) {
      var bi = 0
      while (bi < 3) {
        builder.setCharAt(3 * ai + bi, Dimension.toDimensionSymbol(matrix(ai)(bi)))
        bi += 1
      }
      ai += 1
    }
    builder.toString
  }
}
