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
 * Models an OGC SFS <code>LinearRing</code>. A <code>LinearRing</code> is a {link LineString} which
 * is both closed and simple. In other words, the first and last coordinate in the ring must be
 * equal, and the interior of the ring must not self-intersect. Either orientation of the ring is
 * allowed. <p> A ring must have either 0 or 4 or more points. The first and last points must be
 * equal (in 2D). If these conditions are not met, the constructors throw an {link
 * IllegalArgumentException}
 *
 * @version 1.7
 */
@SerialVersionUID(-4261142084085851829L)
object LinearRing {

  /**
   * The minimum number of vertices allowed in a valid non-empty ring (= 4). Empty rings with 0
   * vertices are also valid.
   */
  val MINIMUM_VALID_SIZE = 4
}

@SerialVersionUID(-4261142084085851829L)
class LinearRing(val point: CoordinateSequence, val factor: GeometryFactory)

// /**
//  * Constructs a <code>LinearRing</code> with the vertices
//  * specified by the given {link CoordinateSequence}.
//  *
//  * @param  points a sequence points forming a closed and simple linestring, or
//  *                <code>null</code> to create the empty geometry.
//  * throws IllegalArgumentException if the ring is not closed, or has too few points
//  *
//  */
    extends LineString(point, factor) {
  validateConstruction()

  /**
   * This method is ONLY used to avoid deprecation warnings.
   *
   * @param points
   * @param factory
   *   throws IllegalArgumentException if the ring is not closed, or has too few points
   */
  def this(points: Array[Coordinate], factory: GeometryFactory) =
    this(factory.getCoordinateSequenceFactory.create(points), factory)

  /**
   * Constructs a <code>LinearRing</code> with the given points.
   *
   * @param points
   *   points forming a closed and simple linestring, or <code>null</code> or an empty array to
   *   create the empty geometry. This array must not contain <code>null</code> elements.
   * @param precisionModel
   *   the specification of the grid of allowable points for this <code>LinearRing</code>
   * @param SRID
   *   the ID of the Spatial Reference System used by this <code>LinearRing</code> throws
   *   IllegalArgumentException if the ring is not closed, or has too few points
   * @deprecated
   *   Use GeometryFactory instead
   */
  def this(points: Array[Coordinate], precisionModel: PrecisionModel, SRID: Int) = {
    this(points, new GeometryFactory(precisionModel, SRID))
    validateConstruction()
  }

  private def validateConstruction(): Unit = {
    if (!isEmpty && !super.isClosed)
      throw new IllegalArgumentException("Points of LinearRing do not form a closed linestring")
    if (
      getCoordinateSequence.size >= 1 && getCoordinateSequence.size < LinearRing.MINIMUM_VALID_SIZE
    )
      throw new IllegalArgumentException(
        "Invalid number of points in LinearRing (found " + getCoordinateSequence.size + " - must be 0 or >= 4)"
      )
  }

  /**
   * Returns <code>Dimension.FALSE</code>, since by definition LinearRings do not have a boundary.
   *
   * return Dimension.FALSE
   */
  override def getBoundaryDimension: Int = Dimension.FALSE

  /**
   * Tests whether this ring is closed. Empty rings are closed by definition.
   *
   * return true if this ring is closed
   */
  override def isClosed: Boolean = {
    if (isEmpty) { // empty LinearRings are closed by definition
      return true
    }
    super.isClosed
  }

  override def getGeometryType = "LinearRing"

  override protected def getSortIndex: Int = Geometry.SORTINDEX_LINEARRING

  override protected def copyInternal = new LinearRing(points.copy, factory)

  /** @deprecated */
  override def reverse: Geometry = super.reverse

  override def reverseInternal: LinearRing = {
    val seq = points.copy
    CoordinateSequences.reverse(seq)
    getFactory.createLinearRing(seq)
  }
}
