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
package org.locationtech.jts.precision

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateFilter
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequenceFilter
import org.locationtech.jts.geom.Geometry

/**
 * Removes common most-significant mantissa bits
 * from one or more {link Geometry}s.
 * <p>
 * The CommonBitsRemover "scavenges" precision
 * which is "wasted" by a large displacement of the geometry
 * from the origin.
 * For example, if a small geometry is displaced from the origin
 * by a large distance,
 * the displacement increases the significant figures in the coordinates,
 * but does not affect the <i>relative</i> topology of the geometry.
 * Thus the geometry can be translated back to the origin
 * without affecting its topology.
 * In order to compute the translation without affecting
 * the full precision of the coordinate values,
 * the translation is performed at the bit level by
 * removing the common leading mantissa bits.
 * <p>
 * If the geometry envelope already contains the origin,
 * the translation procedure cannot be applied.
 * In this case, the common bits value is computed as zero.
 * <p>
 * If the geometry crosses the Y axis but not the X axis
 * (and <i>mutatis mutandum</i>),
 * the common bits for Y are zero,
 * but the common bits for X are non-zero.
 *
 * @version 1.7
 */
object CommonBitsRemover {

  private[precision] class CommonCoordinateFilter extends CoordinateFilter {
    private val commonBitsX = new CommonBits
    private val commonBitsY = new CommonBits

    override def filter(coord: Coordinate): Unit = {
      commonBitsX.add(coord.x)
      commonBitsY.add(coord.y)
    }

    def getCommonCoordinate = new Coordinate(commonBitsX.getCommon, commonBitsY.getCommon)
  }

  private[precision] class Translater(val trans: Coordinate) extends CoordinateSequenceFilter {
//    this.trans = trans
//    private[precision] var trans = null

    override def filter(seq: CoordinateSequence, i: Int): Unit = {
      val xp = seq.getOrdinate(i, 0) + trans.x
      val yp = seq.getOrdinate(i, 1) + trans.y
      seq.setOrdinate(i, 0, xp)
      seq.setOrdinate(i, 1, yp)
    }

    override def isDone = false

    override def isGeometryChanged = true
  }

}

class CommonBitsRemover() {
  private var commonCoord: Coordinate = null
  private val ccFilter                = new CommonBitsRemover.CommonCoordinateFilter

  /**
   * Add a geometry to the set of geometries whose common bits are
   * being computed.  After this method has executed the
   * common coordinate reflects the common bits of all added
   * geometries.
   *
   * @param geom a Geometry to test for common bits
   */
  def add(geom: Geometry): Unit = {
    geom.applyF(ccFilter)
    commonCoord = ccFilter.getCommonCoordinate
  }

  /**
   * The common bits of the Coordinates in the supplied Geometries.
   */
  def getCommonCoordinate: Coordinate = commonCoord

  /**
   * Removes the common coordinate bits from a Geometry.
   * The coordinates of the Geometry are changed.
   *
   * @param geom the Geometry from which to remove the common coordinate bits
   * return the shifted Geometry
   */
  def removeCommonBits(geom: Geometry): Geometry = {
    if ((commonCoord.x == 0.0) && (commonCoord.y == 0.0)) return geom
    val invCoord = new Coordinate(commonCoord)
    invCoord.x = -invCoord.x
    invCoord.y = -invCoord.y
    val trans    = new CommonBitsRemover.Translater(invCoord)
    geom.applyF(trans)
    geom.geometryChanged()
    geom
  }

  /**
   * Adds the common coordinate bits back into a Geometry.
   * The coordinates of the Geometry are changed.
   *
   * @param geom the Geometry to which to add the common coordinate bits
   */
  def addCommonBits(geom: Geometry): Unit = {
    val trans = new CommonBitsRemover.Translater(commonCoord)
    geom.applyF(trans)
    geom.geometryChanged()
  }
}
