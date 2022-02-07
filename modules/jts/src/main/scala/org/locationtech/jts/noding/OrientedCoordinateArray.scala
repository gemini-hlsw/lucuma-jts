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
 */ /*
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
package org.locationtech.jts.noding

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays

/**
 * Allows comparing {link Coordinate} arrays in an orientation-independent way.
 *
 * @author
 *   Martin Davis
 * @version 1.7
 */
object OrientedCoordinateArray {

  /**
   * Computes the canonical orientation for a coordinate array.
   *
   * @param pts
   *   the array to test return <code>true</code> if the points are oriented forwards or
   *   <code>false</code if the points are oriented in reverse
   */
  private def orientation(pts: Array[Coordinate]) = CoordinateArrays.increasingDirection(pts) == 1

  private def compareOriented(
    pts1:         Array[Coordinate],
    orientation1: Boolean,
    pts2:         Array[Coordinate],
    orientation2: Boolean
  ): Int = {
    val dir1   =
      if (orientation1) 1
      else -1
    val dir2   =
      if (orientation2) 1
      else -1
    val limit1 =
      if (orientation1) pts1.length
      else -1
    val limit2 =
      if (orientation2) pts2.length
      else -1
    var i1     =
      if (orientation1) 0
      else pts1.length - 1
    var i2     =
      if (orientation2) 0
      else pts2.length - 1
    while (true) {
      val compPt = pts1(i1).compareTo(pts2(i2))
      if (compPt != 0) return compPt
      i1 += dir1
      i2 += dir2
      val done1  = i1 == limit1
      val done2  = i2 == limit2
      if (done1 && !done2) return -1
      if (!done1 && done2) return 1
      if (done1 && done2) return 0
    }
    0
  }
}

class OrientedCoordinateArray(var pts: Array[Coordinate])
    extends Comparable[OrientedCoordinateArray] {
  private val orientation = OrientedCoordinateArray.orientation(pts)

  /**
   * Compares two {link OrientedCoordinateArray}s for their relative order
   *
   * return -1 this one is smaller; 0 the two objects are equal; 1 this one is greater
   */
  override def compareTo(oca: OrientedCoordinateArray): Int = {
    val comp = OrientedCoordinateArray.compareOriented(pts, orientation, oca.pts, oca.orientation)
    /*
        // MD - testing only
        int oldComp = SegmentStringDissolver.ptsComp.compare(pts, oca.pts);
        if ((oldComp == 0 || comp == 0) && oldComp != comp) {
          System.out.println("bidir mismatch");

          boolean orient1 = orientation(pts);
          boolean orient2 = orientation(oca.pts);
          int comp2 = compareOriented(pts, orientation,
                                   oca.pts, oca.orientation);
          int oldComp2 = SegmentStringDissolver.ptsComp.compare(pts, oca.pts);
        }
     */
    comp
  }
}
