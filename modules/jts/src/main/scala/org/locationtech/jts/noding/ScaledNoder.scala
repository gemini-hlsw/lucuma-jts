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

import java.util
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays

/**
 * Wraps a {link Noder} and transforms its input into the integer domain. This is intended for use
 * with Snap-Rounding noders, which typically are only intended to work in the integer domain.
 * Offsets can be provided to increase the number of digits of available precision. <p> Clients
 * should be aware that rescaling can involve loss of precision, which can cause zero-length line
 * segments to be created. These in turn can cause problems when used to build a planar graph. This
 * situation should be checked for and collapsed segments removed if necessary.
 *
 * @version 1.7
 */
class ScaledNoder(
  var noder:       Noder[SegmentString],
  var scaleFactor: Double,
  val offsetX:     Double,
  val offsetY:     Double
) extends Noder[SegmentString] { // no need to scale if input precision is already integral
//  private val offsetX = .0
//  private val offsetY = .0
  private val isScaled = !isIntegerPrecision

  def this(noder: Noder[SegmentString], scaleFactor: Double) =
    this(noder, scaleFactor, 0, 0)

  def isIntegerPrecision: Boolean = scaleFactor == 1.0

  override def getNodedSubstrings: util.Collection[SegmentString] = {
    val splitSS = noder.getNodedSubstrings
    if (isScaled) rescale(splitSS)
    splitSS
  }

  override def computeNodes(inputSegStrings: util.Collection[SegmentString]): Unit = {
    var intSegStrings = inputSegStrings
    if (isScaled) intSegStrings = scale(inputSegStrings)
    noder.computeNodes(intSegStrings)
  }

  private def scale(segStrings: util.Collection[SegmentString]): util.List[SegmentString] = {
    val nodedSegmentStrings = new util.ArrayList[SegmentString](segStrings.size)
    val i                   = segStrings.iterator
    while (i.hasNext) {
      val ss  = i.next
      val str = new NodedSegmentString(scale(ss.getCoordinates), ss.getData)
      nodedSegmentStrings.add(str)
    }
    nodedSegmentStrings
  }

  private def scale(pts: Array[Coordinate]): Array[Coordinate] = {
    val roundPts      = new Array[Coordinate](pts.length)
    var i             = 0
    while (i < pts.length) {
      roundPts(i) = new Coordinate(Math.round((pts(i).x - offsetX) * scaleFactor).toDouble,
                                   Math.round((pts(i).y - offsetY) * scaleFactor).toDouble,
                                   pts(i).getZ
      )
      i += 1
    }
    val roundPtsNoDup = CoordinateArrays.removeRepeatedPoints(roundPts)
    roundPtsNoDup
  }

  private def rescale(segStrings: util.Collection[SegmentString]): Unit = {
    val i = segStrings.iterator
    while (i.hasNext) {
      val ss = i.next
      rescale(ss.getCoordinates)
    }
  }

  private def rescale(pts: Array[Coordinate]): Unit = {
    var i = 0
    while (i < pts.length) {
      pts(i).x = pts(i).x / scaleFactor + offsetX
      pts(i).y = pts(i).y / scaleFactor + offsetY
      i += 1
    }
    if (pts.length == 2 && pts(0).equals2D(pts(1))) System.out.println(pts)
  }

  //private double rescale(double val) { return val / scaleFactor; }
}
