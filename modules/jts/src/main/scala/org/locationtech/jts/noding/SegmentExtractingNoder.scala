// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.noding;

import org.locationtech.jts.geom.Coordinate

import java.util.ArrayList
import java.util.Collection
import java.util.List
import scala.jdk.CollectionConverters._

/**
 * A noder which extracts all line segments as {@link SegmentString}s. This enables fast overlay of
 * geometries which are known to be already fully noded. In particular, it provides fast union of
 * polygonal and linear coverages. Unioning a noded set of lines is an effective way to perform line
 * merging and line dissolving. <p> No precision reduction is carried out. If that is required,
 * another noder must be used (such as a snap-rounding noder), or the input must be
 * precision-reduced beforehand.
 *
 * @author
 *   Martin Davis
 */
class SegmentExtractingNoder(var segList: List[SegmentString] = null) extends Noder[SegmentString] {

  override def computeNodes(segStrings: Collection[SegmentString]) =
    segList = SegmentExtractingNoder.extractSegments(segStrings)

  override def getNodedSubstrings: Collection[SegmentString] =
    segList
}

object SegmentExtractingNoder {

  def extractSegments(segStrings: Collection[SegmentString]): List[SegmentString] = {
    val segList = new ArrayList[SegmentString]()
    for (ss <- segStrings.asScala)
      extractSegments(ss, segList)
    segList
  }

  def extractSegments(ss: SegmentString, segList: List[SegmentString]): Unit =
    for (i <- 0 until ss.size - 1) {
      val p0  = ss.getCoordinate(i)
      val p1  = ss.getCoordinate(i + 1)
      val seg = new BasicSegmentString(Array[Coordinate](p0, p1), ss.getData)
      segList.add(seg)
    }

}
