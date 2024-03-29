// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2020 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.noding.snap

/*
 * Copyright (c) 2020 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.noding.MCIndexNoder
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.Noder
import org.locationtech.jts.noding.SegmentString

import java.util
import scala.jdk.CollectionConverters._

/*
 * Copyright (c) 2020 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * Nodes a set of segment strings snapping vertices and intersection points together if they lie
 * within the given snap tolerance distance. Vertices take priority over intersection points for
 * snapping. Input segment strings are generally only split at true node points (i.e. the output
 * segment strings are of maximal length in the output arrangement). <p> The snap tolerance should
 * be chosen to be as small as possible while still producing a correct result. It probably only
 * needs to be small enough to eliminate "nearly-coincident" segments, for which intersection points
 * cannot be computed accurately. This implies a factor of about 10e-12 smaller than the magnitude
 * of the segment coordinates. <p> With an appropriate snap tolerance this algorithm appears to be
 * very robust. So far no failure cases have been found, given a small enough snap tolerance. <p>
 * The correctness of the output is not verified by this noder. If required this can be done by
 * {@link ValidatingNoder}.
 *
 * @version 1.17
 */
class SnappingNoder(var snapTolerance: Double) extends Noder[SegmentString] {
  private val snapIndex                             = new SnappingPointIndex(snapTolerance)
  private var nodedResult: util.List[SegmentString] = null

  /**
   * @return
   *   a Collection of NodedSegmentStrings representing the substrings
   */
  override def getNodedSubstrings: util.Collection[SegmentString] = nodedResult

  /**
   * @param inputSegStrings
   *   a Collection of SegmentStrings
   */
  override def computeNodes(inputSegStrings: util.Collection[SegmentString]): Unit = {
    val snappedSS: util.List[NodedSegmentString] = snapVertices(inputSegStrings)
    nodedResult = snapIntersections(snappedSS)
  }

  private def snapVertices(
    segStrings: util.Collection[SegmentString]
  ): util.List[NodedSegmentString] = {
    val nodedStrings = new util.ArrayList[NodedSegmentString]
    for (ss <- segStrings.asScala)
      nodedStrings.add(snapVertices(ss))
    nodedStrings
  }

  private def snapVertices(ss: SegmentString): NodedSegmentString = {
    val snapCoords = snap(ss.getCoordinates)
    new NodedSegmentString(snapCoords, ss.getData)
  }

  private def snap(coords: Array[Coordinate]) = {
    val snapCoords = new CoordinateList
    for (i <- 0 until coords.length) {
      val pt = snapIndex.snap(coords(i))
      snapCoords.add(pt, false)
    }
    snapCoords.toCoordinateArray
  }

  /**
   * Computes all interior intersections in the collection of {@link SegmentString}s, and returns
   * their {@link Coordinate}s.
   *
   * Also adds the intersection nodes to the segments.
   *
   * @return
   *   a list of Coordinates for the intersections
   */
  private def snapIntersections(inputSS: util.List[NodedSegmentString]) = {
    val intAdder = new SnappingIntersectionAdder(snapTolerance, snapIndex)

    /**
     * Use an overlap tolerance to ensure all possible snapped intersections are found
     */
    val noder = new MCIndexNoder(intAdder, 2 * snapTolerance)
    noder.computeNodes(inputSS.asScala.collect { case s: SegmentString =>
      s: SegmentString
    }.asJavaCollection)
    noder.getNodedSubstrings
  }
}
