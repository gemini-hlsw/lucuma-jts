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
package org.locationtech.jts.noding

import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.TopologyException

import java.util
//import org.locationtech.jts.io.WKTWriter

// /**
//  * Validates that a collection of {link SegmentString}s is correctly noded.
//  * Indexing is used to improve performance.
//  * By default validation stops after a single
//  * non-noded intersection is detected.
//  * Alternatively, it can be requested to detect all intersections
//  * by using {link #setFindAllIntersections(boolean)}.
//  * <p>
//  * The validator does not check for topology collapse situations
//  * (e.g. where two segment strings are fully co-incident).
//  * <p>
//  * The validator checks for the following situations which indicated incorrect noding:
//  * <ul>
//  * <li>Proper intersections between segments (i.e. the intersection is interior to both segments)
//  * <li>Intersections at an interior vertex (i.e. with an endpoint or another interior vertex)
//  * </ul>
//  * <p>
//  * The client may either test the {link #isValid()} condition,
//  * or request that a suitable {link TopologyException} be thrown.
//  *
//  * @version 1.7
//  * @see NodingIntersectionFinder
//  */
object FastNodingValidator {

  /**
   * Gets a list of all intersections found. Intersections are represented as {link Coordinate}s.
   * List is empty if none were found.
   *
   * @param segStrings
   *   a collection of SegmentStrings return a list of Coordinate
   */
  def computeIntersections(segStrings: util.Collection[SegmentString]): util.List[?] = {
    val nv = new FastNodingValidator(segStrings)
    nv.setFindAllIntersections(true)
    nv.isValid()
    nv.getIntersections
  }
}

class FastNodingValidator(val segStrings: util.Collection[SegmentString]) {

  /**
   * Creates a new noding validator for a given set of linework.
   *
   * @param segStrings
   *   a collection of {@link SegmentString} s
   */
  private val li                               = new RobustLineIntersector
  private var findAllIntersections             = false
  private var segInt: NodingIntersectionFinder = null
  private var visValid                         = true

  def setFindAllIntersections(findAllIntersections: Boolean): Unit = this.findAllIntersections =
    findAllIntersections

  /**
   * Gets a list of all intersections found. Intersections are represented as {link Coordinate}s.
   * List is empty if none were found.
   *
   * return a list of Coordinate
   */
  def getIntersections: util.ArrayList[?] = segInt.getIntersections

  /**
   * Checks for an intersection and reports if one is found.
   *
   * return true if the arrangement contains an interior intersection
   */
  def isValid(): Boolean = {
    execute()
    visValid
  }

  /**
   * Returns an error message indicating the segments containing the intersection.
   *
   * return an error message documenting the intersection location
   */
  def getErrorMessage: String = {
    if (visValid) return "no intersections found"
//    val intSegs = segInt.getIntersectionSegments
    // "found non-noded intersection between " + WKTWriter.toLineString(intSegs(0), intSegs(1)) + " and " + WKTWriter.toLineString(intSegs(2), intSegs(3))
    "found non-noded intersection between "
  }

  /**
   * Checks for an intersection and throws a TopologyException if one is found.
   *
   * throws TopologyException if an intersection is found
   */
  def checkValid(): Unit = {
    execute()
    if (!visValid) throw new TopologyException(getErrorMessage, segInt.getIntersection)
  }

  private def execute(): Unit = {
    if (segInt != null) return
    checkInteriorIntersections()
  }

  private def checkInteriorIntersections(): Unit = {

    /**
     * MD - It may even be reliable to simply check whether end segments (of SegmentStrings) have an
     * interior intersection, since noding should have split any true interior intersections
     * already.
     */
    visValid = true
    segInt = new NodingIntersectionFinder(li)
    segInt.setFindAllIntersections(findAllIntersections)
    val noder = new MCIndexNoder
    noder.setSegmentIntersector(segInt)
    noder.computeNodes(segStrings)
    if (segInt.hasIntersection) {
      visValid = false
      return
    }
  }
}
