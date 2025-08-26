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

import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.geom.Coordinate

/**
 * Computes the possible intersections between two line segments in {link NodedSegmentString}s and
 * adds them to each string using {link NodedSegmentString#addIntersection(LineIntersector, int,
 * int, int)}.
 *
 * @version 1.7
 */
object IntersectionAdder {
  def isAdjacentSegments(i1: Int, i2: Int): Boolean = Math.abs(i1 - i2) == 1
}

class IntersectionAdder(var li: LineIntersector) extends SegmentIntersector {

  /**
   * These variables keep track of what types of intersections were found during ALL edges that have
   * been intersected.
   */
  private var vhasIntersection                    = false
  private var hasProper                           = false
  private var hasProperInterior                   = false
  private var hasInterior                         = false
  // the proper intersection point found
  private val properIntersectionPoint: Coordinate = null
  // private val isSelfIntersection = false
  // private boolean intersectionFound;
  var numIntersections                            = 0
  var numInteriorIntersections                    = 0
  var numProperIntersections                      = 0
  // testing only
  var numTests                                    = 0

  def getLineIntersector: LineIntersector = li

  /**
   * return the proper intersection point, or <code>null</code> if none was found
   */
  def getProperIntersectionPoint: Coordinate = properIntersectionPoint

  def hasIntersection: Boolean = vhasIntersection

  /**
   * A proper intersection is an intersection which is interior to at least two line segments. Note
   * that a proper intersection is not necessarily in the interior of the entire Geometry, since
   * another edge may have an endpoint equal to the intersection, which according to SFS semantics
   * can result in the point being on the Boundary of the Geometry.
   */
  def hasProperIntersection: Boolean = hasProper

  /**
   * A proper interior intersection is a proper intersection which is <b>not</b> contained in the
   * set of boundary nodes set for this SegmentIntersector.
   */
  def hasProperInteriorIntersection: Boolean = hasProperInterior

  /**
   * An interior intersection is an intersection which is in the interior of some segment.
   */
  def hasInteriorIntersection: Boolean = hasInterior

  /**
   * A trivial intersection is an apparent self-intersection which in fact is simply the point
   * shared by adjacent line segments. Note that closed edges require a special check for the point
   * shared by the beginning and end segments.
   */
  private def isTrivialIntersection(
    e0:        SegmentString,
    segIndex0: Int,
    e1:        SegmentString,
    segIndex1: Int
  ): Boolean = {
    if (e0 eq e1) if (li.getIntersectionNum == 1) {
      if (IntersectionAdder.isAdjacentSegments(segIndex0, segIndex1)) return true
      if (e0.isClosed) {
        val maxSegIndex = e0.size - 1
        if (
          (segIndex0 == 0 && segIndex1 == maxSegIndex) || (segIndex1 == 0 && segIndex0 == maxSegIndex)
        ) return true
      }
    }
    false
  }

  /**
   * This method is called by clients of the {link SegmentIntersector} class to process
   * intersections for two segments of the {link SegmentString}s being intersected. Note that some
   * clients (such as <code>MonotoneChain</code>s) may optimize away this call for segment pairs
   * which they have determined do not intersect (e.g. by an disjoint envelope test).
   */
  override def processIntersections(
    e0:        SegmentString,
    segIndex0: Int,
    e1:        SegmentString,
    segIndex1: Int
  ): Unit = {
    if ((e0 eq e1) && segIndex0 == segIndex1) return
    numTests += 1
    val p00 = e0.getCoordinates(segIndex0)
    val p01 = e0.getCoordinates(segIndex0 + 1)
    val p10 = e1.getCoordinates(segIndex1)
    val p11 = e1.getCoordinates(segIndex1 + 1)
    li.computeIntersection(p00, p01, p10, p11)
    // if (li.hasIntersection() && li.isProper()) Debug.println(li);
    if (li.hasIntersection) { // intersectionFound = true;
      numIntersections += 1
      if (li.isInteriorIntersection) {
        numInteriorIntersections += 1
        hasInterior = true
        // System.out.println(li);
      }
      // if the segments are adjacent they have at least one trivial intersection,
      // the shared endpoint.  Don't bother adding it if it is the
      // only intersection.
      if (!isTrivialIntersection(e0, segIndex0, e1, segIndex1)) {
        vhasIntersection = true
        e0.asInstanceOf[NodedSegmentString].addIntersections(li, segIndex0, 0)
        e1.asInstanceOf[NodedSegmentString].addIntersections(li, segIndex1, 1)
        if (li.isProperF) {
          numProperIntersections += 1
          // Debug.println(li.toString());  Debug.println(li.getIntersection(0));
          // properIntersectionPoint = (Coordinate) li.getIntersection(0).clone();
          hasProper = true
          hasProperInterior = true
        }
      }
    }
  }

  /**
   * Always process all intersections
   *
   * return false always
   */
  override def isDone = false
}
