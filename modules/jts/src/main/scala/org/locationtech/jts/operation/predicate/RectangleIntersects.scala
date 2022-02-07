// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.predicate

import java.util
import org.locationtech.jts.algorithm.RectangleLineIntersector
import org.locationtech.jts.algorithm.locate.SimplePointInAreaLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.util.LinearComponentExtracter
import org.locationtech.jts.geom.util.ShortCircuitedGeometryVisitor

/**
 * Implementation of the <tt>intersects</tt> spatial predicate optimized for the case where one
 * {link Geometry} is a rectangle. This class works for all input geometries, including {link
 * GeometryCollection}s. <p> As a further optimization, this class can be used in batch style to
 * test many geometries against a single rectangle.
 *
 * @version 1.7
 */
object RectangleIntersects {

  /**
   * Tests whether a rectangle intersects a given geometry.
   *
   * @param rectangle
   *   a rectangular Polygon
   * @param b
   *   a Geometry of any type return true if the geometries intersect
   */
  def intersects(rectangle: Polygon, b: Geometry): Boolean = {
    val rp = new RectangleIntersects(rectangle)
    rp.intersects(b)
  }
}

class RectangleIntersects(var rectangle: Polygon) {

  /**
   * Create a new intersects computer for a rectangle.
   *
   * @param rectangle
   *   a rectangular Polygon
   */
  private val rectEnv = rectangle.getEnvelopeInternal

  /**
   * Tests whether the given Geometry intersects the query rectangle.
   *
   * @param geom
   *   the Geometry to test (may be of any type) return true if the geometry intersects the query
   *   rectangle
   */
  def intersects(geom: Geometry): Boolean = {
    if (!rectEnv.intersects(geom.getEnvelopeInternal)) return false

    /**
     * Test if rectangle envelope intersects any component envelope. This handles Point components
     * as well
     */
    val visitor = new EnvelopeIntersectsVisitor(rectEnv)
    visitor.applyTo(geom)
    if (visitor.intersects) return true

    /**
     * Test if any rectangle vertex is contained in the target geometry
     */
    val ecpVisitor = new GeometryContainsPointVisitor(rectangle)
    ecpVisitor.applyTo(geom)
    if (ecpVisitor.containsPoint) return true

    /**
     * Test if any target geometry line segment intersects the rectangle
     */
    val riVisitor = new RectangleIntersectsSegmentVisitor(rectangle)
    riVisitor.applyTo(geom)
    if (riVisitor.intersects) return true
    false
  }
}

/**
 * Tests whether it can be concluded that a rectangle intersects a geometry, based on the
 * relationship of the envelope(s) of the geometry.
 *
 * @author
 *   Martin Davis
 * @version 1.7
 */
class EnvelopeIntersectsVisitor(var rectEnv: Envelope) extends ShortCircuitedGeometryVisitor {
  private var vintersects = false

  /**
   * Reports whether it can be concluded that an intersection occurs, or whether further testing is
   * required.
   *
   * return true if an intersection must occur or false if no conclusion about intersection can be
   * made
   */
  def intersects: Boolean = vintersects

  override protected def visit(element: Geometry): Unit = {
    val elementEnv = element.getEnvelopeInternal
    // disjoint => no intersection
    if (!rectEnv.intersects(elementEnv)) return
    // rectangle contains target env => must intersect
    if (rectEnv.contains(elementEnv)) {
      vintersects = true
      return
    }

    /**
     * Since the envelopes intersect and the test element is connected, if the test envelope is
     * completely bisected by an edge of the rectangle the element and the rectangle must touch
     * (This is basically an application of the Jordan Curve Theorem). The alternative situation is
     * that the test envelope is "on a corner" of the rectangle envelope, i.e. is not completely
     * bisected. In this case it is not possible to make a conclusion about the presence of an
     * intersection.
     */
    if (elementEnv.getMinX >= rectEnv.getMinX && elementEnv.getMaxX <= rectEnv.getMaxX) {
      vintersects = true
      return
    }
    if (elementEnv.getMinY >= rectEnv.getMinY && elementEnv.getMaxY <= rectEnv.getMaxY) {
      vintersects = true
      return
    }
  }

  override protected def isDone: Boolean = vintersects == true
}

/**
 * A visitor which tests whether it can be concluded that a geometry contains a vertex of a query
 * geometry.
 *
 * @author
 *   Martin Davis
 * @version 1.7
 */
class GeometryContainsPointVisitor(val rectangle: Polygon) extends ShortCircuitedGeometryVisitor {
  private val rectSeq        = rectangle.getExteriorRing.getCoordinateSequence
  private val rectEnv        = rectangle.getEnvelopeInternal
  private var vcontainsPoint = false

  /**
   * Reports whether it can be concluded that a corner point of the rectangle is contained in the
   * geometry, or whether further testing is required.
   *
   * return true if a corner point is contained or false if no conclusion about intersection can be
   * made
   */
  def containsPoint: Boolean = vcontainsPoint

  override protected def visit(geom: Geometry): Unit = { // if test geometry is not polygonal this check is not needed
    if (!geom.isInstanceOf[Polygon]) return
    // skip if envelopes do not intersect
    val elementEnv = geom.getEnvelopeInternal
    if (!rectEnv.intersects(elementEnv)) return
    // test each corner of rectangle for inclusion
    val rectPt     = new Coordinate
    var i          = 0
    while (i < 4) {
      rectSeq.getCoordinate(i, rectPt)
      if (elementEnv.contains(rectPt)) {
        // check rect point in poly (rect is known not to touch polygon at this
        // point)
        if (SimplePointInAreaLocator.containsPointInPolygon(rectPt, geom.asInstanceOf[Polygon])) {
          vcontainsPoint = true
          return
        }
        i += 1
      }
    }
  }

  override protected def isDone: Boolean = vcontainsPoint
}

/**
 * A visitor to test for intersection between the query rectangle and the line segments of the
 * geometry.
 *
 * @author
 *   Martin Davis
 */
class RectangleIntersectsSegmentVisitor(val rectangle: Polygon)

/**
 * Creates a visitor for checking rectangle intersection
 * with segments
 *
 * @param rectangle the query rectangle
 */ extends ShortCircuitedGeometryVisitor {
  private val rectEnv         = rectangle.getEnvelopeInternal
  private val rectIntersector = new RectangleLineIntersector(rectEnv)
  private var hasIntersection = false
  private val p0              = new Coordinate
  private val p1              = new Coordinate

  /**
   * Reports whether any segment intersection exists.
   *
   * return true if a segment intersection exists or false if no segment intersection exists
   */
  def intersects: Boolean = hasIntersection

  override protected def visit(geom: Geometry): Unit = {

    /**
     * It may be the case that the rectangle and the envelope of the geometry component are
     * disjoint, so it is worth checking this simple condition.
     */
    val elementEnv = geom.getEnvelopeInternal
    if (!rectEnv.intersects(elementEnv)) return
    // check segment intersections
    // get all lines from geometry component
    // (there may be more than one if it's a multi-ring polygon)
    val lines      = LinearComponentExtracter.getLines(geom)
    checkIntersectionWithLineStrings(lines)
  }

  private def checkIntersectionWithLineStrings(lines: util.List[_]): Unit = {
    val i = lines.iterator
    while (i.hasNext) {
      val testLine = i.next.asInstanceOf[LineString]
      checkIntersectionWithSegments(testLine)
      if (hasIntersection) return
    }
  }

  private def checkIntersectionWithSegments(testLine: LineString): Unit = {
    val seq1 = testLine.getCoordinateSequence
    var j    = 1
    while (j < seq1.size) {
      seq1.getCoordinate(j - 1, p0)
      seq1.getCoordinate(j, p1)
      if (rectIntersector.intersects(p0, p1)) {
        hasIntersection = true
        return
      }
      {
        j += 1; j - 1
      }
    }
  }

  override protected def isDone: Boolean = hasIntersection
}
