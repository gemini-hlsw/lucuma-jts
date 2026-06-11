// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2024 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import java.util.Comparator

import org.locationtech.jts.algorithm.PolygonNodeTopology
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.io.WKTWriter

/**
 * Represents a computed node along with the incident edges on either side of it (if they exist).
 * This captures the information about a node in a geometry component required to determine the
 * component's contribution to the node topology. A node in an area geometry always has edges on
 * both sides of the node. A node in a linear geometry may have one or other incident edge missing,
 * if the node occurs at an endpoint of the line. The edges of an area node are assumed to be
 * provided with CW-shell orientation (as per JTS norm). This must be enforced by the caller.
 *
 * @author
 *   Martin Davis
 */
class NodeSection(
  val isA:            Boolean,
  val dimension:      Int,
  val id:             Int,
  val ringId:         Int,
  private val poly:   Geometry,
  val isNodeAtVertex: Boolean,
  private val v0:     Coordinate,
  val nodePt:         Coordinate,
  private val v1:     Coordinate
) extends Comparable[NodeSection] {

  def getVertex(i: Int): Coordinate =
    if (i == 0) v0 else v1

  /**
   * Gets the polygon this section is part of. Will be null if section is not on a polygon boundary.
   *
   * return the associated polygon, or null
   */
  def getPolygonal: Geometry = poly

  def isShell: Boolean = ringId == 0

  def isArea: Boolean = dimension == Dimension.A

  def isSameGeometry(ns: NodeSection): Boolean = isA == ns.isA

  def isSamePolygon(ns: NodeSection): Boolean = isA == ns.isA && id == ns.id

  def isProper: Boolean = !isNodeAtVertex

  override def toString: String = {
    val geomName    = RelateGeometry.name(isA)
    val atVertexInd = if (isNodeAtVertex) "-V-" else "---"
    val polyId      = if (id >= 0) "[" + id + ":" + ringId + "]" else ""
    s"$geomName$dimension$polyId: ${edgeRep(v0, nodePt)} $atVertexInd ${edgeRep(nodePt, v1)}"
  }

  private def edgeRep(p0: Coordinate, p1: Coordinate): String = {
    if (p0 == null || p1 == null)
      return "null"
    WKTWriter.toLineString(p0, p1)
  }

  /**
   * Compare node sections by parent geometry, dimension, element id and ring id, and edge vertices.
   * Sections are assumed to be at the same node point.
   */
  def compareTo(o: NodeSection): Int = {
    // Assert: nodePt.equals2D(o.nodePt)

    // sort A before B
    if (isA != o.isA) {
      if (isA) return -1
      return 1
    }
    // -- sort on dimensions
    val compDim = Integer.compare(dimension, o.dimension)
    if (compDim != 0) return compDim

    // -- sort on id and ring id
    val compId = Integer.compare(id, o.id)
    if (compId != 0) return compId

    val compRingId = Integer.compare(ringId, o.ringId)
    if (compRingId != 0) return compRingId

    // -- sort on edge coordinates
    val compV0 = NodeSection.compareWithNull(v0, o.v0)
    if (compV0 != 0) return compV0

    NodeSection.compareWithNull(v1, o.v1)
  }

}

object NodeSection {

  /**
   * Compares sections by the angle the entering edge makes with the positive X axis.
   */
  class EdgeAngleComparator extends Comparator[NodeSection] {

    override def compare(ns1: NodeSection, ns2: NodeSection): Int =
      PolygonNodeTopology.compareAngle(ns1.nodePt, ns1.getVertex(0), ns2.getVertex(0))
  }

  def isAreaArea(a: NodeSection, b: NodeSection): Boolean =
    a.dimension == Dimension.A && b.dimension == Dimension.A

  def isProper(a: NodeSection, b: NodeSection): Boolean =
    a.isProper && b.isProper

  private def compareWithNull(v0: Coordinate, v1: Coordinate): Int = {
    if (v0 == null) {
      if (v1 == null)
        return 0
      // -- null is lower than non-null
      return -1
    }
    // v0 is non-null
    if (v1 == null)
      return 1
    v0.compareTo(v1)
  }

}
