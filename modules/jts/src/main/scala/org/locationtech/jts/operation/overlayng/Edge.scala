// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Location
import org.locationtech.jts.io.WKTWriter

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

/**
 * Represents the linework for edges in the topology derived from (up to) two parent geometries. An
 * edge may be the result of the merging of two or more edges which have the same linework (although
 * possibly different orientations). In this case the topology information is derived from the
 * merging of the information in the source edges. Merged edges can occur in the following
 * situations <ul> <li>Due to coincident edges of polygonal or linear geometries. <li>Due to
 * topology collapse caused by snapping or rounding of polygonal geometries. </ul> The source edges
 * may have the same parent geometry, or different ones, or a mix of the two.
 *
 * @author
 *   mdavis
 */
object Edge {

  /**
   * Tests if the given point sequence is a collapsed line. A collapsed edge has fewer than two
   * distinct points.
   *
   * @param pts
   *   the point sequence to check
   * @return
   *   true if the points form a collapsed line
   */
  def isCollapsed(pts: Array[Coordinate]): Boolean = {
    if (pts.length < 2) return true
    // zero-length line
    if (pts(0).equals2D(pts(1))) return true
    // TODO: is pts > 2 with equal points ever expected?
    if (pts.length > 2) if (pts(pts.length - 1).equals2D(pts(pts.length - 2))) return true
    false
  }

  /**
   * Populates the label for an edge resulting from an input geometry.
   *
   * <ul> <li>If the edge is not part of the input, the label is left as NOT_PART <li>If input is an
   * Area and the edge is on the boundary (which may include some collapses), edge is marked as an
   * AREA edge and side locations are assigned <li>If input is an Area and the edge is collapsed
   * (depth delta = 0), the label is set to COLLAPSE. The location will be determined later by
   * evaluating the final graph topology. <li>If input is a Line edge is set to a LINE edge. For
   * line edges the line location is not significant (since there is no parent area for which to
   * determine location). </ul>
   *
   * @param lbl
   * @param geomIndex
   * @param dim
   * @param depthDelta
   */
  private def initLabel(
    lbl:        OverlayLabel,
    geomIndex:  Int,
    dim:        Int,
    depthDelta: Int,
    isHole:     Boolean
  ): Unit = {
    val dimLabel = labelDim(dim, depthDelta)
    dimLabel match {
      case OverlayLabel.DIM_NOT_PART =>
        lbl.initNotPart(geomIndex)

      case OverlayLabel.DIM_BOUNDARY =>
        lbl.initBoundary(geomIndex, locationLeft(depthDelta), locationRight(depthDelta), isHole)

      case OverlayLabel.DIM_COLLAPSE =>
        lbl.initCollapse(geomIndex, isHole)

      case OverlayLabel.DIM_LINE =>
        lbl.initLine(geomIndex)

    }
  }

  private def labelDim(dim: Int, depthDelta: Int): Int = {
    if (dim == Dimension.FALSE) return OverlayLabel.DIM_NOT_PART
    if (dim == Dimension.L) return OverlayLabel.DIM_LINE
    // assert: dim is A
    val isCollapse = depthDelta == 0
    if (isCollapse) return OverlayLabel.DIM_COLLAPSE
    OverlayLabel.DIM_BOUNDARY
  }

  private def locationRight(depthDelta: Int): Int = {
    val delSign0 = delSign(depthDelta)
    delSign0 match {
      case 0  =>
        return OverlayLabel.LOC_UNKNOWN
      case 1  =>
        return Location.INTERIOR
      case -1 =>
        return Location.EXTERIOR
    }
    OverlayLabel.LOC_UNKNOWN
  }

  private def locationLeft(depthDelta: Int): Int = { // TODO: is it always safe to ignore larger depth deltas?
    val delSign0 = delSign(depthDelta)
    delSign0 match {
      case 0  =>
        OverlayLabel.LOC_UNKNOWN
      case 1  =>
        Location.EXTERIOR
      case -1 =>
        Location.INTERIOR
      case _  =>
        OverlayLabel.LOC_UNKNOWN
    }
  }

  private def delSign(depthDel: Int): Int = {
    if (depthDel > 0) return 1
    if (depthDel < 0) return -1
    0
  }

  private def isHoleMerged(geomIndex: Int, edge1: Edge, edge2: Edge) = { // TOD: this might be clearer with tri-state logic for isHole?
    val isShell1      = edge1.isShell(geomIndex)
    val isShell2      = edge2.isShell(geomIndex)
    val isShellMerged = isShell1 || isShell2
    // flip since isHole is stored
    !isShellMerged
  }

  private def toStringPts(pts: Array[Coordinate]) = {
    val orig     = pts(0)
    val dest     = pts(pts.length - 1)
    val dirPtStr =
      if (pts.length > 2) ", " + WKTWriter.format(pts(1))
      else ""
    val ptsStr   = WKTWriter.format(orig) + dirPtStr + " .. " + WKTWriter.format(dest)
    ptsStr
  }

  def infoString(index: Int, dim: Int, isHole: Boolean, depthDelta: Int): String =
    (if (index == 0) "A:"
     else
       "B:") + OverlayLabel.dimensionSymbol(dim) + ringRoleSymbol(dim, isHole) + Integer.toString(
      depthDelta
    ) // force to string

  private def ringRoleSymbol(dim: Int, isHole: Boolean): String = {
    if (hasAreaParent(dim)) return "" + OverlayLabel.ringRoleSymbol(isHole)
    ""
  }

  private def hasAreaParent(dim: Int) =
    dim == OverlayLabel.DIM_BOUNDARY || dim == OverlayLabel.DIM_COLLAPSE
}

class Edge(var pts: Array[Coordinate], val info: EdgeSourceInfo) {
  private var aDim        = OverlayLabel.DIM_UNKNOWN
  private var aDepthDelta = 0
  private var aIsHole     = false
  private var bDim        = OverlayLabel.DIM_UNKNOWN
  private var bDepthDelta = 0
  private var bIsHole     = false
  copyInfo(info)

  def getCoordinates: Array[Coordinate] = pts

  def getCoordinate(index: Int): Coordinate = pts(index)

  def size: Int = pts.length

  def direction: Boolean = {
    val pts  = getCoordinates
    if (pts.length < 2) throw new IllegalStateException("Edge must have >= 2 points")
    val p0   = pts(0)
    val p1   = pts(1)
    val pn0  = pts(pts.length - 1)
    val pn1  = pts(pts.length - 2)
    var cmp  = 0
    val cmp0 = p0.compareTo(pn0)
    if (cmp0 != 0) cmp = cmp0
    if (cmp == 0) {
      val cmp1 = p1.compareTo(pn1)
      if (cmp1 != 0) cmp = cmp1
    }
    if (cmp == 0)
      throw new IllegalStateException(
        "Edge direction cannot be determined because endpoints are equal"
      )
    if (cmp == -1) true
    else false
  }

  /**
   * Compares two coincident edges to determine whether they have the same or opposite direction.
   *
   * @param edge1
   *   an edge
   * @param edge2
   *   an edge
   * @return
   *   true if the edges have the same direction, false if not
   */
  def relativeDirection(edge2: Edge): Boolean = { // assert: the edges match (have the same coordinates up to direction)
    if (!getCoordinate(0).equals2D(edge2.getCoordinate(0))) return false
    if (!getCoordinate(1).equals2D(edge2.getCoordinate(1))) return false
    true
  }

  def createLabel: OverlayLabel = {
    val lbl = new OverlayLabel
    Edge.initLabel(lbl, 0, aDim, aDepthDelta, aIsHole)
    Edge.initLabel(lbl, 1, bDim, bDepthDelta, bIsHole)
    lbl
  }

  /**
   * Tests whether the edge is part of a shell in the given geometry. This is only the case if the
   * edge is a boundary.
   *
   * @param geomIndex
   *   the index of the geometry
   * @return
   *   true if this edge is a boundary and part of a shell
   */
  private def isShell(geomIndex: Int): Boolean = {
    if (geomIndex == 0) return aDim == OverlayLabel.DIM_BOUNDARY && !aIsHole
    bDim == OverlayLabel.DIM_BOUNDARY && !bIsHole
  }

  private def copyInfo(info: EdgeSourceInfo): Unit =
    if (info.getIndex == 0) {
      aDim = info.getDimension
      aIsHole = info.isHole
      aDepthDelta = info.getDepthDelta
    } else {
      bDim = info.getDimension
      bIsHole = info.isHole
      bDepthDelta = info.getDepthDelta
    }

  /**
   * Merges an edge into this edge, updating the topology info accordingly.
   *
   * @param edge
   */
  def merge(edge: Edge): Unit = {

    /**
     * Marks this as a shell edge if any contributing edge is a shell. Update hole status first,
     * since it depends on edge dim
     */
    aIsHole = Edge.isHoleMerged(0, this, edge)
    bIsHole = Edge.isHoleMerged(1, this, edge)
    if (edge.aDim > aDim) aDim = edge.aDim
    if (edge.bDim > bDim) bDim = edge.bDim
    val relDir     = relativeDirection(edge)
    val flipFactor =
      if (relDir) 1
      else -1
    aDepthDelta += flipFactor * edge.aDepthDelta
    bDepthDelta += flipFactor * edge.bDepthDelta
    /*
        if (aDepthDelta > 1) {
          Debug.println(this);
        }
     */
  }

  override def toString: String = {
    val ptsStr = Edge.toStringPts(pts)
    val aInfo  = Edge.infoString(0, aDim, aIsHole, aDepthDelta)
    val bInfo  = Edge.infoString(1, bDim, bIsHole, bDepthDelta)
    "Edge( " + ptsStr + " ) " + aInfo + "/" + bInfo
  }

  def toLineString: String = WKTWriter.toLineString(pts)
}
