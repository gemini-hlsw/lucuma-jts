// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Point

import java.util
import scala.jdk.CollectionConverters.*

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
 * Extracts Point resultants from an overlay graph created by an Intersection operation between
 * non-Point inputs. Points may be created during intersection if lines or areas touch one another
 * at single points. Intersection is the only overlay operation which can result in Points from
 * non-Point inputs. <p> Overlay operations where one or more inputs are Points are handled via a
 * different code path.
 *
 * @author
 *   Martin Davis
 * @see
 *   OverlayPoints
 */
class IntersectionPointBuilder(var graph: OverlayGraph, var geometryFactory: GeometryFactory) {
  private val points = new util.ArrayList[Point]

  /**
   * Controls whether lines created by area topology collapses to participate in the result
   * computation. True provides the original JTS semantics.
   */
  private var isAllowCollapseLines = !OverlayNG.STRICT_MODE_DEFAULT

  def setStrictMode(isStrictMode: Boolean): Unit =
    isAllowCollapseLines = !isStrictMode

  def getPoints: util.List[Point] = {
    addResultPoints()
    points
  }

  private def addResultPoints(): Unit =
    for (nodeEdge <- graph.getNodeEdges.asScala)
      if (isResultPoint(nodeEdge)) {
        val pt = geometryFactory.createPoint(nodeEdge.getCoordinate.copy)
        points.add(pt)
      }

  /**
   * Tests if a node is a result point. This is the case if the node is incident on edges from both
   * inputs, and none of the edges are themselves in the result.
   *
   * @param nodeEdge
   *   an edge originating at the node
   * @return
   *   true if this node is a result point
   */
  private def isResultPoint(nodeEdge: OverlayEdge): Boolean = {
    var isEdgeOfA    = false
    var isEdgeOfB    = false
    var edge         = nodeEdge
    while ({
      if (edge.isInResult) return false
      val label = edge.getLabel
      isEdgeOfA |= isEdgeOf(label, 0)
      isEdgeOfB |= isEdgeOf(label, 1)
      edge = edge.oNext.asInstanceOf[OverlayEdge]
      edge ne nodeEdge
    }) ()
    val isNodeInBoth = isEdgeOfA && isEdgeOfB
    isNodeInBoth
  }

  private def isEdgeOf(label: OverlayLabel, i: Int): Boolean = {
    if (!isAllowCollapseLines && label.isBoundaryCollapse) return false
    label.isBoundary(i) || label.isLine(i)
  }
}
