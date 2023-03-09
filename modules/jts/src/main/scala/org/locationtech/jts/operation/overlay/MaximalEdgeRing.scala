// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.overlay

import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geomgraph.DirectedEdge
import org.locationtech.jts.geomgraph.DirectedEdgeStar
import org.locationtech.jts.geomgraph.EdgeRing

import java.util

/**
 * A ring of {link DirectedEdge}s which may contain nodes of degree &gt; 2. A
 * <tt>MaximalEdgeRing</tt> may represent two different spatial entities: <ul> <li>a single polygon
 * possibly containing inversions (if the ring is oriented CW) <li>a single hole possibly containing
 * exversions (if the ring is oriented CCW) </ul> If the MaximalEdgeRing represents a polygon, the
 * interior of the polygon is strongly connected. <p> These are the form of rings used to define
 * polygons under some spatial data models. However, under the OGC SFS model, {link
 * MinimalEdgeRing}s are required. A MaximalEdgeRing can be converted to a list of MinimalEdgeRings
 * using the {link #buildMinimalRings() } method.
 *
 * @version 1.7
 * @see
 *   org.locationtech.jts.operation.overlay.MinimalEdgeRing
 */
class MaximalEdgeRing(override val start: DirectedEdge, val geometryFactoryArg: GeometryFactory)
    extends EdgeRing(start, geometryFactoryArg) {
  override def getNext(de: DirectedEdge): DirectedEdge = de.getNext

  override def setEdgeRing(de: DirectedEdge, er: EdgeRing): Unit = de.setEdgeRing(er)

  /**
   * For all nodes in this EdgeRing, link the DirectedEdges at the node to form minimalEdgeRings
   */
  def linkDirectedEdgesForMinimalEdgeRings(): Unit = {
    var de = startDe
    while ({
      {
        val node = de.getNode
        node.getEdges.asInstanceOf[DirectedEdgeStar].linkMinimalDirectedEdges(this)
        de = de.getNext
      }; de != startDe
    }) ()
  }

  def buildMinimalRings: util.ArrayList[MinimalEdgeRing] = {
    val minEdgeRings = new util.ArrayList[MinimalEdgeRing]
    var de           = startDe
    while ({
      {
        if (de.getMinEdgeRing == null) {
          val minEr = new MinimalEdgeRing(de, geometryFactory)
          minEdgeRings.add(minEr)
        }
        de = de.getNext
      }; de != startDe
    }) ()
    minEdgeRings
  }
}
