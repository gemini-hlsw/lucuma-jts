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

import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.LineString

import java.util

/**
 * Determines the boundary points of a linear geometry, using a {link BoundaryNodeRule}.
 *
 * @author
 *   mdavis
 */
class LinearBoundary(lines: util.List[LineString], bnRule: BoundaryNodeRule) {

  // assert: dim(geom) == 1
  private val boundaryNodeRule: BoundaryNodeRule              = bnRule
  private val vertexDegree: util.Map[Coordinate, Integer]     =
    LinearBoundary.computeBoundaryPoints(lines)
  private val hasBoundaryPoints: Boolean                      = checkBoundary(vertexDegree)

  private def checkBoundary(vertexDegree: util.Map[Coordinate, Integer]): Boolean = {
    val it = vertexDegree.values.iterator
    while (it.hasNext) {
      val degree = it.next().intValue
      if (boundaryNodeRule.isInBoundary(degree)) {
        return true
      }
    }
    false
  }

  def hasBoundary: Boolean = hasBoundaryPoints

  def isBoundary(pt: Coordinate): Boolean = {
    if (!vertexDegree.containsKey(pt))
      return false
    val degree = vertexDegree.get(pt).intValue
    boundaryNodeRule.isInBoundary(degree)
  }

}

object LinearBoundary {

  private def computeBoundaryPoints(
    lines: util.List[LineString]
  ): util.Map[Coordinate, Integer] = {
    val vertexDegree = new util.HashMap[Coordinate, Integer]
    val it           = lines.iterator
    while (it.hasNext) {
      val line = it.next()
      if (!line.isEmpty) {
        addEndpoint(line.getCoordinateN(0), vertexDegree)
        addEndpoint(line.getCoordinateN(line.getNumPoints - 1), vertexDegree)
      }
    }
    vertexDegree
  }

  private def addEndpoint(p: Coordinate, degree: util.Map[Coordinate, Integer]): Unit = {
    var dim = 0
    if (degree.containsKey(p)) {
      dim = degree.get(p).intValue
    }
    dim += 1
    degree.put(p, Integer.valueOf(dim))
    ()
  }

}
