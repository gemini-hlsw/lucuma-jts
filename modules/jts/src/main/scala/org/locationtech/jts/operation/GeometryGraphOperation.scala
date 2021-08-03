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
package org.locationtech.jts.operation

import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geomgraph.GeometryGraph

/**
 * The base class for operations that require {link GeometryGraph}s.
 *
 * @version 1.7
 */
class GeometryGraphOperation(g0: Geometry, g1: Geometry, boundaryNodeRule: BoundaryNodeRule) {
  final protected val li: LineIntersector            = new RobustLineIntersector
  protected var resultPrecisionModel: PrecisionModel = null

  /**
   * The operation args into an array so they can be accessed by index
   */
  protected var arg: Array[GeometryGraph] = null // the arg(s) of the operation
  // use the most precise model for the result
  if (g0.getPrecisionModel.compareTo(g1.getPrecisionModel) >= 0)
    setComputationPrecision(g0.getPrecisionModel)
  else setComputationPrecision(g1.getPrecisionModel)

  arg = new Array[GeometryGraph](2)
  arg(0) = new GeometryGraph(0, g0, boundaryNodeRule)
  arg(1) = new GeometryGraph(1, g1, boundaryNodeRule)

  def this(g0: Geometry, g1: Geometry) = {
    this(g0,
         g1,
         BoundaryNodeRule.OGC_SFS_BOUNDARY_RULE
    ) //         BoundaryNodeRule.ENDPOINT_BOUNDARY_RULE
  }

  def this(g0: Geometry) = {
    this(g0, null, null)
    setComputationPrecision(g0.getPrecisionModel)
    arg = new Array[GeometryGraph](1)
    arg(0) = new GeometryGraph(0, g0)
  }

  def getArgGeometry(i: Int): Geometry = arg(i).getGeometry

  protected def setComputationPrecision(pm: PrecisionModel): Unit = {
    resultPrecisionModel = pm
    li.setPrecisionModel(resultPrecisionModel)
  }
}
