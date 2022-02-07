// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.union

import java.util
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.util.GeometryCombiner

/**
 * Experimental code to union MultiPolygons with processing limited to the elements which actually
 * interact.
 *
 * Not currently used, since it doesn't seem to offer much of a performance advantage.
 *
 * @author
 *   mbdavis
 */
object UnionInteracting {
  def union(g0: Geometry, g1: Geometry): Geometry = {
    val uue = new UnionInteracting(g0, g1)
    uue.union
  }
}

class UnionInteracting(var g0: Geometry, var g1: Geometry) {
  private val geomFactory = g0.getFactory
  private val interacts0  = new Array[Boolean](g0.getNumGeometries)
  private val interacts1  = new Array[Boolean](g1.getNumGeometries)

  def union: Geometry = {
    computeInteracting()
    // check for all interacting or none interacting!
    val int0         = extractElements(g0, interacts0, true)
    val int1         = extractElements(g1, interacts1, true)
    //		System.out.println(int0);
    //		System.out.println(int1);
    if (int0.isEmpty || int1.isEmpty) {
      System.out.println("found empty!")
      //			computeInteracting();
    }
    //		if (! int0.isValid()) {
    // System.out.println(int0);
    // throw new RuntimeException("invalid geom!");
    //		}
    val union        = int0.union(int1)
    // Geometry union = bufferUnion(int0, int1);
    val disjoint0    = extractElements(g0, interacts0, false)
    val disjoint1    = extractElements(g1, interacts1, false)
    val overallUnion = GeometryCombiner.combine(union, disjoint0, disjoint1)
    overallUnion
  }

  def bufferUnion(g0: Geometry, g1: Geometry) = {
    val factory  = g0.getFactory
    val gColl    = factory.createGeometryCollection(Array[Geometry](g0, g1))
    val unionAll = gColl.buffer(0.0)
    unionAll
  }

  private def computeInteracting(): Unit = {
    var i = 0
    while (i < g0.getNumGeometries) {
      val elem = g0.getGeometryN(i)
      interacts0(i) = computeInteracting(elem)
      i += 1
    }
  }

  private def computeInteracting(elem0: Geometry) = {
    var interactsWithAny = false
    var i                = 0
    while (i < g1.getNumGeometries) {
      val elem1     = g1.getGeometryN(i)
      val interacts = elem1.getEnvelopeInternal.intersects(elem0.getEnvelopeInternal)
      if (interacts) interacts1(i) = true
      if (interacts) interactsWithAny = true
      i += 1
    }
    interactsWithAny
  }

  private def extractElements(geom: Geometry, interacts: Array[Boolean], isInteracting: Boolean) = {
    val extractedGeoms = new util.ArrayList[Geometry]
    var i              = 0
    while (i < geom.getNumGeometries) {
      val elem = geom.getGeometryN(i)
      if (interacts(i) == isInteracting) extractedGeoms.add(elem)
      i += 1
    }
    geomFactory.buildGeometry(extractedGeoms)
  }
}
