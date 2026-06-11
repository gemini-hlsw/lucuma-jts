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

import java.util

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry

class NodeSections(private val nodePt: Coordinate) {

  private val sections: util.List[NodeSection] = new util.ArrayList[NodeSection]()

  def getCoordinate: Coordinate = nodePt

  def addNodeSection(e: NodeSection): Unit =
    sections.add(e)

  def hasInteractionAB: Boolean = {
    var isA = false
    var isB = false
    val it  = sections.iterator
    while (it.hasNext) {
      val ns = it.next()
      if (ns.isA)
        isA = true
      else
        isB = true
      if (isA && isB)
        return true
    }
    false
  }

  def getPolygonal(isA: Boolean): Geometry = {
    val it = sections.iterator
    while (it.hasNext) {
      val ns = it.next()
      if (ns.isA == isA) {
        val poly = ns.getPolygonal
        if (poly != null)
          return poly
      }
    }
    null
  }

  def createNode(): RelateNode = {
    prepareSections()

    val node = new RelateNode(nodePt)
    var i    = 0
    while (i < sections.size) {
      val ns = sections.get(i)
      // -- if there multiple polygon sections incident at node convert them to maximal-ring structure
      if (ns.isArea && NodeSections.hasMultiplePolygonSections(sections, i)) {
        val polySections = NodeSections.collectPolygonSections(sections, i)
        val nsConvert    = PolygonNodeConverter.convert(polySections)
        node.addEdges(nsConvert)
        i += polySections.size
      } else {
        // -- the most common case is a line or a single polygon ring section
        node.addEdges(ns)
        i += 1
      }
    }
    node
  }

  /**
   * Sorts the sections so that: <ul> <li>lines are before areas <li>edges from the same polygon are
   * contiguous </ul>
   */
  private def prepareSections(): Unit =
    util.Collections.sort(sections)
    // TODO: remove duplicate sections

}

object NodeSections {

  private def hasMultiplePolygonSections(sections: util.List[NodeSection], i: Int): Boolean = {
    // -- if last section can only be one
    if (i >= sections.size - 1)
      return false
    // -- check if there are at least two sections for same polygon
    val ns     = sections.get(i)
    val nsNext = sections.get(i + 1)
    ns.isSamePolygon(nsNext)
  }

  private def collectPolygonSections(
    sections: util.List[NodeSection],
    i:        Int
  ): util.List[NodeSection] = {
    val polySections = new util.ArrayList[NodeSection]()
    // -- note ids are only unique to a geometry
    val polySection  = sections.get(i)
    var j            = i
    while (j < sections.size && polySection.isSamePolygon(sections.get(j))) {
      polySections.add(sections.get(j))
      j += 1
    }
    polySections
  }

}
