// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ // Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.distance

import java.util
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryComponentFilter
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point
import org.locationtech.jts.index.strtree.STRtree

object FacetSequenceTreeBuilder { // 6 seems to be a good facet sequence size
  private val FACET_SEQUENCE_SIZE    = 6
  // Seems to be better to use a minimum node capacity
  private val STR_TREE_NODE_CAPACITY = 4

  def build(g: Geometry): STRtree = {
    val tree     = new STRtree(STR_TREE_NODE_CAPACITY)
    val sections = computeFacetSequences(g)
    val i        = sections.iterator
    while (i.hasNext) {
      val section = i.next
      tree.insert(section.getEnvelope, section)
    }
    tree.build()
    tree
  }

  /**
   * Creates facet sequences
   *
   * @param g
   *   return List<GeometryFacetSequence>
   */
  private def computeFacetSequences(g: Geometry): util.List[FacetSequence] = {
    val sections = new util.ArrayList[FacetSequence]
    g.applyF(new GeometryComponentFilter() {
      override def filter(geom: Geometry): Unit = {
        var seq: CoordinateSequence = null
        if (geom.isInstanceOf[LineString]) {
          seq = geom.asInstanceOf[LineString].getCoordinateSequence
          addFacetSequences(geom, seq, sections)
        } else if (geom.isInstanceOf[Point]) {
          seq = geom.asInstanceOf[Point].getCoordinateSequence
          addFacetSequences(geom, seq, sections)
        }
      }
    })
    sections
  }

  private def addFacetSequences(
    geom:     Geometry,
    pts:      CoordinateSequence,
    sections: util.List[FacetSequence]
  ): Unit = {
    var i    = 0
    val size = pts.size
    while (i <= size - 1) {
      var end  = i + FACET_SEQUENCE_SIZE + 1
      // if only one point remains after this section, include it in this
      // section
      if (end >= size - 1) end = size
      val sect = new FacetSequence(geom, pts, i, end)
      sections.add(sect)
      i = i + FACET_SEQUENCE_SIZE
    }
  }
}
