// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
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

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Dimension

import java.util

/**
 * Converts the node sections at a polygon node where a shell and one or more holes touch, or two or
 * more holes touch. This converts the node topological structure from the OGC "touching-rings" (AKA
 * "minimal-ring") model to the equivalent "self-touch" (AKA "inverted/exverted ring" or "maximal
 * ring") model. In the "self-touch" model the converted NodeSection corners enclose areas which all
 * lies inside the polygon (i.e. they does not enclose hole edges). This allows {link RelateNode} to
 * use simple area-additive semantics for adding edges and propagating edge locations. <p> The input
 * node sections are assumed to have canonical orientation (CW shells and CCW holes). The
 * arrangement of shells and holes must be topologically valid. Specifically, the node sections must
 * not cross or be collinear. <p> This supports multiple shell-shell touches (including ones
 * containing holes), and hole-hole touches, This generalizes the relate algorithm to support both
 * the OGC model and the self-touch model.
 *
 * @author
 *   Martin Davis
 * @see
 *   RelateNode
 */
object PolygonNodeConverter {

  /**
   * Converts a list of sections of valid polygon rings to have "self-touching" structure. There are
   * the same number of output sections as input ones.
   *
   * @param polySections
   *   the original sections return the converted sections
   */
  def convert(polySections: util.List[NodeSection]): util.List[NodeSection] = {
    util.Collections.sort(polySections, new NodeSection.EdgeAngleComparator())

    // TODO: move uniquing up to caller
    val sections = extractUnique(polySections)
    if (sections.size == 1)
      return sections

    // -- find shell section index
    val shellIndex        = findShell(sections)
    if (shellIndex < 0) {
      return convertHoles(sections)
    }
    // -- at least one shell is present.  Handle multiple ones if present
    val convertedSections = new util.ArrayList[NodeSection]()
    var nextShellIndex    = shellIndex
    var isDone            = false
    while (!isDone) {
      nextShellIndex = convertShellAndHoles(sections, nextShellIndex, convertedSections)
      isDone = nextShellIndex == shellIndex
    }

    convertedSections
  }

  private def convertShellAndHoles(
    sections:          util.List[NodeSection],
    shellIndex:        Int,
    convertedSections: util.List[NodeSection]
  ): Int = {
    val shellSection             = sections.get(shellIndex)
    var inVertex                 = shellSection.getVertex(0)
    var i                        = next(sections, shellIndex)
    var holeSection: NodeSection = null
    while (!sections.get(i).isShell) {
      holeSection = sections.get(i)
      // Assert: holeSection.isShell = false
      val outVertex = holeSection.getVertex(1)
      val ns        = createSection(shellSection, inVertex, outVertex)
      convertedSections.add(ns)

      inVertex = holeSection.getVertex(0)
      i = next(sections, i)
    }
    // -- create final section for corner from last hole to shell
    val outVertex                = shellSection.getVertex(1)
    val ns                       = createSection(shellSection, inVertex, outVertex)
    convertedSections.add(ns)
    i
  }

  private def convertHoles(sections: util.List[NodeSection]): util.List[NodeSection] = {
    val convertedSections = new util.ArrayList[NodeSection]()
    val copySection       = sections.get(0)
    var i                 = 0
    while (i < sections.size) {
      val inext     = next(sections, i)
      val inVertex  = sections.get(i).getVertex(0)
      val outVertex = sections.get(inext).getVertex(1)
      val ns        = createSection(copySection, inVertex, outVertex)
      convertedSections.add(ns)
      i += 1
    }
    convertedSections
  }

  private def createSection(ns: NodeSection, v0: Coordinate, v1: Coordinate): NodeSection =
    new NodeSection(ns.isA,
                    Dimension.A,
                    ns.id,
                    0,
                    ns.getPolygonal,
                    ns.isNodeAtVertex,
                    v0,
                    ns.nodePt,
                    v1
    )

  private def extractUnique(sections: util.List[NodeSection]): util.List[NodeSection] = {
    val uniqueSections = new util.ArrayList[NodeSection]()
    var lastUnique     = sections.get(0)
    uniqueSections.add(lastUnique)
    val it             = sections.iterator
    while (it.hasNext) {
      val ns = it.next()
      if (0 != lastUnique.compareTo(ns)) {
        uniqueSections.add(ns)
        lastUnique = ns
      }
    }
    uniqueSections
  }

  private def next(ns: util.List[NodeSection], i: Int): Int = {
    var nxt = i + 1
    if (nxt >= ns.size)
      nxt = 0
    nxt
  }

  private def findShell(polySections: util.List[NodeSection]): Int = {
    var i = 0
    while (i < polySections.size) {
      if (polySections.get(i).isShell)
        return i
      i += 1
    }
    -1
  }
}
