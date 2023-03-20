// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.util.Assert

import java.util
import scala.jdk.CollectionConverters._

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
 * Performs merging on the noded edges of the input geometries. Merging takes place on edges which
 * are coincident (i.e. have the same coordinate list, modulo direction). The following situations
 * can occur: <ul> <li>Coincident edges from different input geometries have their labels combined
 * <li>Coincident edges from the same area geometry indicate a topology collapse. In this case the
 * topology locations are "summed" to provide a final assignment of side location <li>Coincident
 * edges from the same linear geometry can simply be merged using the same ON location </ul>
 *
 * The merging attempts to preserve the direction of linear edges if possible (which is the case if
 * there is no other coincident edge, or if all coincident edges have the same direction). This
 * ensures that the overlay output line direction will be as consistent as possible with input
 * lines.
 *
 * @author
 *   mdavis
 */
object EdgeMerger {
  def merge(edges: util.List[Edge]): util.List[Edge] = {
    val merger = new EdgeMerger(edges)
    merger.merge
  }
}

class EdgeMerger(var edges: util.Collection[Edge]) {
  private val edgeMap = new util.HashMap[EdgeKey, Edge]

  def merge: util.ArrayList[Edge] = {
    for (edge <- edges.asScala) {
      val edgeKey  = EdgeKey.create(edge)
      val baseEdge = edgeMap.get(edgeKey)
      if (baseEdge == null) { // this is the first (and maybe only) edge for this line
        edgeMap.put(edgeKey, edge)
        // Debug.println("edge added: " + edge);
        // Debug.println(edge.toLineString());
      } else { // found an existing edge
        // Assert: edges are identical (up to direction)
        // this is a fast (but incomplete) sanity check
        Assert.isTrue(baseEdge.size == edge.size,
                      "Merge of edges of different sizes - probable noding error."
        )
        baseEdge.merge(edge)
        // Debug.println("edge merged: " + existing);
      }
    }
    new util.ArrayList[Edge](edgeMap.values)
  }
}
