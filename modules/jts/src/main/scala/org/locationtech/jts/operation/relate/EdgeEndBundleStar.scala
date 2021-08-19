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
package org.locationtech.jts.operation.relate

import org.locationtech.jts.geom.IntersectionMatrix
import org.locationtech.jts.geomgraph.EdgeEnd
import org.locationtech.jts.geomgraph.EdgeEndStar

/**
 * An ordered list of {link EdgeEndBundle}s around a {link RelateNode}. They are maintained in CCW
 * order (starting with the positive x-axis) around the node for efficient lookup and topology
 * building.
 *
 * @version 1.7
 */
class EdgeEndBundleStar()

/**
 * Creates a new empty EdgeEndBundleStar
 */ extends EdgeEndStar {

  /**
   * Insert a EdgeEnd in order in the list. If there is an existing EdgeStubBundle which is
   * parallel, the EdgeEnd is added to the bundle. Otherwise, a new EdgeEndBundle is created to
   * contain the EdgeEnd. <br>
   */
  override def insert(e: EdgeEnd): Unit = {
    var eb = edgeMap.get(e).orNull.asInstanceOf[EdgeEndBundle]
    if (eb == null) {
      eb = new EdgeEndBundle(e)
      insertEdgeEnd(e, eb)
    } else eb.insert(e)
    ()
  }

  /**
   * Update the IM with the contribution for the EdgeStubs around the node.
   */
  private[relate] def updateIM(im: IntersectionMatrix): Unit = {
    val it = iterator
    while (it.hasNext) {
      val esb = it.next.asInstanceOf[EdgeEndBundle]
      esb.updateIM(im)
    }
  }
}
