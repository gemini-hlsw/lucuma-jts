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
 */ /*
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
package org.locationtech.jts.index.intervaltree

import java.util.Comparator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.index.ItemVisitor
//import org.locationtech.jts.io.WKTWriter

object IntervalRTreeNode {

  class NodeComparator extends Comparator[IntervalRTreeNode] {
    override def compare(n1: IntervalRTreeNode, n2: IntervalRTreeNode): Int = {
      val mid1 = (n1.min + n1.max) / 2
      val mid2 = (n2.min + n2.max) / 2
      if (mid1 < mid2) return -1
      if (mid1 > mid2) return 1
      0
    }
  }

}

abstract class IntervalRTreeNode(
  protected[intervaltree] var min: Double = java.lang.Double.POSITIVE_INFINITY,
  protected[intervaltree] var max: Double = java.lang.Double.NEGATIVE_INFINITY
) {

  def getMin: Double = min

  def getMax: Double = max

  def query(queryMin: Double, queryMax: Double, visitor: ItemVisitor): Unit

  protected def intersects(queryMin: Double, queryMax: Double): Boolean = {
    if (min > queryMax || max < queryMin) return false
    true
  }

//  override def toString: String = WKTWriter.toLineString(new Coordinate(min, 0), new Coordinate(max, 0))
  override def toString: String = (new Coordinate(min, 0), new Coordinate(max, 0)).toString()
}
