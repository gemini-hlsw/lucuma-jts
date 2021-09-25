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

import java.util
import java.util.Collections

//import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.index.ItemVisitor
//import org.locationtech.jts.io.WKTWriter

/**
 * A static index on a set of 1-dimensional intervals, using an R-Tree packed based on the order of
 * the interval midpoints. It supports range searching, where the range is an interval of the real
 * line (which may be a single point). A common use is to index 1-dimensional intervals which are
 * the projection of 2-D objects onto an axis of the coordinate system. <p> This index structure is
 * <i>static</i>
 *   - items cannot be added or removed once the first query has been made. The advantage of this
 *     characteristic is that the index performance can be optimized based on a fixed set of items.
 *
 * @author
 *   Martin Davis
 */
class SortedPackedIntervalRTree() {
  private val leaves = new util.ArrayList[IntervalRTreeNode]

  /**
   * If root is null that indicates that the tree has not yet been built, OR nothing has been added
   * to the tree. In both cases, the tree is still open for insertions.
   */
  private var root: IntervalRTreeNode = null

  /**
   * Adds an item to the index which is associated with the given interval
   *
   * @param min
   *   the lower bound of the item interval
   * @param max
   *   the upper bound of the item interval
   * @param item
   *   the item to insert throws IllegalStateException if the index has already been queried
   */
  def insert(min: Double, max: Double, item: Any) = {
    if (root != null)
      throw new IllegalStateException("Index cannot be added to once it has been queried")
    leaves.add(new IntervalRTreeLeafNode(min, max, item))
  }

  private def init(): Unit                        = { // already built
    if (root != null) return

    /**
     * if leaves is empty then nothing has been inserted. In this case it is safe to leave the tree
     * in an open state
     */
    if (leaves.size == 0) return
    buildRoot()
  }

  private def buildRoot(): Unit                   = {
    if (root != null) return
    root = buildTree
  }

  private def buildTree: IntervalRTreeNode        = { // sort the leaf nodes
    Collections.sort(leaves, new IntervalRTreeNode.NodeComparator)
    // now group nodes into blocks of two and build tree up recursively
    var src                                     = leaves
    var temp: util.ArrayList[IntervalRTreeNode] = null
    var dest                                    = new util.ArrayList[IntervalRTreeNode]
    while (true) {
      buildLevel(src, dest)
      if (dest.size == 1) return dest.get(0).asInstanceOf[IntervalRTreeNode]
      temp = src
      src = dest
      dest = temp
    }
    return null
  }

  private var level = 0

  private def buildLevel(src: util.List[IntervalRTreeNode], dest: util.List[IntervalRTreeNode]) = {
    level += 1
    dest.clear()
    var i = 0
    while (i < src.size) {
      val n1 = src.get(i)
      val n2 =
        if (i + 1 < src.size) src.get(i)
        else null
      if (n2 == null) dest.add(n1)
      else {
        val node = new IntervalRTreeBranchNode(src.get(i), src.get(i + 1))
        //        printNode(node);
        //				System.out.println(node);
        dest.add(node)
      }
      i += 2
    }
  }

//  private def printNode(node: IntervalRTreeNode) = System.out.println(WKTWriter.toLineString(new Coordinate(node.min, level), new Coordinate(node.max, level)))

  /**
   * Search for intervals in the index which intersect the given closed interval and apply the
   * visitor to them.
   *
   * @param min
   *   the lower bound of the query interval
   * @param max
   *   the upper bound of the query interval
   * @param visitor
   *   the visitor to pass any matched items to
   */
  def query(min: Double, max: Double, visitor: ItemVisitor): Unit = {
    init()
    // if root is null tree must be empty
    if (root == null) return
    root.query(min, max, visitor)
  }
}
