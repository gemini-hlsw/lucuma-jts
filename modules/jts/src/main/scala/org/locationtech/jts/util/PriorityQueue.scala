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
package org.locationtech.jts.util

import java.util

import org.locationtech.jts.index.strtree.BoundablePair

/**
 * A priority queue over a set of {link Comparable} objects.
 *
 * @author
 *   Martin Davis
 */
class PriorityQueue() {

  /**
   * Creates a new empty priority queue
   */
  private val items              = new util.ArrayList[BoundablePair]
  // create space for sentinel
  items.add(null)
  private var vsize              = 0 // Number of elements in queue
//  private var items = null // The queue binary heap array
  /**
   * Insert into the priority queue. Duplicates are allowed.
   *
   * @param x
   *   the item to insert.
   */
  def add(x: BoundablePair): Any = { // increase the size of the items heap to create a hole for the new item
    items.add(null)
    // Insert item at end of heap and then re-establish ordering
    vsize += 1
    var hole = vsize
    // set the item as a sentinel at the base of the heap
    items.set(0, x)
    // move the item up from the hole position to its correct place
    while (x.compareTo(items.get(hole / 2)) < 0) {
      items.set(hole, items.get(hole / 2))
      hole /= 2
    }
    // insert the new item in the correct place
    items.set(hole, x)
  }

  /**
   * Test if the priority queue is logically empty.
   *
   * return true if empty, false otherwise.
   */
  def isEmpty: Boolean = vsize == 0

  /**
   * Returns size.
   *
   * return current size.
   */
  def size(): Int = vsize

  /**
   * Make the priority queue logically empty.
   */
  def clear(): Unit = {
    vsize = 0
    items.clear()
  }

  /**
   * Remove the smallest item from the priority queue.
   *
   * return the smallest item, or null if empty
   */
  def poll: BoundablePair = {
    if (isEmpty) return null
    val minItem = items.get(1)
    items.set(1, items.get(vsize))
    vsize -= 1
    reorder(1)
    minItem
  }

  def peek: BoundablePair = {

    if (isEmpty) return null
    val minItem = items.get(1)
    minItem
  }

  /**
   * Internal method to percolate down in the heap.
   *
   * @param hole
   *   the index at which the percolate begins.
   */
  private def reorder(holeArg: Int): Unit = {
    var hole  = holeArg
    var child = 0
    val tmp   = items.get(hole)
    while (hole * 2 <= vsize) {
      child = hole * 2
      if (
        child != vsize && items
          .get(child + 1)
          .asInstanceOf[Comparable[Any]]
          .compareTo(items.get(child)) < 0
      ) {
        child += 1
      }
      if (items.get(child).asInstanceOf[Comparable[Any]].compareTo(tmp) < 0)
        items.set(hole, items.get(child))
      else {
        //break //todo: break is not supported
        hole = vsize // break the loop
      }
      hole = child
    }
    items.set(hole, tmp)
    ()
  }
}
