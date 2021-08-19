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
package org.locationtech.jts.geom

import java.util
import java.util.NoSuchElementException

/**
 * Iterates over all {link Geometry}s in a {link Geometry}, (which may be either a collection or an
 * atomic geometry). The iteration sequence follows a pre-order, depth-first traversal of the
 * structure of the <code>GeometryCollection</code> (which may be nested). The original
 * <code>Geometry</code> object is returned as well (as the first object), as are all
 * sub-collections and atomic elements. It is simple to ignore the intermediate
 * <code>GeometryCollection</code> objects if they are not needed.
 *
 * @version 1.7
 */
object GeometryCollectionIterator {
  private def isAtomic(geom: Geometry) = !geom.isInstanceOf[GeometryCollection]
}

class GeometryCollectionIterator(
  /**
   * The <code>Geometry</code> being iterated over.
   */
  var parent: Geometry
)

// /**
//  * Constructs an iterator over the given <code>Geometry</code>.
//  *
//  * @param  parent the geometry over which to iterate; also, the first
//  *                element returned by the iterator.
//  */
    extends util.Iterator[Geometry] {

  /**
   * Indicates whether or not the first element (the root <code>GeometryCollection</code>) has been
   * returned.
   */
  private var atStart = false

  /**
   * The number of <code>Geometry</code>s in the the <code>GeometryCollection</code>.
   */
  private var max = 0

  /**
   * The index of the <code>Geometry</code> that will be returned when <code>next</code> is called.
   */
  private var index = 0

  /**
   * The iterator over a nested <code>Geometry</code>, or <code>null</code> if this
   * <code>GeometryCollectionIterator</code> is not currently iterating over a nested
   * <code>GeometryCollection</code>.
   */
  private var subcollectionIterator: util.Iterator[Geometry] = null
  atStart = true
  index = 0
  max = parent.getNumGeometries

  /**
   * Tests whether any geometry elements remain to be returned.
   *
   * return true if more geometry elements remain
   */
  override def hasNext: Boolean = {
    if (atStart) return true
    if (subcollectionIterator != null) {
      if (subcollectionIterator.hasNext) return true
      subcollectionIterator = null
    }
    if (index >= max) return false
    true
  }

  /**
   * Gets the next geometry in the iteration sequence.
   *
   * return the next geometry in the iteration
   */
  override def next: Geometry = { // the parent GeometryCollection is the first object returned
    if (atStart) {
      atStart = false
      if (GeometryCollectionIterator.isAtomic(parent)) {
        index += 1
      }
      return parent
    }
    if (subcollectionIterator != null)
      if (subcollectionIterator.hasNext) return subcollectionIterator.next
      else subcollectionIterator = null
    if (index >= max) throw new NoSuchElementException
    val obj = parent.getGeometryN {
      index += 1; index - 1
    }
    if (obj.isInstanceOf[GeometryCollection]) {
      subcollectionIterator = new GeometryCollectionIterator(obj.asInstanceOf[GeometryCollection])
      // there will always be at least one element in the sub-collection
      return subcollectionIterator.next
    }
    obj
  }

  /**
   * Removal is not supported.
   *
   * throws UnsupportedOperationException This method is not implemented.
   */
  override def remove(): Unit = throw new UnsupportedOperationException(getClass.getName)
}
