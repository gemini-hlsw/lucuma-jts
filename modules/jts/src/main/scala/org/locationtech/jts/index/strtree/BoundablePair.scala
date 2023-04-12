// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 */ /*
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
package org.locationtech.jts.index.strtree

import org.locationtech.jts.geom.Envelope

import java.util.PriorityQueue

/**
 * A pair of {link Boundable}s, whose leaf items support a distance metric between them. Used to
 * compute the distance between the members, and to expand a member relative to the other in order
 * to produce new branches of the Branch-and-Bound evaluation tree. Provides an ordering based on
 * the distance between the members, which allows building a priority queue by minimum distance.
 *
 * @author
 *   Martin Davis
 */
object BoundablePair {
  def isComposite(item: Any): Boolean = item.isInstanceOf[AbstractNode]

  private def area(b: Boundable) = b.getBounds.asInstanceOf[Envelope].getArea
}

class BoundablePair(
  var boundable1:   Boundable,
  var boundable2:   Boundable,
  var itemDistance: ItemDistance
) extends Comparable[BoundablePair] {
  private val vdistance = distance

  /**
   * Gets one of the member {link Boundable}s in the pair (indexed by [0, 1]).
   *
   * @param i
   *   the index of the member to return (0 or 1) return the chosen member
   */
  def getBoundable(i: Int): Boundable = {
    if (i == 0) return boundable1
    boundable2
  }

  /**
   * Computes the maximum distance between any two items in the pair of nodes.
   *
   * return the maximum distance between items in the pair
   */
  def maximumDistance: Double = EnvelopeDistance.maximumDistance(
    boundable1.getBounds.asInstanceOf[Envelope],
    boundable2.getBounds.asInstanceOf[Envelope]
  )

  /**
   * Computes the distance between the {link Boundable}s in this pair. The boundables are either
   * composites or leaves. If either is composite, the distance is computed as the minimum distance
   * between the bounds. If both are leaves, the distance is computed by {link
   * #itemDistance(ItemBoundable, ItemBoundable)}.
   *
   * return
   */
  private def distance: Double = { // if items, compute exact distance
    if (isLeaves)
      return itemDistance.distance(boundable1.asInstanceOf[ItemBoundable],
                                   boundable2.asInstanceOf[ItemBoundable]
      )
    // otherwise compute distance between bounds of boundables
    boundable1.getBounds
      .asInstanceOf[Envelope]
      .distance(boundable2.getBounds.asInstanceOf[Envelope])
  }

  /**
   * Gets the minimum possible distance between the Boundables in this pair. If the members are both
   * items, this will be the exact distance between them. Otherwise, this distance will be a lower
   * bound on the distances between the items in the members.
   *
   * return the exact or lower bound distance for this pair
   */
  def getDistance: Double = vdistance

  /**
   * Compares two pairs based on their minimum distances
   */
  override def compareTo(nd: BoundablePair): Int = {
    if (vdistance < nd.vdistance) return -1
    if (vdistance > nd.vdistance) return 1
    0
  }

  /**
   * Tests if both elements of the pair are leaf nodes
   *
   * return true if both pair elements are leaf nodes
   */
  def isLeaves: Boolean =
    !(BoundablePair.isComposite(boundable1) || BoundablePair.isComposite(boundable2))

  /**
   * For a pair which is not a leaf (i.e. has at least one composite boundable) computes a list of
   * new pairs from the expansion of the larger boundable with distance less than minDistance and
   * adds them to a priority queue. <p> Note that expanded pairs may contain the same item/node on
   * both sides. This must be allowed to support distance functions which have non-zero distances
   * between the item and itself (non-zero reflexive distance).
   *
   * @param priQ
   *   the priority queue to add the new pairs to
   * @param minDistance
   *   the limit on the distance between added pairs
   */
  def expandToQueue(priQ: PriorityQueue[BoundablePair], minDistance: Double): Unit = {
    val isComp1 = BoundablePair.isComposite(boundable1)
    val isComp2 = BoundablePair.isComposite(boundable2)

    /**
     * HEURISTIC: If both boundable are composite, choose the one with largest area to expand.
     * Otherwise, simply expand whichever is composite.
     */
    if (isComp1 && isComp2) if (BoundablePair.area(boundable1) > BoundablePair.area(boundable2)) {
      expand(boundable1, boundable2, false, priQ, minDistance)
      return
    } else {
      expand(boundable2, boundable1, true, priQ, minDistance)
      return
    }
    else if (isComp1) {
      expand(boundable1, boundable2, false, priQ, minDistance)
      return
    } else if (isComp2) {
      expand(boundable2, boundable1, true, priQ, minDistance)
      return
    }
    throw new IllegalArgumentException("neither boundable is composite")
  }

  private def expand(
    bndComposite: Boundable,
    bndOther:     Boundable,
    isFlipped:    Boolean,
    priQ:         PriorityQueue[BoundablePair],
    minDistance:  Double
  ): Unit = {
    val children = bndComposite.asInstanceOf[AbstractNode].getChildBoundables
    val i        = children.iterator
    while (i.hasNext) {
      val child             = i.next
      var bp: BoundablePair = null
      if (isFlipped) bp = new BoundablePair(bndOther, child, itemDistance)
      else bp = new BoundablePair(child, bndOther, itemDistance)
      // only add to queue if this pair might contain the closest points
      // MD - it's actually faster to construct the object rather than called distance(child, bndOther)!
      if (bp.getDistance < minDistance) priQ.add(bp)
    }
  }
}
