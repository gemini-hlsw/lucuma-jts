// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
package org.locationtech.jts.index.strtree

import java.io.Serializable
import java.util
import java.util.Collections
import java.util.Comparator

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.index.ItemVisitor
import org.locationtech.jts.index.SpatialIndex
import org.locationtech.jts.index.strtree.AbstractSTRtree.IntersectsOp
import org.locationtech.jts.util.Assert
import org.locationtech.jts.util.PriorityQueue

/**
 * A query-only R-tree created using the Sort-Tile-Recursive (STR) algorithm. For two-dimensional
 * spatial data. <P> The STR packed R-tree is simple to implement and maximizes space utilization;
 * that is, as many leaves as possible are filled to capacity. Overlap between nodes is far less
 * than in a basic R-tree. However, once the tree has been built (explicitly or on the first call to
 * #query), items may not be added or removed. <P> Described in: P. Rigaux, Michel Scholl and Agnes
 * Voisard. <i>Spatial Databases With Application To GIS</i>. Morgan Kaufmann, San Francisco, 2002.
 * <p> <b>Note that inserting items into a tree is not thread-safe.</b> Inserting performed on more
 * than one thread must be synchronized externally. <p> Querying a tree is thread-safe. The building
 * phase is done synchronously, and querying is stateless.
 *
 * @version 1.7
 */
@SerialVersionUID(259274702368956900L)
object STRtree {

  final class STRtreeNode(val level: Int) extends AbstractNode(level) {
    override protected def computeBounds: Envelope = {
      var bounds: Envelope = null
      val i                = getChildBoundables.iterator
      while (i.hasNext) {
        val childBoundable = i.next
        if (bounds == null) bounds = new Envelope(childBoundable.getBounds.asInstanceOf[Envelope])
        else bounds.expandToInclude(childBoundable.getBounds.asInstanceOf[Envelope])
      }
      bounds
    }
  }

  private val xComparator = new Comparator[Boundable]() {
    override def compare(o1: Boundable, o2: Boundable): Int =
      AbstractSTRtree.compareDoubles(centreX(o1.getBounds.asInstanceOf[Envelope]),
                                     centreX(o2.getBounds.asInstanceOf[Envelope])
      )
  }
  private val yComparator = new Comparator[Boundable]() {
    override def compare(o1: Boundable, o2: Boundable): Int =
      AbstractSTRtree.compareDoubles(centreY(o1.getBounds.asInstanceOf[Envelope]),
                                     centreY(o2.getBounds.asInstanceOf[Envelope])
      )
  }

  private def centreX(e: Envelope) = avg(e.getMinX, e.getMaxX)

  private def centreY(e: Envelope) = avg(e.getMinY, e.getMaxY)

  private def avg(a: Double, b: Double) = (a + b) / 2d

  private val intersectsOp          = new AbstractSTRtree.IntersectsOp() {
    override def intersects(aBounds: Any, bBounds: Any): Boolean =
      aBounds.asInstanceOf[Envelope].intersects(bBounds.asInstanceOf[Envelope])
  }
  private val DEFAULT_NODE_CAPACITY = 10

  private def getItems(kNearestNeighbors: PriorityQueue) = {

    /**
     * Iterate the K Nearest Neighbour Queue and retrieve the item from each BoundablePair in this
     * queue
     */
    val items = new Array[Any](kNearestNeighbors.size())
    var count = 0
    while (!kNearestNeighbors.isEmpty) {
      val bp = kNearestNeighbors.poll.asInstanceOf[BoundablePair]
      items(count) = bp.getBoundable(0).asInstanceOf[ItemBoundable].getItem
      count += 1
    }
    items
  }
}

@SerialVersionUID(259274702368956900L)
class STRtree(val nodeCapacityArg: Int)

/**
 * Constructs an STRtree with the given maximum number of child nodes that
 * a node may have.
 * <p>
 * The minimum recommended capacity setting is 4.
 */ extends AbstractSTRtree(nodeCapacityArg) with SpatialIndex[Any] with Serializable {

  /**
   * Creates the parent level for the given child level. First, orders the items by the x-values of
   * the midpoints, and groups them into vertical slices. For each slice, orders the items by the
   * y-values of the midpoints, and group them into runs of size M (the node capacity). For each
   * run, creates a new (parent) node.
   */
  override protected def createParentBoundables(
    childBoundables: util.List[Boundable],
    newLevel:        Int
  ): util.ArrayList[AbstractNode] = {
    Assert.isTrue(!childBoundables.isEmpty)
    val minLeafCount          = Math.ceil(childBoundables.size / getNodeCapacity.toDouble).toInt
    val sortedChildBoundables = new util.ArrayList[Boundable](childBoundables)
    Collections.sort(sortedChildBoundables, STRtree.xComparator)
    val vverticalSlices       =
      verticalSlices(sortedChildBoundables, Math.ceil(Math.sqrt(minLeafCount.toDouble)).toInt)
    createParentBoundablesFromVerticalSlices(vverticalSlices, newLevel)
  }

  private def createParentBoundablesFromVerticalSlices(
    verticalSlices: Array[util.List[Boundable]],
    newLevel:       Int
  ): util.ArrayList[AbstractNode] = {
    Assert.isTrue(verticalSlices.length > 0)
    val parentBoundables = new util.ArrayList[AbstractNode]
    var i                = 0
    while (i < verticalSlices.length) {
      parentBoundables.addAll(createParentBoundablesFromVerticalSlice(verticalSlices(i), newLevel))
      i += 1
    }
    parentBoundables
  }

  protected def createParentBoundablesFromVerticalSlice(
    childBoundables: util.List[Boundable],
    newLevel:        Int
  ): util.ArrayList[AbstractNode] =
    super.createParentBoundables(childBoundables, newLevel)

  /**
   * @param childBoundables
   *   Must be sorted by the x-value of the envelope midpoints
   */
  protected def verticalSlices(
    childBoundables: util.List[Boundable],
    sliceCount:      Int
  ): Array[util.List[Boundable]] = {
    val sliceCapacity = Math.ceil(childBoundables.size / sliceCount.toDouble).toInt
    val slices        = new Array[util.List[Boundable]](sliceCount)
    val i             = childBoundables.iterator
    var j             = 0
    while (j < sliceCount) {
      slices(j) = new util.ArrayList[Boundable]
      var boundablesAddedToSlice = 0
      while (i.hasNext && boundablesAddedToSlice < sliceCapacity) {
        val childBoundable = i.next
        slices(j).add(childBoundable)
        boundablesAddedToSlice += 1
      }
      j += 1
    }
    slices
  }

  /**
   * Constructs an STRtree with the default node capacity.
   */
  def this() =
    this(STRtree.DEFAULT_NODE_CAPACITY)

  override protected def createNode(level: Int) = new STRtree.STRtreeNode(level)

  override protected def getIntersectsOp: IntersectsOp = STRtree.intersectsOp

  /**
   * Inserts an item having the given bounds into the tree.
   */
  override def insert(itemEnv: Envelope, item: Any): Unit = {
    if (itemEnv.isNull) return
    super[AbstractSTRtree].insert(itemEnv, item)
    ()
  }

  /**
   * Returns items whose bounds intersect the given envelope.
   */
  override def query(
    searchEnv: Envelope
  ): util.List[Any] = // Yes this method does something. It specifies that the bounds is an
    // Envelope. super.query takes an Object, not an Envelope. [Jon Aquino 10/24/2003]
    super[AbstractSTRtree].query(searchEnv)

  override def query(searchEnv: Envelope, visitor: ItemVisitor): Unit =
    super[AbstractSTRtree].query(searchEnv, visitor)

  /**
   * Removes a single item from the tree.
   *
   * @param itemEnv
   *   the Envelope of the item to remove
   * @param item
   *   the item to remove return <code>true</code> if the item was found
   */
  override def remove(itemEnv: Envelope, item: Any): Unit = {
    super[AbstractSTRtree].remove(itemEnv, item)
    ()
  }

  /**
   * Returns the number of items in the tree.
   *
   * return the number of items in the tree
   */
  override def size: Int = super.size

  override def depth: Int = super.depth

  override protected def getComparator: Comparator[Boundable] = STRtree.yComparator

  /**
   * Finds the two nearest items in the tree, using {link ItemDistance} as the distance metric. A
   * Branch-and-Bound tree traversal algorithm is used to provide an efficient search. <p> If the
   * tree is empty, the return value is <code>null</code. If the tree contains only one item, the
   * return value is a pair containing that item. <b> If it is required to find only pairs of
   * distinct items, the {link ItemDistance} function must be <b>anti-reflexive</b>.
   *
   * @param itemDist
   *   a distance metric applicable to the items in this tree return the pair of the nearest items
   *   or <code>null</code> if the tree is empty
   */
  def nearestNeighbour(itemDist: ItemDistance): Array[Any] = {
    if (isEmpty) return null
    // if tree has only one item this will return null
    val bp = new BoundablePair(this.getRoot, this.getRoot, itemDist)
    nearestNeighbour(bp)
  }

  /**
   * Finds the item in this tree which is nearest to the given {link Object}, using {link
   * ItemDistance} as the distance metric. A Branch-and-Bound tree traversal algorithm is used to
   * provide an efficient search. <p> The query <tt>object</tt> does <b>not</b> have to be contained
   * in the tree, but it does have to be compatible with the <tt>itemDist</tt> distance metric.
   *
   * @param env
   *   the envelope of the query item
   * @param item
   *   the item to find the nearest neighbour of
   * @param itemDist
   *   a distance metric applicable to the items in this tree and the query item return the nearest
   *   item in this tree or <code>null</code> if the tree is empty
   */
  def nearestNeighbour(env: Envelope, item: Any, itemDist: ItemDistance): Any = {
    val bnd = new ItemBoundable(env, item)
    val bp  = new BoundablePair(this.getRoot, bnd, itemDist)
    nearestNeighbour(bp)(0)
  }

  /**
   * Finds the two nearest items from this tree and another tree, using {link ItemDistance} as the
   * distance metric. A Branch-and-Bound tree traversal algorithm is used to provide an efficient
   * search. The result value is a pair of items, the first from this tree and the second from the
   * argument tree.
   *
   * @param tree
   *   another tree
   * @param itemDist
   *   a distance metric applicable to the items in the trees return the pair of the nearest items,
   *   one from each tree or <code>null</code> if no pair of distinct items can be found
   */
  def nearestNeighbour(tree: STRtree, itemDist: ItemDistance): Array[Any] = {
    if (isEmpty || tree.isEmpty) return null
    val bp = new BoundablePair(this.getRoot, tree.getRoot, itemDist)
    nearestNeighbour(bp)
  }

  private def nearestNeighbour(initBndPair: BoundablePair): Array[Any] = {
    var distanceLowerBound     = java.lang.Double.POSITIVE_INFINITY
    var minPair: BoundablePair = null
    // initialize search queue
    val priQ                   = new PriorityQueue
    priQ.add(initBndPair)
    while (!priQ.isEmpty && distanceLowerBound > 0.0) { // pop head of queue and expand one side of pair
      val bndPair      = priQ.poll.asInstanceOf[BoundablePair]
      val pairDistance = bndPair.getDistance

      /**
       * If the distance for the first pair in the queue is >= current minimum distance, other nodes
       * in the queue must also have a greater distance. So the current minDistance must be the true
       * minimum, and we are done.
       */
      if (pairDistance >= distanceLowerBound) {
        distanceLowerBound = 1 // break //todo: break is not supported
      } else {

        /**
         * If the pair members are leaves then their distance is the exact lower bound. Update the
         * distanceLowerBound to reflect this (which must be smaller, due to the test immediately
         * prior to this).
         */
        if (bndPair.isLeaves) { // assert: currentDistance < minimumDistanceFound
          distanceLowerBound = pairDistance
          minPair = bndPair
        } else {

          /**
           * Otherwise, expand one side of the pair, and insert the expanded pairs into the queue.
           * The choice of which side to expand is determined heuristically.
           */
          bndPair.expandToQueue(priQ, distanceLowerBound)
        }
      }
    }
    if (minPair == null) return null
    // done - return items with min distance
    Array[Any](minPair.getBoundable(0).asInstanceOf[ItemBoundable].getItem,
               minPair.getBoundable(1).asInstanceOf[ItemBoundable].getItem
    )
  }

  /**
   * Tests whether some two items from this tree and another tree lie within a given distance. {link
   * ItemDistance} is used as the distance metric. A Branch-and-Bound tree traversal algorithm is
   * used to provide an efficient search.
   *
   * @param tree
   *   another tree
   * @param itemDist
   *   a distance metric applicable to the items in the trees
   * @param maxDistance
   *   the distance limit for the search return true if there are items within the distance
   */
  def isWithinDistance(tree: STRtree, itemDist: ItemDistance, maxDistance: Double): Boolean = {
    val bp = new BoundablePair(this.getRoot, tree.getRoot, itemDist)
    isWithinDistance(bp, maxDistance)
  }

  /**
   * Performs a withinDistance search on the tree node pairs. This is a different search algorithm
   * to nearest neighbour. It can utilize the {link BoundablePair#maximumDistance()} between tree
   * nodes to confirm if two internal nodes must have items closer than the maxDistance, and
   * short-circuit the search.
   *
   * @param initBndPair
   *   the initial pair containing the tree root nodes
   * @param maxDistance
   *   the maximum distance to search for return true if two items lie within the given distance
   */
  private def isWithinDistance(initBndPair: BoundablePair, maxDistance: Double): Boolean = {
    var distanceUpperBound = java.lang.Double.POSITIVE_INFINITY
    val priQ               = new PriorityQueue
    priQ.add(initBndPair)
    while (!priQ.isEmpty) {
      val bndPair      = priQ.poll.asInstanceOf[BoundablePair]
      val pairDistance = bndPair.getDistance

      /**
       * If the distance for the first pair in the queue is > maxDistance, all other pairs in the
       * queue must have a greater distance as well. So can conclude no items are within the
       * distance and terminate with result = false
       */
      if (pairDistance > maxDistance) return false

      /**
       * If the maximum distance between the nodes is less than the maxDistance, than all items in
       * the nodes must be closer than the max distance. Then can terminate with result = true.
       *
       * NOTE: using Envelope MinMaxDistance would provide a tighter bound, but not much performance
       * improvement has been observed
       */
      if (bndPair.maximumDistance <= maxDistance) return true

      /**
       * If the pair items are leaves then their actual distance is an upper bound. Update the
       * distanceUpperBound to reflect this
       */
      if (bndPair.isLeaves) {
        distanceUpperBound = pairDistance

        /**
         * If the items are closer than maxDistance can terminate with result = true.
         */
        if (distanceUpperBound <= maxDistance) return true
      } else bndPair.expandToQueue(priQ, distanceUpperBound)
    }
    return false
  }

  /**
   * Finds k items in this tree which are the top k nearest neighbors to the given {@code item},
   * using {@code itemDist} as the distance metric. A Branch-and-Bound tree traversal algorithm is
   * used to provide an efficient search. This method implements the KNN algorithm described in the
   * following paper: <p> Roussopoulos, Nick, Stephen Kelley, and Frédéric Vincent. "Nearest
   * neighbor queries." ACM sigmod record. Vol. 24. No. 2. ACM, 1995. <p> The query {@code item}
   * does <b>not</b> have to be contained in the tree, but it does have to be compatible with the
   * {@code itemDist} distance metric.
   *
   * @param env
   *   the envelope of the query item
   * @param item
   *   the item to find the nearest neighbour of
   * @param itemDist
   *   a distance metric applicable to the items in this tree and the query item
   * @param k
   *   the K nearest items in kNearestNeighbour return the K nearest items in this tree
   */
  def nearestNeighbour(env: Envelope, item: Any, itemDist: ItemDistance, k: Int): Array[Any] = {
    val bnd = new ItemBoundable(env, item)
    val bp  = new BoundablePair(this.getRoot, bnd, itemDist)
    nearestNeighbourK(bp, k)
  }

  private def nearestNeighbourK(initBndPair: BoundablePair, k: Int): Array[Any] =
    nearestNeighbourK(initBndPair, java.lang.Double.POSITIVE_INFINITY, k)

  private def nearestNeighbourK(
    initBndPair: BoundablePair,
    maxDistance: Double,
    k:           Int
  ): Array[Any] = {
    var distanceLowerBound = maxDistance
    // initialize internal structures
    val priQ               = new PriorityQueue
    // initialize queue
    priQ.add(initBndPair)
    val kNearestNeighbors  = new PriorityQueue
    while (!priQ.isEmpty && distanceLowerBound >= 0.0) {
      val bndPair      = priQ.poll.asInstanceOf[BoundablePair]
      val pairDistance = bndPair.getDistance

      /**
       * If the distance for the first node in the queue is >= the current maximum distance in the k
       * queue , all other nodes in the queue must also have a greater distance. So the current
       * minDistance must be the true minimum, and we are done.
       */
      if (pairDistance >= distanceLowerBound) {
        distanceLowerBound = 11 // break
      } else { // todo: break is not supported
        if (bndPair.isLeaves)
          if (kNearestNeighbors.size() < k) kNearestNeighbors.add(bndPair)
          else {
            val bp1 = kNearestNeighbors.peek.asInstanceOf[BoundablePair]
            if (bp1.getDistance > pairDistance) {
              kNearestNeighbors.poll
              kNearestNeighbors.add(bndPair)
            }
            /*
             * minDistance should be the farthest point in the K nearest neighbor queue.
             */
            val bp2 = kNearestNeighbors.peek.asInstanceOf[BoundablePair]
            distanceLowerBound = bp2.getDistance
          }
        else {

          /**
           * Otherwise, expand one side of the pair, (the choice of which side to expand is
           * heuristically determined) and insert the new expanded pairs into the queue
           */
          bndPair.expandToQueue(priQ, distanceLowerBound)
        }
      }
    }
    STRtree.getItems(kNearestNeighbors)
  }
}
