// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
package org.locationtech.jts.index.hprtree

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.index.ArrayListVisitor
import org.locationtech.jts.index.ItemVisitor
import org.locationtech.jts.index.SpatialIndex
import org.locationtech.jts.util.IntArrayList

import java.util

object HPRtree {
  private val ENV_SIZE = 4

  private val HILBERT_LEVEL = 12

  private val DEFAULT_NODE_CAPACITY = 16

  private def intersects(bounds: Array[Double], nodeIndex: Int, env: Envelope): Boolean = {
    val isBeyond = (env.getMaxX < bounds(nodeIndex)) ||
      (env.getMaxY < bounds(nodeIndex + 1)) ||
      (env.getMinX > bounds(nodeIndex + 2)) ||
      (env.getMinY > bounds(nodeIndex + 3))
    !isBeyond
  }

  private def createBoundsArray(size: Int): Array[Double] = {
    val a = new Array[Double](4 * size)
    var i = 0
    while (i < size) {
      val index = 4 * i
      a(index) = Double.MaxValue
      a(index + 1) = Double.MaxValue
      a(index + 2) = -Double.MaxValue
      a(index + 3) = -Double.MaxValue
      i += 1
    }
    a
  }

  private def computeLayerIndices(itemSize: Int, nodeCapacity: Int): Array[Int] = {
    val layerIndexList = new IntArrayList
    var layerSize      = itemSize
    var index          = 0
    var done           = false
    while (!done) {
      layerIndexList.add(index)
      layerSize = numNodesToCover(layerSize, nodeCapacity)
      index += ENV_SIZE * layerSize
      done = layerSize <= 1
    }
    layerIndexList.toArray
  }

  /**
   * Computes the number of blocks (nodes) required to cover a given number of children.
   *
   * @param nChild
   * @param nodeCapacity
   *   return the number of nodes needed to cover the children
   */
  private def numNodesToCover(nChild: Int, nodeCapacity: Int): Int = {
    val mult  = nChild / nodeCapacity
    val total = mult * nodeCapacity
    if (total == nChild) return mult
    mult + 1
  }
}

/**
 * A Hilbert-Packed R-tree. This is a static R-tree which is packed by using the Hilbert ordering
 * of the tree items. <p> The tree is constructed by sorting the items by the Hilbert code of the
 * midpoint of their envelope. Then, a set of internal layers is created recursively as follows:
 * <ul> <li>The items/nodes of the previous are partitioned into blocks of size
 * <code>nodeCapacity</code> <li>For each block a layer node is created with range equal to the
 * envelope of the items/nodes in the block </ul> The internal layers are stored using an array to
 * store the node bounds. The link between a node and its children is stored implicitly in the
 * indexes of the array. For efficiency, the offsets to the layers within the node array are
 * pre-computed and stored. <p> NOTE: Based on performance testing, the HPRtree is somewhat faster
 * than the STRtree. It should also be more memory-efficient, due to fewer object allocations.
 *
 * @author
 *   Martin Davis
 */
class HPRtree(private val nodeCapacity: Int) extends SpatialIndex[Any] {
  import HPRtree.*

  private var itemsToLoad: util.List[Item] = new util.ArrayList[Item]

  private var numItems = 0

  private val totalExtent = new Envelope

  private var layerStartIndex: Array[Int] = null

  private var nodeBounds: Array[Double] = null

  private var itemBounds: Array[Double] = null

  private var itemValues: Array[Any] = null

  @volatile private var isBuilt = false

  /**
   * Creates a new index with the default node capacity.
   */
  def this() = this(HPRtree.DEFAULT_NODE_CAPACITY)

  /**
   * Gets the number of items in the index.
   *
   * return the number of items
   */
  def size: Int = numItems

  override def insert(itemEnv: Envelope, item: Any): Unit = {
    if (isBuilt) throw new IllegalStateException("Cannot insert items after tree is built.")
    numItems += 1
    itemsToLoad.add(new Item(itemEnv, item))
    totalExtent.expandToInclude(itemEnv)
    ()
  }

  override def query(searchEnv: Envelope): util.List[Any] = {
    build()

    if (!totalExtent.intersects(searchEnv)) return new util.ArrayList[Any]

    val visitor = new ArrayListVisitor
    query(searchEnv, visitor)
    visitor.getItems.asInstanceOf[util.List[Any]]
  }

  override def query(searchEnv: Envelope, visitor: ItemVisitor): Unit = {
    build()
    if (!totalExtent.intersects(searchEnv)) return
    if (layerStartIndex == null) queryItems(0, searchEnv, visitor)
    else queryTopLayer(searchEnv, visitor)
  }

  private def queryTopLayer(searchEnv: Envelope, visitor: ItemVisitor): Unit = {
    val layerIndex = layerStartIndex.length - 2
    val sz         = layerSize(layerIndex)
    // query each node in layer
    var i          = 0
    while (i < sz) {
      queryNode(layerIndex, i, searchEnv, visitor)
      i += ENV_SIZE
    }
  }

  private def queryNode(
    layerIndex: Int,
    nodeOffset: Int,
    searchEnv:  Envelope,
    visitor:    ItemVisitor
  ): Unit = {
    val layerStart = layerStartIndex(layerIndex)
    val nodeIndex  = layerStart + nodeOffset
    if (!intersects(nodeBounds, nodeIndex, searchEnv)) return
    if (layerIndex == 0) {
      val childNodesOffset = nodeOffset / ENV_SIZE * nodeCapacity
      queryItems(childNodesOffset, searchEnv, visitor)
    } else {
      val childNodesOffset = nodeOffset * nodeCapacity
      queryNodeChildren(layerIndex - 1, childNodesOffset, searchEnv, visitor)
    }
  }

  private def queryNodeChildren(
    layerIndex:  Int,
    blockOffset: Int,
    searchEnv:   Envelope,
    visitor:     ItemVisitor
  ): Unit = {
    val layerStart = layerStartIndex(layerIndex)
    val layerEnd   = layerStartIndex(layerIndex + 1)
    var i          = 0
    var break      = false
    while (!break && i < nodeCapacity) {
      val nodeOffset = blockOffset + ENV_SIZE * i
      // don't query past layer end
      if (layerStart + nodeOffset >= layerEnd) break = true
      else queryNode(layerIndex, nodeOffset, searchEnv, visitor)
      i += 1
    }
  }

  private def queryItems(blockStart: Int, searchEnv: Envelope, visitor: ItemVisitor): Unit = {
    var i     = 0
    var break = false
    while (!break && i < nodeCapacity) {
      val itemIndex = blockStart + i
      // don't query past end of items
      if (itemIndex >= numItems) break = true
      else if (intersects(itemBounds, itemIndex * ENV_SIZE, searchEnv))
        visitor.visitItem(itemValues(itemIndex))
      i += 1
    }
  }

  private def layerSize(layerIndex: Int): Int = {
    val layerStart = layerStartIndex(layerIndex)
    val layerEnd   = layerStartIndex(layerIndex + 1)
    layerEnd - layerStart
  }

  override def remove(itemEnv: Envelope, item: Any): Unit = ()

  /**
   * Builds the index, if not already built.
   */
  def build(): Unit =
    // skip if already built
    if (!isBuilt) synchronized {
      if (!isBuilt) {
        prepareIndex()
        prepareItems()
        this.isBuilt = true
      }
    }

  private def prepareIndex(): Unit = {
    // don't need to build an empty or very small tree
    if (itemsToLoad.size <= nodeCapacity) return

    sortItems()

    layerStartIndex = computeLayerIndices(numItems, nodeCapacity)
    // allocate storage
    val nodeCount = layerStartIndex(layerStartIndex.length - 1) / 4
    nodeBounds = createBoundsArray(nodeCount)

    // compute tree nodes
    computeLeafNodes(layerStartIndex(1))
    var i         = 1
    while (i < layerStartIndex.length - 1) {
      computeLayerNodes(i)
      i += 1
    }
  }

  private def prepareItems(): Unit = {
    // copy item contents out to arrays for querying
    var boundsIndex = 0
    var valueIndex  = 0
    itemBounds = new Array[Double](itemsToLoad.size * 4)
    itemValues = new Array[Any](itemsToLoad.size)
    val it          = itemsToLoad.iterator
    while (it.hasNext) {
      val item     = it.next
      val envelope = item.getEnvelope
      itemBounds(boundsIndex) = envelope.getMinX
      itemBounds(boundsIndex + 1) = envelope.getMinY
      itemBounds(boundsIndex + 2) = envelope.getMaxX
      itemBounds(boundsIndex + 3) = envelope.getMaxY
      boundsIndex += 4
      itemValues(valueIndex) = item.getItem
      valueIndex += 1
    }
    // and let GC free the original list
    itemsToLoad = null
  }

  private def computeLayerNodes(layerIndex: Int): Unit = {
    val layerStart      = layerStartIndex(layerIndex)
    val childLayerStart = layerStartIndex(layerIndex - 1)
    val sz              = layerSize(layerIndex)
    val childLayerEnd   = layerStart
    var i               = 0
    while (i < sz) {
      val childStart = childLayerStart + nodeCapacity * i
      computeNodeBounds(layerStart + i, childStart, childLayerEnd)
      i += ENV_SIZE
    }
  }

  private def computeNodeBounds(nodeIndex: Int, blockStart: Int, nodeMaxIndex: Int): Unit = {
    var i     = 0
    var break = false
    while (!break && i <= nodeCapacity) {
      val index = blockStart + 4 * i
      if (index >= nodeMaxIndex) break = true
      else
        updateNodeBounds(nodeIndex,
                         nodeBounds(index),
                         nodeBounds(index + 1),
                         nodeBounds(index + 2),
                         nodeBounds(index + 3)
        )
      i += 1
    }
  }

  private def computeLeafNodes(layerSize: Int): Unit = {
    var i = 0
    while (i < layerSize) {
      computeLeafNodeBounds(i, nodeCapacity * i / 4)
      i += ENV_SIZE
    }
  }

  private def computeLeafNodeBounds(nodeIndex: Int, blockStart: Int): Unit = {
    var i     = 0
    var break = false
    while (!break && i <= nodeCapacity) {
      val itemIndex = blockStart + i
      if (itemIndex >= itemsToLoad.size) break = true
      else {
        val env = itemsToLoad.get(itemIndex).getEnvelope
        updateNodeBounds(nodeIndex, env.getMinX, env.getMinY, env.getMaxX, env.getMaxY)
      }
      i += 1
    }
  }

  private def updateNodeBounds(
    nodeIndex: Int,
    minX:      Double,
    minY:      Double,
    maxX:      Double,
    maxY:      Double
  ): Unit = {
    if (minX < nodeBounds(nodeIndex)) nodeBounds(nodeIndex) = minX
    if (minY < nodeBounds(nodeIndex + 1)) nodeBounds(nodeIndex + 1) = minY
    if (maxX > nodeBounds(nodeIndex + 2)) nodeBounds(nodeIndex + 2) = maxX
    if (maxY > nodeBounds(nodeIndex + 3)) nodeBounds(nodeIndex + 3) = maxY
  }

  /**
   * Gets the extents of the internal index nodes
   *
   * return a list of the internal node extents
   */
  def getBounds: Array[Envelope] = {
    val numNodes = nodeBounds.length / 4
    val bounds   = new Array[Envelope](numNodes)
    // create from largest to smallest
    var i        = numNodes - 1
    while (i >= 0) {
      val boundIndex = 4 * i
      bounds(i) = new Envelope(nodeBounds(boundIndex),
                               nodeBounds(boundIndex + 2),
                               nodeBounds(boundIndex + 1),
                               nodeBounds(boundIndex + 3)
      )
      i -= 1
    }
    bounds
  }

  private def sortItems(): Unit = {
    val encoder       = new HilbertEncoder(HILBERT_LEVEL, totalExtent)
    val hilbertValues = new Array[Int](itemsToLoad.size)
    var pos           = 0
    val it            = itemsToLoad.iterator
    while (it.hasNext) {
      hilbertValues(pos) = encoder.encode(it.next.getEnvelope)
      pos += 1
    }
    quickSortItemsIntoNodes(hilbertValues, 0, itemsToLoad.size - 1)
  }

  private def quickSortItemsIntoNodes(values: Array[Int], lo: Int, hi: Int): Unit =
    // stop sorting when left/right pointers are within the same node
    // because queryItems just searches through them all sequentially
    if (lo / nodeCapacity < hi / nodeCapacity) {
      val pivot = hoarePartition(values, lo, hi)
      quickSortItemsIntoNodes(values, lo, pivot)
      quickSortItemsIntoNodes(values, pivot + 1, hi)
    }

  private def hoarePartition(values: Array[Int], lo: Int, hi: Int): Int = {
    val pivot = values((lo + hi) >> 1)
    var i     = lo - 1
    var j     = hi + 1

    while (true) {
      i += 1
      while (values(i) < pivot) i += 1
      j -= 1
      while (values(j) > pivot) j -= 1
      if (i >= j) return j
      swapItems(values, i, j)
    }
    j
  }

  private def swapItems(values: Array[Int], i: Int, j: Int): Unit = {
    val tmpItem = itemsToLoad.get(i)
    itemsToLoad.set(i, itemsToLoad.get(j))
    itemsToLoad.set(j, tmpItem)

    val tmpValue = values(i)
    values(i) = values(j)
    values(j) = tmpValue
  }
}
