// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.index.kdtree

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Envelope

import java.util

/*
 * Copyright (c) 2016 Vivid Solutions.
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
 * An implementation of a 2-D KD-Tree. KD-trees provide fast range searching on point data. <p> This
 * implementation supports detecting and snapping points which are closer than a given distance
 * tolerance. If the same point (up to tolerance) is inserted more than once, it is snapped to the
 * existing node. In other words, if a point is inserted which lies within the tolerance of a node
 * already in the index, it is snapped to that node. When a point is snapped to a node then a new
 * node is not created but the count of the existing node is incremented. If more than one node in
 * the tree is within tolerance of an inserted point, the closest and then lowest node is snapped
 * to.
 *
 * @author
 *   David Skea
 * @author
 *   Martin Davis
 */
object KdTree {

  /**
   * Converts a collection of {@link KdNode} s to an array of {@link Coordinate} s.
   *
   * @param kdnodes
   *   a collection of nodes
   * @return
   *   an array of the coordinates represented by the nodes
   */
  def toCoordinates(kdnodes: util.Collection[_]): Array[Coordinate] = toCoordinates(kdnodes, false)

  /**
   * Converts a collection of {@link KdNode} s to an array of {@link Coordinate} s, specifying
   * whether repeated nodes should be represented by multiple coordinates.
   *
   * @param kdnodes
   *   a collection of nodes
   * @param includeRepeated
   *   true if repeated nodes should be included multiple times
   * @return
   *   an array of the coordinates represented by the nodes
   */
  def toCoordinates(kdnodes: util.Collection[_], includeRepeated: Boolean): Array[Coordinate] = {
    val coord = new CoordinateList
    val it    = kdnodes.iterator
    while (it.hasNext) {
      val node  = it.next.asInstanceOf[KdNode]
      val count =
        if (includeRepeated) node.getCount
        else 1
      for (_ <- 0 until count)
        coord.add(node.getCoordinate, true)
    }
    coord.toCoordinateArray
  }

  private class BestMatchVisitor(var p: Coordinate, var tolerance: Double) extends KdNodeVisitor {
    private var matchNode: KdNode = null
    private var matchDist         = 0.0

    def queryEnvelope: Envelope = {
      val queryEnv = new Envelope(p)
      queryEnv.expandBy(tolerance)
      queryEnv
    }

    def getNode: KdNode = matchNode

    override def visit(node: KdNode): Unit = {
      val dist          = p.distance(node.getCoordinate)
      val isInTolerance = dist <= tolerance
      if (!isInTolerance) return
      var update        = false
      if (
        matchNode == null || dist < matchDist || // if distances are the same, record the lesser coordinate
        (matchNode != null && dist == matchDist && node.getCoordinate.compareTo(
          matchNode.getCoordinate
        ) < 1)
      ) update = true
      if (update) {
        matchNode = node
        matchDist = dist
      }
    }
  }
}

/**
 * Creates a new instance of a KdTree, specifying a snapping distance tolerance. Points which lie
 * closer than the tolerance to a point already in the tree will be treated as identical to the
 * existing point.
 *
 * @param tolerance
 *   the tolerance distance for considering two points equal
 */
class KdTree(var tolerance: Double) {
  private var root: KdNode        = null
  private var numberOfNodes: Long = 0L

  /**
   * Creates a new instance of a KdTree with a snapping tolerance of 0.0. (I.e. distinct points will
   * <i>not</i> be snapped)
   */
  def this() =
    this(0.0)

  /**
   * Tests whether the index contains any items.
   *
   * @return
   *   true if the index does not contain any items
   */
  def isEmpty: Boolean = {
    if (root == null) {
      return true
    }
    return false
  }

  /**
   * Inserts a new point in the kd-tree, with no data.
   *
   * @param p
   *   the point to insert
   * @return
   *   the kdnode containing the point
   */
  def insert(p: Coordinate): KdNode =
    return insert(p, null)

  /**
   * Inserts a new point into the kd-tree.
   *
   * @param p
   *   the point to insert
   * @param data
   *   a data item for the point
   * @return
   *   returns a new KdNode if a new point is inserted, else an existing node is returned with its
   *   counter incremented. This can be checked by testing returnedNode.getCount() &gt; 1.
   */
  def insert(p: Coordinate, data: Any): KdNode = {
    if (root == null) {
      root = new KdNode(p, data)
      return root
    }

    /**
     * Check if the point is already in the tree, up to tolerance. If tolerance is zero, this phase
     * of the insertion can be skipped.
     */
    if (tolerance > 0) {
      val matchNode: KdNode = findBestMatchNode(p)
      if (matchNode != null) { // point already in index - increment counter
        matchNode.increment()
        return matchNode
      }
    }
    return insertExact(p, data)
  }

  /**
   * Finds the node in the tree which is the best match for a point being inserted. The match is
   * made deterministic by returning the lowest of any nodes which lie the same distance from the
   * point. There may be no match if the point is not within the distance tolerance of any existing
   * node.
   *
   * @param p
   *   the point being inserted
   * @return
   *   the best matching node
   * @return
   *   null if no match was found
   */
  private def findBestMatchNode(p: Coordinate): KdNode = {
    val visitor: KdTree.BestMatchVisitor = new KdTree.BestMatchVisitor(p, tolerance)
    query(visitor.queryEnvelope, visitor)
    return visitor.getNode
  }

  /**
   * Inserts a point known to be beyond the distance tolerance of any existing node. The point is
   * inserted at the bottom of the exact splitting path, so that tree shape is deterministic.
   *
   * @param p
   *   the point to insert
   * @param data
   *   the data for the point
   * @return
   *   the created node
   */
  private def insertExact(p: Coordinate, data: Any): KdNode = {
    var currentNode: KdNode = root
    var leafNode: KdNode    = root
    var isOddLevel: Boolean = true
    var isLessThan: Boolean = true

    /**
     * Traverse the tree, first cutting the plane left-right (by X ordinate) then top-bottom (by Y
     * ordinate)
     */
    while (currentNode != null) { // test if point is already a node (not strictly necessary)
      val isInTolerance = p.distance(currentNode.getCoordinate) <= tolerance

      // check if point is already in tree (up to tolerance) and if so simply
      // return existing node
      if (isInTolerance) {
        currentNode.increment()
        return currentNode
      }
      if (isOddLevel) {
        isLessThan = p.x < currentNode.getX
      } else {
        isLessThan = p.y < currentNode.getY
      }
      leafNode = currentNode
      if (isLessThan) {
        currentNode = currentNode.getLeft
      } else {
        currentNode = currentNode.getRight
      }
      isOddLevel = !isOddLevel
    }
    // no node found, add new leaf node to tree
    numberOfNodes = numberOfNodes + 1
    val node: KdNode = new KdNode(p, data)
    if (isLessThan) {
      leafNode.setLeft(node)
    } else {
      leafNode.setRight(node)
    }
    return node
  }

  private def queryNode(
    currentNode: KdNode,
    queryEnv:    Envelope,
    odd:         Boolean,
    visitor:     KdNodeVisitor
  ): Unit = {
    if (currentNode == null) {
      return
    }
    var min: Double          = .0
    var max: Double          = .0
    var discriminant: Double = .0
    if (odd) {
      min = queryEnv.getMinX
      max = queryEnv.getMaxX
      discriminant = currentNode.getX
    } else {
      min = queryEnv.getMinY
      max = queryEnv.getMaxY
      discriminant = currentNode.getY
    }
    val searchLeft: Boolean  = min < discriminant
    val searchRight: Boolean = discriminant <= max
    // search is computed via in-order traversal
    if (searchLeft) {
      queryNode(currentNode.getLeft, queryEnv, !odd, visitor)
    }
    if (queryEnv.contains(currentNode.getCoordinate)) {
      visitor.visit(currentNode)
    }
    if (searchRight) {
      queryNode(currentNode.getRight, queryEnv, !odd, visitor)
    }
  }

  private def queryNodePoint(currentNode: KdNode, queryPt: Coordinate, odd: Boolean): KdNode = {
    if (currentNode == null) {
      return null
    }
    if (currentNode.getCoordinate.equals2D(queryPt)) {
      return currentNode
    }
    var ord: Double          = .0
    var discriminant: Double = .0
    if (odd) {
      ord = queryPt.getX
      discriminant = currentNode.getX
    } else {
      ord = queryPt.getY
      discriminant = currentNode.getY
    }
    val searchLeft: Boolean  = ord < discriminant
    if (searchLeft) {
      return queryNodePoint(currentNode.getLeft, queryPt, !odd)
    } else {
      return queryNodePoint(currentNode.getRight, queryPt, !odd)
    }
  }

  /**
   * Performs a range search of the points in the index and visits all nodes found.
   *
   * @param queryEnv
   *   the range rectangle to query
   * @param visitor
   *   a visitor to visit all nodes found by the search
   */
  def query(queryEnv: Envelope, visitor: KdNodeVisitor): Unit =
    queryNode(root, queryEnv, true, visitor)

  /**
   * Performs a range search of the points in the index.
   *
   * @param queryEnv
   *   the range rectangle to query
   * @return
   *   a list of the KdNodes found
   */
  def query(queryEnv: Envelope): util.List[KdNode] = {
    val result: util.List[KdNode] = new util.ArrayList[KdNode]
    query(queryEnv, result)
    return result
  }

  /**
   * Performs a range search of the points in the index.
   *
   * @param queryEnv
   *   the range rectangle to query
   * @param result
   *   a list to accumulate the result nodes into
   */
  def query(queryEnv: Envelope, result: util.List[KdNode]): Unit =
    queryNode(root,
              queryEnv,
              true,
              new KdNodeVisitor() {
                override def visit(node: KdNode): Unit = {
                  result.add(node)
                  ()
                }
              }
    )

  /**
   * Searches for a given point in the index and returns its node if found.
   *
   * @param queryPt
   *   the point to query
   * @return
   *   the point node, if it is found in the index, or null if not
   */
  def query(queryPt: Coordinate): KdNode =
    return queryNodePoint(root, queryPt, true)

  /**
   * Computes the depth of the tree.
   *
   * @return
   *   the depth of the tree
   */
  def depth(): Int =
    depthNode(root)

  private def depthNode(currentNode: KdNode): Int =
    if (currentNode == null) {
      0
    } else {

      val dL = depthNode(currentNode.getLeft)
      val dR = depthNode(currentNode.getRight)
      1 + (if (dL > dR) dL else dR)
    }

  /**
   * Computes the size (number of items) in the tree.
   *
   * @return
   *   the size of the tree
   */
  def size(): Int =
    sizeNode(root)

  private def sizeNode(currentNode: KdNode): Int =
    if (currentNode == null) {
      0
    } else {

      val sizeL = sizeNode(currentNode.getLeft)
      val sizeR = sizeNode(currentNode.getRight)
      1 + sizeL + sizeR
    }
}
