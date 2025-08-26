// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

import org.locationtech.jts.index.ItemVisitor
import org.locationtech.jts.index.strtree.AbstractSTRtree.IntersectsOp
import org.locationtech.jts.util.Assert

import java.io.Serializable
import java.util
import java.util.Collections
import java.util.Comparator
import scala.jdk.CollectionConverters.*

/**
 * Base class for STRtree and SIRtree. STR-packed R-trees are described in: P. Rigaux, Michel Scholl
 * and Agnes Voisard. <i>Spatial Databases With Application To GIS.</i> Morgan Kaufmann, San
 * Francisco, 2002. <p> This implementation is based on {link Boundable}s rather than {link
 * AbstractNode}s, because the STR algorithm operates on both nodes and data, both of which are
 * treated as Boundables. <p> This class is thread-safe. Building the tree is synchronized, and
 * querying is stateless.
 *
 * @see
 *   STRtree
 * @see
 *   SIRtree
 * @version 1.7
 */
@SerialVersionUID(-3886435814360241337L)
object AbstractSTRtree {

  /**
   * A test for intersection between two bounds, necessary because subclasses of AbstractSTRtree
   * have different implementations of bounds.
   */
  trait IntersectsOp {

    /**
     * For STRtrees, the bounds will be Envelopes; for SIRtrees, Intervals; for other subclasses of
     * AbstractSTRtree, some other class.
     *
     * @param aBounds
     *   the bounds of one spatial object
     * @param bBounds
     *   the bounds of another spatial object return whether the two bounds intersect
     */
    def intersects(aBounds: Any, bBounds: Any): Boolean
  }

  private val DEFAULT_NODE_CAPACITY = 10

  def compareDoubles(a: Double, b: Double): Int = if (a > b) 1
  else if (a < b) -1
  else 0
}

@SerialVersionUID(-3886435814360241337L)
abstract class AbstractSTRtree(var nodeCapacity: Int)

/**
 * Constructs an AbstractSTRtree with the specified maximum number of child nodes that a node may
 * have
 *
 * @param nodeCapacity
 *   the maximum number of child nodes in a node
 */
    extends Serializable {
  Assert.isTrue(nodeCapacity > 1, "Node capacity must be greater than 1")
  protected var root: AbstractNode = null
  private var built                = false

  /**
   * Set to <tt>null</tt> when index is built, to avoid retaining memory.
   */
  private var itemBoundables = new util.ArrayList[Boundable]

  /**
   * Constructs an AbstractSTRtree with the default node capacity.
   */
  def this() =
    this(AbstractSTRtree.DEFAULT_NODE_CAPACITY)

  /**
   * Creates parent nodes, grandparent nodes, and so forth up to the root node, for the data that
   * has been inserted into the tree. Can only be called once, and thus can be called only after all
   * of the data has been inserted into the tree.
   */
  def build(): Unit = {
    if (built) return
    root =
      if (itemBoundables.isEmpty) createNode(0)
      else createHigherLevels(itemBoundables, -1)
    // the item list is no longer needed
    itemBoundables = null
    built = true
  }

  protected def createNode(level: Int): AbstractNode

  /**
   * Sorts the childBoundables then divides them into groups of size M, where M is the node
   * capacity.
   */
  protected def createParentBoundables(
    childBoundables: util.List[Boundable],
    newLevel:        Int
  ): util.ArrayList[AbstractNode] = {
    Assert.isTrue(!childBoundables.isEmpty)
    val parentBoundables                            = new util.ArrayList[AbstractNode]
    parentBoundables.add(createNode(newLevel))
    val sortedChildBoundables: util.List[Boundable] = new util.ArrayList(childBoundables)
    Collections.sort(sortedChildBoundables, getComparator)
    val i                                           = sortedChildBoundables.iterator
    while (i.hasNext) {
      val childBoundable = i.next
      if (lastNode(parentBoundables).getChildBoundables.size == getNodeCapacity)
        parentBoundables.add(createNode(newLevel))
      lastNode(parentBoundables).addChildBoundable(childBoundable)
    }
    parentBoundables
  }

  protected def lastNode(nodes: util.List[AbstractNode]): AbstractNode = nodes.get(nodes.size - 1)

  /**
   * Creates the levels higher than the given level
   *
   * @param boundablesOfALevel
   *   the level to build on
   * @param level
   *   the level of the Boundables, or -1 if the boundables are item boundables (that is, below
   *   level 0) return the root, which may be a ParentNode or a LeafNode
   */
  private def createHigherLevels(
    boundablesOfALevel: util.List[Boundable],
    level:              Int
  ): AbstractNode = {
    Assert.isTrue(!boundablesOfALevel.isEmpty)
    val parentBoundables = createParentBoundables(boundablesOfALevel, level + 1)
    if (parentBoundables.size == 1) return parentBoundables.get(0)
    createHigherLevels(parentBoundables.asScala.map(x => x: Boundable).asJava, level + 1)
  }

  /**
   * Gets the root node of the tree.
   *
   * return the root node
   */
  def getRoot: AbstractNode = {
    build()
    root
  }

  /**
   * Returns the maximum number of child nodes that a node may have.
   *
   * return the node capacity
   */
  def getNodeCapacity: Int = nodeCapacity

  /**
   * Tests whether the index contains any items. This method does not build the index, so items can
   * still be inserted after it has been called.
   *
   * return true if the index does not contain any items
   */
  def isEmpty: Boolean = {
    if (!built) return itemBoundables.isEmpty
    root.isEmpty
  }

  protected def size: Int = {
    if (isEmpty) return 0
    build()
    size(root)
  }

  protected def size(node: AbstractNode): Int = {
    var vsize = 0
    val i     = node.getChildBoundables.iterator
    while (i.hasNext) {
      val childBoundable = i.next
      if (childBoundable.isInstanceOf[AbstractNode])
        vsize += size(childBoundable.asInstanceOf[AbstractNode])
      else if (childBoundable.isInstanceOf[ItemBoundable]) vsize += 1
    }
    vsize
  }

  protected def depth: Int = {
    if (isEmpty) return 0
    build()
    depth(root)
  }

  protected def depth(node: AbstractNode): Int = {
    var maxChildDepth = 0
    val i             = node.getChildBoundables.iterator
    while (i.hasNext) {
      val childBoundable = i.next
      if (childBoundable.isInstanceOf[AbstractNode]) {
        val childDepth = depth(childBoundable.asInstanceOf[AbstractNode])
        if (childDepth > maxChildDepth) maxChildDepth = childDepth
      }
    }
    maxChildDepth + 1
  }

  protected def insert(bounds: AnyRef, item: Any) = {
    Assert.isTrue(!built, "Cannot insert items into an STR packed R-tree after it has been built.")
    itemBoundables.add(new ItemBoundable(bounds, item))
  }

  /**
   * Also builds the tree, if necessary.
   */
  protected def query(searchBounds: Any): util.List[Any] = {
    build()
    val matches = new util.ArrayList[Any]
    if (isEmpty) { // Assert.isTrue(root.getBounds() == null);
      return matches
    }
    if (getIntersectsOp.intersects(root.getBounds, searchBounds))
      queryInternal(searchBounds, root, matches)
    matches
  }

  protected def query(searchBounds: Any, visitor: ItemVisitor): Unit = {
    build()
    if (isEmpty) { // nothing in tree, so return
      return
    }
    if (getIntersectsOp.intersects(root.getBounds, searchBounds))
      queryInternal(searchBounds, root, visitor)
  }

  /**
   * return a test for intersection between two bounds, necessary because subclasses of
   * AbstractSTRtree have different implementations of bounds.
   * @see
   *   IntersectsOp
   */
  protected def getIntersectsOp: IntersectsOp

  private def queryInternal(
    searchBounds: Any,
    node:         AbstractNode,
    matches:      util.List[Any]
  ): Unit = {
    val childBoundables = node.getChildBoundables
    var i               = 0
    while (i < childBoundables.size) {
      val childBoundable = childBoundables.get(i)
      if (getIntersectsOp.intersects(childBoundable.getBounds, searchBounds)) {
        if (childBoundable.isInstanceOf[AbstractNode])
          queryInternal(searchBounds, childBoundable.asInstanceOf[AbstractNode], matches)
        else if (childBoundable.isInstanceOf[ItemBoundable])
          matches.add(childBoundable.asInstanceOf[ItemBoundable].getItem)
        else Assert.shouldNeverReachHere()
      }
      i += 1
    }
  }

  private def queryInternal(searchBounds: Any, node: AbstractNode, visitor: ItemVisitor): Unit = {
    val childBoundables = node.getChildBoundables
    var i               = 0
    while (i < childBoundables.size) {
      val childBoundable = childBoundables.get(i)
      if (getIntersectsOp.intersects(childBoundable.getBounds, searchBounds)) {
        if (childBoundable.isInstanceOf[AbstractNode])
          queryInternal(searchBounds, childBoundable.asInstanceOf[AbstractNode], visitor)
        else if (childBoundable.isInstanceOf[ItemBoundable])
          visitor.visitItem(childBoundable.asInstanceOf[ItemBoundable].getItem)
        else Assert.shouldNeverReachHere()
      }
      i += 1
    }
  }

  /**
   * Gets a tree structure (as a nested list) corresponding to the structure of the items and nodes
   * in this tree. <p> The returned {link List}s contain either {link Object} items, or Lists which
   * correspond to subtrees of the tree Subtrees which do not contain any items are not included.
   * <p> Builds the tree if necessary.
   *
   * return a List of items and/or Lists
   */
  def itemsTree: util.List[Any] = {
    build()
    val valuesTree = itemsTree(root)
    if (valuesTree == null) return new util.ArrayList[Any]
    valuesTree
  }

  private def itemsTree(node: AbstractNode): util.List[Any] = {
    val valuesTreeForNode = new util.ArrayList[Any]
    val i                 = node.getChildBoundables.iterator
    while (i.hasNext) {
      val childBoundable = i.next
      if (childBoundable.isInstanceOf[AbstractNode]) {
        val valuesTreeForChild = itemsTree(childBoundable.asInstanceOf[AbstractNode])
        // only add if not null (which indicates an item somewhere in this tree
        if (valuesTreeForChild != null) valuesTreeForNode.add(valuesTreeForChild)
      } else if (childBoundable.isInstanceOf[ItemBoundable])
        valuesTreeForNode.add(childBoundable.asInstanceOf[ItemBoundable].getItem)
      else Assert.shouldNeverReachHere()
    }
    if (valuesTreeForNode.size <= 0) return null
    valuesTreeForNode
  }

  /**
   * Removes an item from the tree. (Builds the tree, if necessary.)
   */
  protected def remove(searchBounds: Any, item: Any): Boolean = {
    build()
    if (getIntersectsOp.intersects(root.getBounds, searchBounds))
      return remove(searchBounds, root, item)
    false
  }

  private def removeItem(node: AbstractNode, item: Any): Boolean = {
    var childToRemove: Boundable = null
    val i                        = node.getChildBoundables.iterator
    while (i.hasNext) {
      val childBoundable = i.next
      if (childBoundable.isInstanceOf[ItemBoundable])
        if (childBoundable.asInstanceOf[ItemBoundable].getItem == item)
          childToRemove = childBoundable
    }
    if (childToRemove != null) {
      node.getChildBoundables.remove(childToRemove)
      return true
    }
    false
  }

  private def remove(searchBounds: Any, node: AbstractNode, item: Any): Boolean = { // first try removing item from this node
    var found                      = removeItem(node, item)
    if (found) return true
    var childToPrune: AbstractNode = null
    // next try removing item from lower nodes
    val i                          = node.getChildBoundables.iterator
    var shouldBreak                = false
    while (i.hasNext && !shouldBreak) {
      val childBoundable = i.next
      if (getIntersectsOp.intersects(childBoundable.getBounds, searchBounds)) {
        if (childBoundable.isInstanceOf[AbstractNode]) {
          found = remove(searchBounds, childBoundable.asInstanceOf[AbstractNode], item)
          // if found, record child for pruning and exit
          if (found) {
            childToPrune = childBoundable.asInstanceOf[AbstractNode]
            shouldBreak = true
          }
        }
      }
    }
    // prune child if possible
    if (childToPrune != null)
      if (childToPrune.getChildBoundables.isEmpty) node.getChildBoundables.remove(childToPrune)
    found
  }

  protected def boundablesAtLevel(level: Int): util.ArrayList[Boundable] = {
    val boundables = new util.ArrayList[Boundable]
    boundablesAtLevel(level, root, boundables)
    boundables
  }

  /**
   * @param level
   *   -1 to get items
   */
  private def boundablesAtLevel(
    level:      Int,
    top:        AbstractNode,
    boundables: util.Collection[Boundable]
  ): Unit = {
    Assert.isTrue(level > -2)
    if (top.getLevel == level) {
      boundables.add(top)
      return
    }
    val i = top.getChildBoundables.iterator
    while (i.hasNext) {
      val boundable = i.next
      if (boundable.isInstanceOf[AbstractNode])
        boundablesAtLevel(level, boundable.asInstanceOf[AbstractNode], boundables)
      else {
        Assert.isTrue(boundable.isInstanceOf[ItemBoundable])
        if (level == -1) boundables.add(boundable)
      }
    }
  }

  protected def getComparator: Comparator[Boundable]
}
