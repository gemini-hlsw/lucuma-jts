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
import org.locationtech.jts.util.Assert

/**
 * A node of an {link AbstractSTRtree}. A node is one of: <ul> <li>empty <li>an <i>interior node</i>
 * containing child {link AbstractNode}s <li>a <i>leaf node</i> containing data items ({link
 * ItemBoundable}s). </ul> A node stores the bounds of its children, and its level within the index
 * tree.
 *
 * @version 1.7
 */
@SerialVersionUID(6493722185909573708L)
abstract class AbstractNode(level: Int = 0)

/**
 * Default constructor required for serialization.
 */ extends Boundable with Serializable {
  private val childBoundables = new util.ArrayList[Boundable]
  private var bounds: AnyRef  = null
//  private var level = 0

  /**
   * Constructs an AbstractNode at the given level in the tree
   *
   * @param level
   *   0 if this node is a leaf, 1 if a parent of a leaf, and so on; the root node will have the
   *   highest level
   */
//  def this {
//    this()
//    this.level = level
//  }

  /**
   * Returns either child {link AbstractNode}s, or if this is a leaf node, real data (wrapped in
   * {link ItemBoundable}s).
   *
   * return a list of the children
   */
  def getChildBoundables: util.ArrayList[Boundable] = childBoundables

  /**
   * Returns a representation of space that encloses this Boundable, preferably not much bigger than
   * this Boundable's boundary yet fast to test for intersection with the bounds of other
   * Boundables. The class of object returned depends on the subclass of AbstractSTRtree.
   *
   * return an Envelope (for STRtrees), an Interval (for SIRtrees), or other object (for other
   * subclasses of AbstractSTRtree)
   * @see
   *   AbstractSTRtree.IntersectsOp
   */
  protected def computeBounds: AnyRef

  /**
   * Gets the bounds of this node
   *
   * return the object representing bounds in this index
   */
  override def getBounds: AnyRef = {
    if (bounds == null) bounds = computeBounds
    bounds
  }

  /**
   * Returns 0 if this node is a leaf, 1 if a parent of a leaf, and so on; the root node will have
   * the highest level
   *
   * return the node level
   */
  def getLevel: Int = level

  /**
   * Gets the count of the {link Boundable}s at this node.
   *
   * return the count of boundables at this node
   */
  def size: Int = childBoundables.size

  /**
   * Tests whether there are any {link Boundable}s at this node.
   *
   * return true if there are boundables at this node
   */
  def isEmpty: Boolean = childBoundables.isEmpty

  /**
   * Adds either an AbstractNode, or if this is a leaf node, a data object (wrapped in an
   * ItemBoundable)
   *
   * @param childBoundable
   *   the child to add
   */
  def addChildBoundable(childBoundable: Boundable): Boolean = {
    Assert.isTrue(bounds == null)
    childBoundables.add(childBoundable)
  }
}
