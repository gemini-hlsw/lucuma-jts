// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.index.kdtree

import org.locationtech.jts.geom.Coordinate

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
 * A node of a {@link KdTree}, which represents one or more points in the same location.
 *
 * @author
 *   dskea
 */
class KdNode {
  private var p: Coordinate = null
  private var data: Any     = null
  private var left: KdNode  = null
  private var right: KdNode = null
  private var count         = 0

  /**
   * Creates a new KdNode.
   *
   * @param _x
   *   coordinate of point
   * @param _y
   *   coordinate of point
   * @param data
   *   a data objects to associate with this node
   */
  def this(_x: Double, _y: Double, data: Any) = {
    this()
    p = new Coordinate(_x, _y)
    left = null
    right = null
    count = 1
    this.data = data
  }

  /**
   * Creates a new KdNode.
   *
   * @param p
   *   point location of new node
   * @param data
   *   a data objects to associate with this node
   */
  def this(p: Coordinate, data: Any) = {
    this()
    this.p = new Coordinate(p)
    left = null
    right = null
    count = 1
    this.data = data
  }

  /**
   * Returns the X coordinate of the node
   *
   * @return
   *   X coordinate of the node
   */
  def getX: Double = p.x

  /**
   * Returns the Y coordinate of the node
   *
   * @return
   *   Y coordinate of the node
   */
  def getY: Double = p.y

  /**
   * Returns the location of this node
   *
   * @return
   *   p location of this node
   */
  def getCoordinate: Coordinate = p

  /**
   * Gets the user data object associated with this node.
   *
   * @return
   */
  def getData: Any = data

  /**
   * Returns the left node of the tree
   *
   * @return
   *   left node
   */
  def getLeft: KdNode = left

  /**
   * Returns the right node of the tree
   *
   * @return
   *   right node
   */
  def getRight: KdNode = right

  // Increments counts of points at this location
  private[kdtree] def increment(): Unit =
    count = count + 1

  /**
   * Returns the number of inserted points that are coincident at this location.
   *
   * @return
   *   number of inserted points that this node represents
   */
  def getCount: Int = count

  /**
   * Tests whether more than one point with this value have been inserted (up to the tolerance)
   *
   * @return
   *   true if more than one point have been inserted with this value
   */
  def isRepeated: Boolean = count > 1

  // Sets left node value
  private[kdtree] def setLeft(_left: KdNode): Unit =
    left = _left

  // Sets right node value
  private[kdtree] def setRight(_right: KdNode): Unit =
    right = _right
}
