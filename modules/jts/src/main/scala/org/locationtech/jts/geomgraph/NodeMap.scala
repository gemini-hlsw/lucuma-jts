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
package org.locationtech.jts.geomgraph

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Location

import java.io.PrintStream
import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * A map of nodes, indexed by the coordinate of the node
 *
 * @version 1.7
 */
class NodeMap(var nodeFact: NodeFactory) {
  // Map nodeMap = new HashMap();
  private[geomgraph] val nodeMap = mutable.TreeMap.empty[Coordinate, Node]

  /**
   * This method expects that a node has a coordinate value.
   */
  def addNode(coord: Coordinate): Node = {
    var node = nodeMap.get(coord).orNull
    if (node == null) {
      node = nodeFact.createNode(coord)
      nodeMap.put(coord, node)
    }
    node
  }

  def addNode(n: Node): Node = {
    val node = nodeMap.get(n.getCoordinate).orNull
    if (node == null) {
      nodeMap.put(n.getCoordinate, n)
      return n
    }
    node.mergeLabel(n)
    node
  }

  /**
   * Adds a node for the start point of this EdgeEnd (if one does not already exist in this map).
   * Adds the EdgeEnd to the (possibly new) node.
   */
  def add(e: EdgeEnd): Unit = {
    val p = e.getCoordinate
    val n = addNode(p)
    n.add(e)
  }

  /**
   * return the node if found; null otherwise
   */
  def find(coord: Coordinate): Node = nodeMap.get(coord).orNull

  def iterator: util.Iterator[Node] = nodeMap.values.iterator.asJava

  def values: util.Collection[Node] = nodeMap.values.toList.asJavaCollection

  def getBoundaryNodes(geomIndex: Int): util.ArrayList[Node] = {
    val bdyNodes = new util.ArrayList[Node]
    val i        = iterator
    while (i.hasNext) {
      val node = i.next
      if (node.getLabel.getLocation(geomIndex) == Location.BOUNDARY) bdyNodes.add(node)
    }
    bdyNodes
  }

  def print(out: PrintStream): Unit = {
    val it = iterator
    while (it.hasNext) {
      val n = it.next
      n.print(out)
    }
  }
}
