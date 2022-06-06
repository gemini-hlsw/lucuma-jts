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

import org.locationtech.jts.noding.OrientedCoordinateArray

import java.io.PrintStream
import java.util
import scala.collection.mutable

/**
 * A EdgeList is a list of Edges. It supports locating edges that are pointwise equals to a target
 * edge.
 *
 * @version 1.7
 */
class EdgeList() {
  private val edges = new util.ArrayList[Edge]

  /**
   * An index of the edges, for fast lookup.
   */
  private val ocaMap = mutable.TreeMap.empty[OrientedCoordinateArray, Edge]

  /**
   * Insert an edge unless it is already in the list
   */
  def add(e: Edge): Edge = {
    edges.add(e)
    val oca = new OrientedCoordinateArray(e.getCoordinates)
    ocaMap.put(oca, e).orNull
  }

  def addAll(edgeColl: util.Collection[Edge]): Unit = {
    val i = edgeColl.iterator
    while (i.hasNext) add(i.next)
  }

  def getEdges: util.ArrayList[Edge] = edges

  /**
   * If there is an edge equal to e already in the list, return it. Otherwise return null.
   *
   * return equal edge, if there is one already in the list null otherwise
   */
  def findEqualEdge(e: Edge): Edge = {
    val oca       = new OrientedCoordinateArray(e.getCoordinates)
    // will return null if no edge matches
    val matchEdge = ocaMap.get(oca)
    matchEdge.orNull
  }

  def iterator: util.Iterator[Edge] = edges.iterator

  def get(i: Int): Edge = edges.get(i)

  /**
   * If the edge e is already in the list, return its index.
   *
   * return index, if e is already in the list
   * -1 otherwise
   */
  def findEdgeIndex(e: Edge): Int = {
    var i = 0
    while (i < edges.size) {
      if (edges.get(i) == e) return i
      i += 1; i - 1
    }
    -1
  }

  def print(out: PrintStream): Unit = {
    out.print("MULTILINESTRING ( ")
    var j = 0
    while (j < edges.size) {
      val e   = edges.get(j)
      if (j > 0) out.print(",")
      out.print("(")
      val pts = e.getCoordinates
      var i   = 0
      while (i < pts.length) {
        if (i > 0) out.print(",")
        out.print(s"${pts(i).x} ${pts(i).y}")
        i += 1
      }
      out.println(")")
      j += 1
    }
    out.print(")  ")
  }
}
