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

import org.locationtech.jts.geom.Location

/**
 * A Depth object records the topological depth of the sides of an Edge for up to two Geometries.
 *
 * @version 1.7
 */
object Depth {
  private val NULL_VALUE = -1

  def depthAtLocation(location: Int): Int = {
    if (location == Location.EXTERIOR) return 0
    if (location == Location.INTERIOR) return 1
    NULL_VALUE
  }
}

class Depth() { // initialize depth array to a sentinel value
  private val depth = Array.ofDim[Int](2, 3)
  var i             = 0
  while (i < 2) {
    var j = 0
    while (j < 3) {
      depth(i)(j) = Depth.NULL_VALUE
      j += 1
    }
    i += 1
  }

  def getDepth(geomIndex: Int, posIndex: Int): Int = depth(geomIndex)(posIndex)

  def setDepth(geomIndex: Int, posIndex: Int, depthValue: Int): Unit = depth(geomIndex)(posIndex) =
    depthValue

  def getLocation(geomIndex: Int, posIndex: Int): Int = {
    if (depth(geomIndex)(posIndex) <= 0) return Location.EXTERIOR
    Location.INTERIOR
  }

  def add(geomIndex: Int, posIndex: Int, location: Int): AnyVal = if (
    location == Location.INTERIOR
  ) {
    depth(geomIndex)(posIndex) += 1; depth(geomIndex)(posIndex) - 1
  }

  /**
   * A Depth object is null (has never been initialized) if all depths are null.
   */
  def isNull: Boolean = {
    var i = 0
    while (i < 2) {
      var j = 0
      while (j < 3) {
        if (depth(i)(j) != Depth.NULL_VALUE) return false
        j += 1
      }
      i += 1
    }
    true
  }

  def isNull(geomIndex: Int): Boolean = depth(geomIndex)(1) == Depth.NULL_VALUE

  def isNull(geomIndex: Int, posIndex: Int): Boolean =
    depth(geomIndex)(posIndex) == Depth.NULL_VALUE

  def add(lbl: Label): Unit = {
    var i = 0
    while (i < 2) {
      var j = 1
      while (j < 3) {
        val loc = lbl.getLocation(i, j)
        if (loc == Location.EXTERIOR || loc == Location.INTERIOR) { // initialize depth if it is null, otherwise add this location value
          if (isNull(i, j)) depth(i)(j) = Depth.depthAtLocation(loc)
          else depth(i)(j) += Depth.depthAtLocation(loc)
        }
        j += 1
      }
      i += 1
    }
  }

  def getDelta(geomIndex: Int): Int =
    depth(geomIndex)(Position.RIGHT) - depth(geomIndex)(Position.LEFT)

  /**
   * Normalize the depths for each geometry, if they are non-null. A normalized depth has depth
   * values in the set { 0, 1 }. Normalizing the depths involves reducing the depths by the same
   * amount so that at least one of them is 0. If the remaining value is &gt; 0, it is set to 1.
   */
  def normalize(): Unit = {
    var i = 0
    while (i < 2) {
      if (!isNull(i)) {
        var minDepth = depth(i)(1)
        if (depth(i)(2) < minDepth) minDepth = depth(i)(2)
        if (minDepth < 0) minDepth = 0
        var j        = 1
        while (j < 3) {
          var newValue = 0
          if (depth(i)(j) > minDepth) newValue = 1
          depth(i)(j) = newValue
          j += 1
        }
      }
      i += 1
    }
  }

  override def toString: String =
    "A: " + depth(0)(1) + "," + depth(0)(2) + " B: " + depth(1)(1) + "," + depth(1)(2)
}
