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

import java.io.PrintStream
import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.util.Assert

/**
 * Models the end of an edge incident on a node. EdgeEnds have a direction determined by the
 * direction of the ray from the initial point to the next point. EdgeEnds are comparable under the
 * ordering "a has a greater angle with the x-axis than b". This ordering is used to sort EdgeEnds
 * around a node.
 *
 * @version 1.7
 */
class EdgeEnd protected (
  var edge: Edge // the parent edge of this edge end
) extends Comparable[EdgeEnd] {
  protected var label: Label = null
  private var node: Node     = null // the node this edge end originates at
  private var p0: Coordinate = null
  private var p1: Coordinate = null // points of initial line segment
  private var dx             = .0
  private var dy             = .0   // the direction vector for this edge from its starting point
  private var quadrant       = 0

  def this(edge: Edge, p0: Coordinate, p1: Coordinate, label: Label) = {
    this(edge)
    init(p0, p1)
    this.label = label
  }

  def this(edge: Edge, p0: Coordinate, p1: Coordinate) = {
    this(edge, p0, p1, null)
  }

  protected def init(p0: Coordinate, p1: Coordinate): Unit = {
    this.p0 = p0
    this.p1 = p1
    dx = p1.x - p0.x
    dy = p1.y - p0.y
    quadrant = Quadrant.quadrant(dx, dy)
    Assert.isTrue(!(dx == 0 && dy == 0), "EdgeEnd with identical endpoints found")
  }

  def getEdge: Edge = edge

  def getLabel: Label = label

  def getCoordinate: Coordinate = p0

  def getDirectedCoordinate: Coordinate = p1

  def getQuadrant: Int = quadrant

  def getDx: Double = dx

  def getDy: Double = dy

  def setNode(node: Node): Unit = this.node = node

  def getNode: Node = node

  override def compareTo(e: EdgeEnd): Int =
    compareDirection(e)

  /**
   * Implements the total order relation: <p> a has a greater angle with the positive x-axis than b
   * <p> Using the obvious algorithm of simply computing the angle is not robust, since the angle
   * calculation is obviously susceptible to roundoff. A robust algorithm is:
   *   - first compare the quadrant. If the quadrants are different, it it trivial to determine
   *     which vector is "greater".
   *   - if the vectors lie in the same quadrant, the computeOrientation function can be used to
   *     decide the relative orientation of the vectors.
   */
  def compareDirection(e: EdgeEnd): Int = {
    if (dx == e.dx && dy == e.dy) return 0
    // if the rays are in different quadrants, determining the ordering is trivial
    if (quadrant > e.quadrant) return 1
    if (quadrant < e.quadrant) return -1
    // vectors are in the same quadrant - check relative orientation of direction vectors
    // this is > e if it is CCW of e
    Orientation.index(e.p0, e.p1, p1)
  }

  def computeLabel(boundaryNodeRule: BoundaryNodeRule): Unit = {
    // subclasses should override this if they are using labels
  }

  def print(out: PrintStream): Unit = {
    val angle      = Math.atan2(dy, dx)
    val className  = getClass.getName
    val lastDotPos = className.lastIndexOf('.')
    val name       = className.substring(lastDotPos + 1)
    out.print("  " + name + ": " + p0 + " - " + p1 + " " + quadrant + ":" + angle + "   " + label)
  }

  override def toString: String = {
    val angle      = Math.atan2(dy, dx)
    val className  = getClass.getName
    val lastDotPos = className.lastIndexOf('.')
    val name       = className.substring(lastDotPos + 1)
    "  " + name + ": " + p0 + " - " + p1 + " " + quadrant + ":" + angle + "   " + label
  }
}
