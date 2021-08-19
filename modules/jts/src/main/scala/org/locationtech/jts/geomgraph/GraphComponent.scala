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
import org.locationtech.jts.geom.IntersectionMatrix
import org.locationtech.jts.util.Assert

/**
 * A GraphComponent is the parent class for the objects' that form a graph. Each GraphComponent can
 * carry a Label.
 *
 * @version 1.7
 */
abstract class GraphComponent(var label: Label) {

  /**
   * isInResult indicates if this component has already been included in the result
   */
  private var visInResult   = false
  private var visCovered    = false
  private var visCoveredSet = false
  private var visVisited    = false

  def this() = {
    this(null)
  }

  def getLabel: Label = label

  def setLabel(label: Label): Unit = this.label = label

  def setInResult(isInResult: Boolean): Unit = this.visInResult = isInResult

  def isInResult: Boolean = visInResult

  def setCovered(isCovered: Boolean): Unit = {
    this.visCovered = isCovered
    this.visCoveredSet = true
  }

  def isCovered: Boolean = visCovered

  def isCoveredSet: Boolean = visCoveredSet

  def isVisited: Boolean = visVisited

  def setVisited(isVisited: Boolean): Unit = this.visVisited = isVisited

  /**
   * return a coordinate in this component (or null, if there are none)
   */
  def getCoordinate: Coordinate

  /**
   * compute the contribution to an IM for this component
   */
  protected def computeIM(im: IntersectionMatrix): Unit

  /**
   * An isolated component is one that does not intersect or touch any other component. This is the
   * case if the label has valid locations for only a single Geometry.
   *
   * return true if this component is isolated
   */
  def isIsolated: Boolean

  /**
   * Update the IM with the contribution for this component. A component only contributes if it has
   * a labelling for both parent geometries
   */
  def updateIM(im: IntersectionMatrix): Unit = {
    Assert.isTrue(label.getGeometryCount >= 2, "found partial label")
    computeIM(im)
  }
}
