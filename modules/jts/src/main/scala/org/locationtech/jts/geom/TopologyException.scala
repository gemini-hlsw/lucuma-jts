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
package org.locationtech.jts.geom

/**
 * Indicates an invalid or inconsistent topological situation encountered during processing
 *
 * @version 1.7
 */
object TopologyException {
  def msgWithCoord(msg: String, pt: Coordinate): String = {
    if (pt != null) return msg + " [ " + pt + " ]"
    msg
  }
}

class TopologyException(msg: String, pt: Coordinate) extends RuntimeException(msg) {
//  private var pt = null

  def this(msg: String) = this(msg, null)

//  def this {
//    this()
//    super (TopologyException.msgWithCoord(msg, pt))
//    this.pt = new Coordinate(pt)
//  }

  def getCoordinate: Coordinate = pt
}
