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

import org.locationtech.jts.geom.Location

/**
 * A <code>Label</code> indicates the topological relationship of a component of a topology graph to
 * a given <code>Geometry</code>. This class supports labels for relationships to two
 * <code>Geometry</code>s, which is sufficient for algorithms for binary operations. <P> Topology
 * graphs support the concept of labeling nodes and edges in the graph. The label of a node or edge
 * specifies its topological relationship to one or more geometries. (In fact, since JTS operations
 * have only two arguments labels are required for only two geometries). A label for a node or edge
 * has one or two elements, depending on whether the node or edge occurs in one or both of the input
 * <code>Geometry</code>s. Elements contain attributes which categorize the topological location of
 * the node or edge relative to the parent <code>Geometry</code>; that is, whether the node or edge
 * is in the interior, boundary or exterior of the <code>Geometry</code>. Attributes have a value
 * from the set <code>{Interior, Boundary, Exterior}</code>. In a node each element has a single
 * attribute <code>&lt;On&gt;</code>. For an edge each element has a triplet of attributes
 * <code>&lt;Left, On, Right&gt;</code>. <P> It is up to the client code to associate the 0 and 1
 * <code>TopologyLocation</code>s with specific geometries.
 *
 * @version 1.7
 */
object Label { // converts a Label to a Line label (that is, one with no side Locations)
  def toLineLabel(label: Label): Label = {
    val lineLabel = new Label(Location.NONE)
    var i         = 0
    while (i < 2) {
      lineLabel.setLocation(i, label.getLocation(i))
      i += 1
    }
    lineLabel
  }
}

class Label() {
  private[geomgraph] val elt = new Array[TopologyLocation](2)

  /**
   * Construct a Label with a single location for both Geometries. Initialize the locations to Null
   */
  def this(onLoc: Int) = {
    this()
    elt(0) = new TopologyLocation(onLoc)
    elt(1) = new TopologyLocation(onLoc)
  }

  /**
   * Construct a Label with a single location for both Geometries. Initialize the location for the
   * Geometry index.
   */
  def this(geomIndex: Int, onLoc: Int) = {
    this()
    elt(0) = new TopologyLocation(Location.NONE)
    elt(1) = new TopologyLocation(Location.NONE)
    elt(geomIndex).setLocation(onLoc)
  }

  /**
   * Construct a Label with On, Left and Right locations for both Geometries. Initialize the
   * locations for both Geometries to the given values.
   */
  def this(onLoc: Int, leftLoc: Int, rightLoc: Int) = {
    this()
    elt(0) = new TopologyLocation(onLoc, leftLoc, rightLoc)
    elt(1) = new TopologyLocation(onLoc, leftLoc, rightLoc)
  }

  /**
   * Construct a Label with On, Left and Right locations for both Geometries. Initialize the
   * locations for the given Geometry index.
   */
  def this(geomIndex: Int, onLoc: Int, leftLoc: Int, rightLoc: Int) = {
    this()
    elt(0) = new TopologyLocation(Location.NONE, Location.NONE, Location.NONE)
    elt(1) = new TopologyLocation(Location.NONE, Location.NONE, Location.NONE)
    elt(geomIndex).setLocations(onLoc, leftLoc, rightLoc)
  }

  /**
   * Construct a Label with the same values as the argument Label.
   */
  def this(lbl: Label) = {
    this()
    elt(0) = new TopologyLocation(lbl.elt(0))
    elt(1) = new TopologyLocation(lbl.elt(1))
  }

  def flip(): Unit = {
    elt(0).flip()
    elt(1).flip()
  }

  def getLocation(geomIndex: Int, posIndex: Int): Int = elt(geomIndex).get(posIndex)

  def getLocation(geomIndex: Int): Int = elt(geomIndex).get(Position.ON)

  def setLocation(geomIndex: Int, posIndex: Int, location: Int): Unit =
    elt(geomIndex).setLocation(posIndex, location)

  def setLocation(geomIndex: Int, location: Int): Unit =
    elt(geomIndex).setLocation(Position.ON, location)

  def setAllLocations(geomIndex: Int, location: Int): Unit =
    elt(geomIndex).setAllLocations(location)

  def setAllLocationsIfNull(geomIndex: Int, location: Int): Unit =
    elt(geomIndex).setAllLocationsIfNull(location)

  def setAllLocationsIfNull(location: Int): Unit = {
    setAllLocationsIfNull(0, location)
    setAllLocationsIfNull(1, location)
  }

  /**
   * Merge this label with another one. Merging updates any null attributes of this label with the
   * attributes from lbl
   */
  def merge(lbl: Label): Unit = {
    var i = 0
    while (i < 2) {
      if (elt(i) == null && lbl.elt(i) != null) elt(i) = new TopologyLocation(lbl.elt(i))
      else elt(i).merge(lbl.elt(i))
      i += 1
    }
  }

  def getGeometryCount: Int = {
    var count = 0
    if (!elt(0).isNull) {
      count += 1
    }
    if (!elt(1).isNull) {
      count += 1
    }
    count
  }

  def isNull(geomIndex: Int): Boolean = elt(geomIndex).isNull

  def isAnyNull(geomIndex: Int): Boolean = elt(geomIndex).isAnyNull

  def isArea: Boolean = elt(0).isArea || elt(1).isArea

  def isArea(geomIndex: Int): Boolean =
    /*  Testing
       if (elt[0].getLocations().length != elt[1].getLocations().length) {
         System.out.println(this);
       }
     */ elt(geomIndex).isArea

  def isLine(geomIndex: Int): Boolean = elt(geomIndex).isLine

  def isEqualOnSide(lbl: Label, side: Int): Boolean =
    this.elt(0).isEqualOnSide(lbl.elt(0), side) && this.elt(1).isEqualOnSide(lbl.elt(1), side)

  def allPositionsEqual(geomIndex: Int, loc: Int): Boolean = elt(geomIndex).allPositionsEqual(loc)

  /**
   * Converts one GeometryLocation to a Line location
   */
  def toLine(geomIndex: Int): Unit =
    if (elt(geomIndex).isArea) elt(geomIndex) = new TopologyLocation(elt(geomIndex).location(0))

  override def toString: String = {
    val buf = new StringBuffer
    if (elt(0) != null) {
      buf.append("A:")
      buf.append(elt(0).toString)
    }
    if (elt(1) != null) {
      buf.append(" B:")
      buf.append(elt(1).toString)
    }
    buf.toString
  }
}
