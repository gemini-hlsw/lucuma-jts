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
package org.locationtech.jts.geom.util

import java.util
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryComponentFilter
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing

/**
 * Extracts all the 1-dimensional ({link LineString}) components from a {link Geometry}.
 * For polygonal geometries, this will extract all the component {link LinearRing}s.
 * If desired, <code>LinearRing</code>s can be forced to be returned as <code>LineString</code>s.
 *
 * @version 1.7
 */
object LinearComponentExtracter {

  /**
   * Extracts the linear components from a single {link Geometry}
   * and adds them to the provided {link Collection}.
   *
   * @param geoms the collection of geometries from which to extract linear components
   * @param lines the collection to add the extracted linear components to
   * return the collection of linear components (LineStrings or LinearRings)
   */
  def getLines(
    geoms: util.Collection[Geometry],
    lines: util.Collection[Geometry]
  ): util.Collection[Geometry] = {
    val i = geoms.iterator
    while (i.hasNext) {
      val g = i.next
      getLines(g, lines)
    }
    lines
  }

  /**
   * Extracts the linear components from a single {link Geometry}
   * and adds them to the provided {link Collection}.
   *
   * @param geoms             the Collection of geometries from which to extract linear components
   * @param lines             the collection to add the extracted linear components to
   * @param forceToLineString true if LinearRings should be converted to LineStrings
   * return the collection of linear components (LineStrings or LinearRings)
   */
  def getLines(
    geoms:             util.Collection[Geometry],
    lines:             util.Collection[Geometry],
    forceToLineString: Boolean
  ): util.Collection[Geometry] = {
    val i = geoms.iterator
    while (i.hasNext) {
      val g = i.next
      getLines(g, lines, forceToLineString)
    }
    lines
  }

  /**
   * Extracts the linear components from a single {link Geometry}
   * and adds them to the provided {link Collection}.
   *
   * @param geom  the geometry from which to extract linear components
   * @param lines the Collection to add the extracted linear components to
   * return the Collection of linear components (LineStrings or LinearRings)
   */
  def getLines(geom: Geometry, lines: util.Collection[Geometry]): util.Collection[Geometry] = {
    if (geom.isInstanceOf[LineString]) lines.add(geom)
    else geom.applyF(new LinearComponentExtracter(lines))
    lines
  }

  /**
   * Extracts the linear components from a single {link Geometry}
   * and adds them to the provided {link Collection}.
   *
   * @param geom              the geometry from which to extract linear components
   * @param lines             the Collection to add the extracted linear components to
   * @param forceToLineString true if LinearRings should be converted to LineStrings
   * return the Collection of linear components (LineStrings or LinearRings)
   */
  def getLines(
    geom:              Geometry,
    lines:             util.Collection[Geometry],
    forceToLineString: Boolean
  ): util.Collection[Geometry] = {
    geom.applyF(new LinearComponentExtracter(lines, forceToLineString))
    lines
  }

  /**
   * Extracts the linear components from a single geometry.
   * If more than one geometry is to be processed, it is more
   * efficient to create a single {link LinearComponentExtracter} instance
   * and pass it to multiple geometries.
   *
   * @param geom the geometry from which to extract linear components
   * return the list of linear components
   */
  def getLines(geom: Geometry): util.ArrayList[Geometry] = getLines(geom, false)

  /**
   * Extracts the linear components from a single geometry.
   * If more than one geometry is to be processed, it is more
   * efficient to create a single {link LinearComponentExtracter} instance
   * and pass it to multiple geometries.
   *
   * @param geom              the geometry from which to extract linear components
   * @param forceToLineString true if LinearRings should be converted to LineStrings
   * return the list of linear components
   */
  def getLines(geom: Geometry, forceToLineString: Boolean): util.ArrayList[Geometry] = {
    val lines = new util.ArrayList[Geometry]
    geom.applyF(new LinearComponentExtracter(lines, forceToLineString))
    lines
  }

  /**
   * Extracts the linear components from a single {link Geometry}
   * and returns them as either a {link LineString} or {link MultiLineString}.
   *
   * @param geom the geometry from which to extract
   * return a linear geometry
   */
  def getGeometry(geom: Geometry): Geometry = geom.getFactory.buildGeometry(getLines(geom))

  /**
   * Extracts the linear components from a single {link Geometry}
   * and returns them as either a {link LineString} or {link MultiLineString}.
   *
   * @param geom              the geometry from which to extract
   * @param forceToLineString true if LinearRings should be converted to LineStrings
   * return a linear geometry
   */
  def getGeometry(geom: Geometry, forceToLineString: Boolean): Geometry =
    geom.getFactory.buildGeometry(getLines(geom, forceToLineString))
}

class LinearComponentExtracter(lines: util.Collection[Geometry], var isForcedToLineString: Boolean)
    extends GeometryComponentFilter {

  /**
   * Constructs a LineExtracterFilter with a list in which to store LineStrings found.
   */
  def this(lines: util.Collection[Geometry]) = {
    this(lines, false)
  }

  /**
   * Indicates that LinearRing components should be
   * converted to pure LineStrings.
   *
   * @param isForcedToLineString true if LinearRings should be converted to LineStrings
   */
  def setForceToLineString(isForcedToLineString: Boolean): Unit = this.isForcedToLineString =
    isForcedToLineString

  override def filter(geom: Geometry): Unit = {
    if (isForcedToLineString && geom.isInstanceOf[LinearRing]) {
      val line =
        geom.getFactory.createLineString(geom.asInstanceOf[LinearRing].getCoordinateSequence)
      lines.add(line)
      return
    }
    // if not being forced, and this is a linear component
    if (geom.isInstanceOf[LineString]) lines.add(geom)
    ()
    // else this is not a linear component, so skip it
  }
}
