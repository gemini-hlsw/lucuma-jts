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
 */ /*
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
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

/**
 * A framework for processes which transform an input {link Geometry} into an output {link
 * Geometry}, possibly changing its structure and type(s). This class is a framework for
 * implementing subclasses which perform transformations on various different Geometry subclasses.
 * It provides an easy way of applying specific transformations to given geometry types, while
 * allowing unhandled types to be simply copied. Also, the framework ensures that if subcomponents
 * change type the parent geometries types change appropriately to maintain valid structure.
 * Subclasses will override whichever <code>transformX</code> methods they need to to handle
 * particular Geometry types. <p> A typically usage would be a transformation class that transforms
 * <tt>Polygons</tt> into <tt>Polygons</tt>, <tt>LineStrings</tt> or <tt>Points</tt>, depending on
 * the geometry of the input (For instance, a simplification operation). This class would likely
 * need to override the {link #transformMultiPolygon(MultiPolygon, Geometry)} method to ensure that
 * if input Polygons change type the result is a <tt>GeometryCollection</tt>, not a
 * <tt>MultiPolygon</tt>. <p> The default behaviour of this class is simply to recursively transform
 * each Geometry component into an identical object by deep copying down to the level of, but not
 * including, coordinates. <p> All <code>transformX</code> methods may return <code>null</code>, to
 * avoid creating empty or invalid geometry objects. This will be handled correctly by the
 * transformer. <code>transform<i>XXX</i></code> methods should always return valid geometry - if
 * they cannot do this they should return <code>null</code> (for instance, it may not be possible
 * for a transformLineString implementation to return at least two points - in this case, it should
 * return <code>null</code>). The {link #transform(Geometry)} method itself will always return a
 * non-null Geometry object (but this may be empty).
 *
 * @version 1.7
 * @see
 *   GeometryEditor
 */
class GeometryTransformer() {

  /**
   * Possible extensions: getParent() method to return immediate parent e.g. of LinearRings in
   * Polygons
   */
  private var inputGeom: Geometry        = null
  protected var factory: GeometryFactory = null

  /**
   * <code>true</code> if empty geometries should not be included in the result
   */
  private val pruneEmptyGeometry = true

  /**
   * <code>true</code> if a homogenous collection result from a {link GeometryCollection} should
   * still be a general GeometryCollection
   */
  private val preserveGeometryCollectionType = true

  /**
   * <code>true</code> if the output from a collection argument should still be a collection
   */
//  private val preserveCollections = false
  /**
   * <code>true</code> if the type of the input should be preserved
   */
  private val preserveType = false

  /**
   * Utility function to make input geometry available
   *
   * return the input geometry
   */
  def getInputGeometry: Geometry = inputGeom

  final def transform(inputGeom: Geometry): Geometry = {
    this.inputGeom = inputGeom
    this.factory = inputGeom.getFactory
    if (inputGeom.isInstanceOf[Point]) return transformPoint(inputGeom.asInstanceOf[Point], null)
    if (inputGeom.isInstanceOf[MultiPoint])
      return transformMultiPoint(inputGeom.asInstanceOf[MultiPoint], null)
    if (inputGeom.isInstanceOf[LinearRing])
      return transformLinearRing(inputGeom.asInstanceOf[LinearRing], null)
    if (inputGeom.isInstanceOf[LineString])
      return transformLineString(inputGeom.asInstanceOf[LineString], null)
    if (inputGeom.isInstanceOf[MultiLineString])
      return transformMultiLineString(inputGeom.asInstanceOf[MultiLineString], null)
    if (inputGeom.isInstanceOf[Polygon])
      return transformPolygon(inputGeom.asInstanceOf[Polygon], null)
    if (inputGeom.isInstanceOf[MultiPolygon])
      return transformMultiPolygon(inputGeom.asInstanceOf[MultiPolygon], null)
    if (inputGeom.isInstanceOf[GeometryCollection])
      return transformGeometryCollection(inputGeom.asInstanceOf[GeometryCollection], null)
    throw new IllegalArgumentException("Unknown Geometry subtype: " + inputGeom.getClass.getName)
  }

  /**
   * Convenience method which provides standard way of creating a {link CoordinateSequence}
   *
   * @param coords
   *   the coordinate array to copy return a coordinate sequence for the array
   */
  final protected def createCoordinateSequence(coords: Array[Coordinate]): CoordinateSequence =
    factory.getCoordinateSequenceFactory.create(coords)

  /**
   * Convenience method which provides a standard way of copying {link CoordinateSequence}s
   *
   * @param seq
   *   the sequence to copy return a deep copy of the sequence
   */
  final protected def copy(seq: CoordinateSequence): CoordinateSequence = seq.copy

  // /**
  //  * Transforms a {link CoordinateSequence}.
  //  * This method should always return a valid coordinate list for
  //  * the desired result type.  (E.g. a coordinate list for a LineString
  //  * must have 0 or at least 2 points).
  //  * If this is not possible, return an empty sequence -
  //  * this will be pruned out.
  //  *
  //  * @param coords the coordinates to transform
  //  * @param parent the parent geometry
  //  * return the transformed coordinates
  //  */
  protected def transformCoordinates(
    coords: CoordinateSequence,
    parent: Geometry
  ): CoordinateSequence = copy(coords)

  protected def transformPoint(geom: Point, parent: Geometry): Point =
    factory.createPoint(transformCoordinates(geom.getCoordinateSequence, geom))

  protected def transformMultiPoint(geom: MultiPoint, parent: Geometry): Geometry = {
    val transGeomList = new util.ArrayList[Geometry]
    var i             = 0
    while (i < geom.getNumGeometries) {
      val transformGeom = transformPoint(geom.getGeometryN(i).asInstanceOf[Point], geom)
      if (transformGeom != null) {
        if (!transformGeom.isEmpty) {
          transGeomList.add(transformGeom)
        }
      }
      i += 1
    }
    factory.buildGeometry(transGeomList)
  }

  // /**
  //  * Transforms a LinearRing.
  //  * The transformation of a LinearRing may result in a coordinate sequence
  //  * which does not form a structurally valid ring (i.e. a degenerate ring of 3 or fewer points).
  //  * In this case a LineString is returned.
  //  * Subclasses may wish to override this method and check for this situation
  //  * (e.g. a subclass may choose to eliminate degenerate linear rings)
  //  *
  //  * @param geom   the ring to simplify
  //  * @param parent the parent geometry
  //  * return a LinearRing if the transformation resulted in a structurally valid ring
  //  * return a LineString if the transformation caused the LinearRing to collapse to 3 or fewer points
  //  */
  protected def transformLinearRing(geom: LinearRing, parent: Geometry): Geometry = {
    val seq     = transformCoordinates(geom.getCoordinateSequence, geom)
    if (seq == null) return factory.createLinearRing(null.asInstanceOf[CoordinateSequence])
    val seqSize = seq.size
    // ensure a valid LinearRing
    if (seqSize > 0 && seqSize < 4 && !preserveType) return factory.createLineString(seq)
    factory.createLinearRing(seq)
  }

  /**
   * Transforms a {link LineString} geometry.
   *
   * @param geom
   * @param parent
   *   return
   */
  protected def transformLineString(
    geom:   LineString,
    parent: Geometry
  ): LineString = // should check for 1-point sequences and downgrade them to points
    factory.createLineString(transformCoordinates(geom.getCoordinateSequence, geom))

  protected def transformMultiLineString(geom: MultiLineString, parent: Geometry): Geometry = {
    val transGeomList = new util.ArrayList[Geometry]
    var i             = 0
    while (i < geom.getNumGeometries) {
      val transformGeom = transformLineString(geom.getGeometryN(i).asInstanceOf[LineString], geom)
      if (transformGeom != null) {
        if (!transformGeom.isEmpty) {
          transGeomList.add(transformGeom)
        }
      }
      i += 1
    }
    factory.buildGeometry(transGeomList)
  }

  protected def transformPolygon(geom: Polygon, parent: Geometry): Geometry = {
    var isAllValidLinearRings = true
    val shell                 = transformLinearRing(geom.getExteriorRing, geom)
    if (shell == null || !shell.isInstanceOf[LinearRing] || shell.isEmpty)
      isAllValidLinearRings = false
    val holes                 = new util.ArrayList[Geometry]
    var i                     = 0
    while (i < geom.getNumInteriorRing) {
      val hole = transformLinearRing(geom.getInteriorRingN(i), geom)
      if (!(hole == null || hole.isEmpty)) {
        if (!hole.isInstanceOf[LinearRing]) isAllValidLinearRings = false
        holes.add(hole)
      }
      i += 1
    }
    if (isAllValidLinearRings)
      return factory.createPolygon(
        shell.asInstanceOf[LinearRing],
        holes.toArray(Array.empty[LinearRing]).asInstanceOf[Array[LinearRing]]
      )
    else {
      val components = new util.ArrayList[Geometry]
      if (shell != null) components.add(shell)
      components.addAll(holes)
      factory.buildGeometry(components)
    }
  }

  protected def transformMultiPolygon(geom: MultiPolygon, parent: Geometry): Geometry = {
    val transGeomList = new util.ArrayList[Geometry]
    var i             = 0
    while (i < geom.getNumGeometries) {
      val transformGeom = transformPolygon(geom.getGeometryN(i).asInstanceOf[Polygon], geom)
      if (transformGeom != null) {
        if (!transformGeom.isEmpty) {
          transGeomList.add(transformGeom)
        }
      }
      i += 1
    }
    factory.buildGeometry(transGeomList)
  }

  protected def transformGeometryCollection(
    geom:   GeometryCollection,
    parent: Geometry
  ): Geometry = {
    val transGeomList = new util.ArrayList[Geometry]
    var i             = 0
    while (i < geom.getNumGeometries) {
      val transformGeom = transform(geom.getGeometryN(i))
      if (transformGeom != null) {
        if (!(pruneEmptyGeometry && transformGeom.isEmpty)) {
          transGeomList.add(transformGeom)
        }
      }
      i += 1
    }
    if (preserveGeometryCollectionType)
      return factory.createGeometryCollection(GeometryFactory.toGeometryArray(transGeomList))
    factory.buildGeometry(transGeomList)
  }
}
