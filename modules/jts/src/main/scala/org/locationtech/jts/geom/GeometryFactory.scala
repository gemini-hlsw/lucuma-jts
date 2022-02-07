// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.geom

import java.io.Serializable
import java.util
import org.locationtech.jts.geom.impl.CoordinateArraySequenceFactory
import org.locationtech.jts.geom.util.GeometryEditor
import org.locationtech.jts.util.Assert

/**
 * Supplies a set of utility methods for building Geometry objects from lists of Coordinates. <p>
 * Note that the factory constructor methods do <b>not</b> change the input coordinates in any way.
 * In particular, they are not rounded to the supplied <tt>PrecisionModel</tt>. It is assumed that
 * input Coordinates meet the given precision. <p> Instances of this class are thread-safe.
 *
 * @version 1.7
 */
@SerialVersionUID(-6820524753094095635L)
object GeometryFactory {
  def createPointFromInternalCoord(coord: Coordinate, exemplar: Geometry): Point = {
    exemplar.getPrecisionModel.makePrecise(coord)
    exemplar.getFactory.createPoint(coord)
  }

  private def getDefaultCoordinateSequenceFactory = CoordinateArraySequenceFactory.instance

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param points
   *   the <code>List</code> of Points to convert return the <code>List</code> in array format
   */
  def toPointArray(points: util.Collection[_]): Array[Point] = {
    val pointArray = new Array[Point](points.size)
    points.toArray(pointArray)
  }

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param geometries
   *   the list of <code>Geometry's</code> to convert return the <code>List</code> in array format
   */
  def toGeometryArray(geometries: util.Collection[_]): Array[Geometry] = {
    if (geometries == null) return null
    val geometryArray = new Array[Geometry](geometries.size)
    geometries.toArray(geometryArray)
  }

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param linearRings
   *   the <code>List</code> of LinearRings to convert return the <code>List</code> in array format
   */
  def toLinearRingArray(linearRings: util.Collection[_]): Array[LinearRing] = {
    val linearRingArray = new Array[LinearRing](linearRings.size)
    linearRings.toArray(linearRingArray)
  }

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param lineStrings
   *   the <code>List</code> of LineStrings to convert return the <code>List</code> in array format
   */
  def toLineStringArray(lineStrings: util.Collection[_]): Array[LineString] = {
    val lineStringArray = new Array[LineString](lineStrings.size)
    lineStrings.toArray(lineStringArray)
  }

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param polygons
   *   the <code>List</code> of Polygons to convert return the <code>List</code> in array format
   */
  def toPolygonArray(polygons: util.Collection[_]): Array[Polygon] = {
    val polygonArray = new Array[Polygon](polygons.size)
    polygons.toArray(polygonArray)
  }

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param multiPolygons
   *   the <code>List</code> of MultiPolygons to convert return the <code>List</code> in array
   *   format
   */
  def toMultiPolygonArray(multiPolygons: util.Collection[_]): Array[MultiPolygon] = {
    val multiPolygonArray = new Array[MultiPolygon](multiPolygons.size)
    multiPolygons.toArray(multiPolygonArray)
  }

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param multiLineStrings
   *   the <code>List</code> of MultiLineStrings to convert return the <code>List</code> in array
   *   format
   */
  def toMultiLineStringArray(multiLineStrings: util.Collection[_]): Array[MultiLineString] = {
    val multiLineStringArray = new Array[MultiLineString](multiLineStrings.size)
    multiLineStrings.toArray(multiLineStringArray)
  }

  /**
   * Converts the <code>List</code> to an array.
   *
   * @param multiPoints
   *   the <code>List</code> of MultiPoints to convert return the <code>List</code> in array format
   */
  def toMultiPointArray(multiPoints: util.Collection[_]): Array[MultiPoint] = {
    val multiPointArray = new Array[MultiPoint](multiPoints.size)
    multiPoints.toArray(multiPointArray)
  }

  private class CoordSeqCloneOp(var coordinateSequenceFactory: CoordinateSequenceFactory)
      extends GeometryEditor.CoordinateSequenceOperation {
    override def edit(coordSeq: CoordinateSequence, geometry: Geometry): CoordinateSequence =
      coordinateSequenceFactory.create(coordSeq)
  }

}

@SerialVersionUID(-6820524753094095635L)
class GeometryFactory(
  var precisionModel:            PrecisionModel,
  var SRID:                      Int,
  var coordinateSequenceFactory: CoordinateSequenceFactory
)

/**
 * Constructs a GeometryFactory that generates Geometries having the given PrecisionModel,
 * spatial-reference ID, and CoordinateSequence implementation.
 */
    extends Serializable {

  /**
   * Constructs a GeometryFactory that generates Geometries having the given CoordinateSequence
   * implementation, a double-precision floating PrecisionModel and a spatial-reference ID of 0.
   */
  def this(coordinateSequenceFactory: CoordinateSequenceFactory) =
    this(new PrecisionModel, 0, coordinateSequenceFactory)

  /**
   * Constructs a GeometryFactory that generates Geometries having the given {link PrecisionModel}
   * and the default CoordinateSequence implementation.
   *
   * @param precisionModel
   *   the PrecisionModel to use
   */
  def this(precisionModel: PrecisionModel) =
    this(precisionModel, 0, GeometryFactory.getDefaultCoordinateSequenceFactory)

  /**
   * Constructs a GeometryFactory that generates Geometries having the given {link PrecisionModel}
   * and spatial-reference ID, and the default CoordinateSequence implementation.
   *
   * @param precisionModel
   *   the PrecisionModel to use
   * @param SRID
   *   the SRID to use
   */
  def this(precisionModel: PrecisionModel, SRID: Int) =
    this(precisionModel, SRID, GeometryFactory.getDefaultCoordinateSequenceFactory)

  /**
   * Constructs a GeometryFactory that generates Geometries having a floating PrecisionModel and a
   * spatial-reference ID of 0.
   */
  def this() =
    this(new PrecisionModel, 0)

  /**
   * Creates a {link Geometry} with the same extent as the given envelope. The Geometry returned is
   * guaranteed to be valid. To provide this behaviour, the following cases occur: <p> If the
   * <code>Envelope</code> is: <ul> <li>null : returns an empty {link Point} <li>a point : returns a
   * non-empty {link Point} <li>a line : returns a two-point {link LineString} <li>a rectangle :
   * returns a {link Polygon} whose points are (minx, miny), (minx, maxy), (maxx, maxy), (maxx,
   * miny), (minx, miny). </ul>
   *
   * @param envelope
   *   the <code>Envelope</code> to convert return an empty <code>Point</code> (for null
   *   <code>Envelope</code>s), a <code>Point</code> (when min x = max x and min y = max y) or a
   *   <code>Polygon</code> (in all other cases)
   */
  def toGeometry(envelope: Envelope): Geometry = { // null envelope - return empty point geometry
    if (envelope.isNull) return createPoint
    // point?
    if (envelope.getMinX == envelope.getMaxX && envelope.getMinY == envelope.getMaxY)
      return createPoint(new Coordinate(envelope.getMinX, envelope.getMinY))
    // vertical or horizontal line?
    if (envelope.getMinX == envelope.getMaxX || envelope.getMinY == envelope.getMaxY)
      return createLineString(
        Array[Coordinate](new Coordinate(envelope.getMinX, envelope.getMinY),
                          new Coordinate(envelope.getMaxX, envelope.getMaxY)
        )
      )
    // create a CW ring for the polygon
    createPolygon(
      createLinearRing(
        Array[Coordinate](
          new Coordinate(envelope.getMinX, envelope.getMinY),
          new Coordinate(envelope.getMinX, envelope.getMaxY),
          new Coordinate(envelope.getMaxX, envelope.getMaxY),
          new Coordinate(envelope.getMaxX, envelope.getMinY),
          new Coordinate(envelope.getMinX, envelope.getMinY)
        )
      ),
      null
    )
  }

  /**
   * Returns the PrecisionModel that Geometries created by this factory will be associated with.
   *
   * return the PrecisionModel for this factory
   */
  def getPrecisionModel: PrecisionModel = precisionModel

  /**
   * Constructs an empty {link Point} geometry.
   *
   * return an empty Point
   */
  def createPoint: Point = createPoint(getCoordinateSequenceFactory.create(Array.empty[Coordinate]))

  /**
   * Creates a Point using the given Coordinate. A null Coordinate creates an empty Geometry.
   *
   * @param coordinate
   *   a Coordinate, or null return the created Point
   */
  def createPoint(coordinate: Coordinate): Point = createPoint(
    if (coordinate != null) getCoordinateSequenceFactory.create(Array[Coordinate](coordinate))
    else null
  )

  /**
   * Creates a Point using the given CoordinateSequence; a null or empty CoordinateSequence will
   * create an empty Point.
   *
   * @param coordinates
   *   a CoordinateSequence (possibly empty), or null return the created Point
   */
  def createPoint(coordinates: CoordinateSequence) = new Point(coordinates, this)

  /**
   * Constructs an empty {link MultiLineString} geometry.
   *
   * return an empty MultiLineString
   */
  def createMultiLineString = new MultiLineString(null, this)

  /**
   * Creates a MultiLineString using the given LineStrings; a null or empty array will create an
   * empty MultiLineString.
   *
   * @param lineStrings
   *   LineStrings, each of which may be empty but not null return the created MultiLineString
   */
  def createMultiLineString(lineStrings: Array[LineString]) = new MultiLineString(lineStrings, this)

  /**
   * Constructs an empty {link GeometryCollection} geometry.
   *
   * return an empty GeometryCollection
   */
  def createGeometryCollection = new GeometryCollection(null, this)

  /**
   * Creates a GeometryCollection using the given Geometries; a null or empty array will create an
   * empty GeometryCollection.
   *
   * @param geometries
   *   an array of Geometries, each of which may be empty but not null, or null return the created
   *   GeometryCollection
   */
  def createGeometryCollection(geometries: Array[Geometry]) =
    new GeometryCollection(geometries, this)

  /**
   * Constructs an empty {link MultiPolygon} geometry.
   *
   * return an empty MultiPolygon
   */
  def createMultiPolygon = new MultiPolygon(null, this)

  /**
   * Creates a MultiPolygon using the given Polygons; a null or empty array will create an empty
   * Polygon. The polygons must conform to the assertions specified in the <A
   * HREF="http://www.opengis.org/techno/specs.htm">OpenGIS Simple Features Specification for
   * SQL</A>.
   *
   * @param polygons
   *   Polygons, each of which may be empty but not null return the created MultiPolygon
   */
  def createMultiPolygon(polygons: Array[Polygon]) = new MultiPolygon(polygons, this)

  /**
   * Constructs an empty {link LinearRing} geometry.
   *
   * return an empty LinearRing
   */
  def createLinearRing: LinearRing = createLinearRing(
    getCoordinateSequenceFactory.create(Array.empty[Coordinate])
  )

  /**
   * Creates a {link LinearRing} using the given {link Coordinate}s. A null or empty array creates
   * an empty LinearRing. The points must form a closed and simple linestring.
   *
   * @param coordinates
   *   an array without null elements, or an empty array, or null return the created LinearRing
   *   throws IllegalArgumentException if the ring is not closed, or has too few points
   */
  def createLinearRing(coordinates: Array[Coordinate]): LinearRing = createLinearRing(
    if (coordinates != null) getCoordinateSequenceFactory.create(coordinates)
    else null
  )

  /**
   * Creates a {link LinearRing} using the given {link CoordinateSequence}. A null or empty array
   * creates an empty LinearRing. The points must form a closed and simple linestring.
   *
   * @param coordinates
   *   a CoordinateSequence (possibly empty), or null return the created LinearRing throws
   *   IllegalArgumentException if the ring is not closed, or has too few points
   */
  def createLinearRing(coordinates: CoordinateSequence) = new LinearRing(coordinates, this)

  /**
   * Constructs an empty {link MultiPoint} geometry.
   *
   * return an empty MultiPoint
   */
  def createMultiPoint = new MultiPoint(null, this)

  /**
   * Creates a {link MultiPoint} using the given {link Point}s. A null or empty array will create an
   * empty MultiPoint.
   *
   * @param point
   *   an array of Points (without null elements), or an empty array, or <code>null</code> return a
   *   MultiPoint object
   */
  def createMultiPoint(point: Array[Point]) = new MultiPoint(point, this)

  /**
   * Creates a {link MultiPoint} using the given {link Coordinate}s. A null or empty array will
   * create an empty MultiPoint.
   *
   * @param coordinates
   *   an array (without null elements), or an empty array, or <code>null</code> return a MultiPoint
   *   object
   * @deprecated
   *   Use { @link GeometryFactory#createMultiPointFromCoords} instead
   */
  def createMultiPoint(coordinates: Array[Coordinate]): MultiPoint = createMultiPoint(
    if (coordinates != null) getCoordinateSequenceFactory.create(coordinates)
    else null
  )

  /**
   * Creates a {link MultiPoint} using the given {link Coordinate}s. A null or empty array will
   * create an empty MultiPoint.
   *
   * @param coordinates
   *   an array (without null elements), or an empty array, or <code>null</code> return a MultiPoint
   *   object
   */
  def createMultiPointFromCoords(coordinates: Array[Coordinate]): MultiPoint = createMultiPoint(
    if (coordinates != null) getCoordinateSequenceFactory.create(coordinates)
    else null
  )

  /**
   * Creates a {link MultiPoint} using the points in the given {link CoordinateSequence}. A
   * <code>null</code> or empty CoordinateSequence creates an empty MultiPoint.
   *
   * @param coordinates
   *   a CoordinateSequence (possibly empty), or <code>null</code> return a MultiPoint geometry
   */
  def createMultiPoint(coordinates: CoordinateSequence): MultiPoint = {
    if (coordinates == null) return createMultiPoint(new Array[Point](0))
    val points = new Array[Point](coordinates.size)
    var i      = 0
    while (i < coordinates.size) {
      val ptSeq =
        getCoordinateSequenceFactory.create(1, coordinates.getDimension, coordinates.getMeasures)
      CoordinateSequences.copy(coordinates, i, ptSeq, 0, 1)
      points(i) = createPoint(ptSeq)
      i += 1
    }
    createMultiPoint(points)
  }

  /**
   * Constructs a <code>Polygon</code> with the given exterior boundary and interior boundaries.
   *
   * @param shell
   *   the outer boundary of the new <code>Polygon</code>, or <code>null</code> or an empty
   *   <code>LinearRing</code> if the empty geometry is to be created.
   * @param holes
   *   the inner boundaries of the new <code>Polygon</code>, or <code>null</code> or empty
   *   <code>LinearRing</code> s if the empty geometry is to be created. throws
   *   IllegalArgumentException if a ring is invalid
   */
  def createPolygon(shell: LinearRing, holes: Array[LinearRing]) = new Polygon(shell, holes, this)

  /**
   * Constructs a <code>Polygon</code> with the given exterior boundary.
   *
   * @param shell
   *   the outer boundary of the new <code>Polygon</code>, or <code>null</code> or an empty
   *   <code>LinearRing</code> if the empty geometry is to be created. throws
   *   IllegalArgumentException if the boundary ring is invalid
   */
  def createPolygon(shell: CoordinateSequence): Polygon = createPolygon(createLinearRing(shell))

  def createPolygon(shell: Array[Coordinate]): Polygon = createPolygon(createLinearRing(shell))

  def createPolygon(shell: LinearRing): Polygon = createPolygon(shell, null)

  /**
   * Constructs an empty {link Polygon} geometry.
   *
   * return an empty polygon
   */
  def createPolygon: Polygon = createPolygon(null, null)

  /**
   * Build an appropriate <code>Geometry</code>, <code>MultiGeometry</code>, or
   * <code>GeometryCollection</code> to contain the <code>Geometry</code>s in it. For example:<br>
   *
   * <ul> <li> If <code>geomList</code> contains a single <code>Polygon</code>, the
   * <code>Polygon</code> is returned. <li> If <code>geomList</code> contains several
   * <code>Polygon</code>s, a <code>MultiPolygon</code> is returned. <li> If <code>geomList</code>
   * contains some <code>Polygon</code>s and some <code>LineString</code>s, a
   * <code>GeometryCollection</code> is returned. <li> If <code>geomList</code> is empty, an empty
   * <code>GeometryCollection</code> is returned </ul>
   *
   * Note that this method does not "flatten" Geometries in the input, and hence if any
   * MultiGeometries are contained in the input a GeometryCollection containing them will be
   * returned.
   *
   * @param geomList
   *   the <code>Geometry</code>s to combine return a <code>Geometry</code> of the "smallest", "most
   *   type-specific" class that can contain the elements of <code>geomList</code> .
   */
  def buildGeometry(geomList: util.Collection[Geometry]): Geometry = {

    /**
     * Determine some facts about the geometries in the list
     */
    var geomClass: Class[_]   = null
    var isHeterogeneous       = false
    var hasGeometryCollection = false
    val i                     = geomList.iterator
    while (i.hasNext) {
      val geom      = i.next
      val partClass = geom.getClass
      if (geomClass == null) geomClass = partClass
      if (partClass != geomClass) isHeterogeneous = true
      if (geom.isInstanceOf[GeometryCollection]) hasGeometryCollection = true
    }

    /**
     * Now construct an appropriate geometry to return
     */
    // for the empty geometry, return an empty GeometryCollection
    if (geomClass == null) return createGeometryCollection
    if (isHeterogeneous || hasGeometryCollection)
      return createGeometryCollection(GeometryFactory.toGeometryArray(geomList))
    // at this point we know the collection is hetereogenous.
    // Determine the type of the result from the first Geometry in the list
    // this should always return a geometry, since otherwise an empty collection would have already been returned
    val geom0        = geomList.iterator.next
    val isCollection = geomList.size > 1
    if (isCollection) {
      if (geom0.isInstanceOf[Polygon])
        return createMultiPolygon(GeometryFactory.toPolygonArray(geomList))
      else if (geom0.isInstanceOf[LineString])
        return createMultiLineString(GeometryFactory.toLineStringArray(geomList))
      else if (geom0.isInstanceOf[Point])
        return createMultiPoint(GeometryFactory.toPointArray(geomList))
      Assert.shouldNeverReachHere("Unhandled class: " + geom0.getClass.getName)
    }
    geom0
  }

  /**
   * Constructs an empty {link LineString} geometry.
   *
   * return an empty LineString
   */
  def createLineString: LineString = createLineString(
    getCoordinateSequenceFactory.create(Array.empty[Coordinate])
  )

  /**
   * Creates a LineString using the given Coordinates. A null or empty array creates an empty
   * LineString.
   *
   * @param coordinates
   *   an array without null elements, or an empty array, or null
   */
  def createLineString(coordinates: Array[Coordinate]): LineString = createLineString(
    if (coordinates != null) getCoordinateSequenceFactory.create(coordinates)
    else null
  )

  /**
   * Creates a LineString using the given CoordinateSequence. A null or empty CoordinateSequence
   * creates an empty LineString.
   *
   * @param coordinates
   *   a CoordinateSequence (possibly empty), or null
   */
  def createLineString(coordinates: CoordinateSequence) = new LineString(coordinates, this)

  /**
   * Creates an empty atomic geometry of the given dimension. If passed a dimension of -1 will
   * create an empty {link GeometryCollection}.
   *
   * @param dimension
   *   the required dimension (-1, 0, 1 or 2) return an empty atomic geometry of given dimension
   */
  def createEmpty(dimension: Int): Geometry = dimension match {
    case -1 =>
      createGeometryCollection
    case 0  =>
      createPoint
    case 1  =>
      createLineString
    case 2  =>
      createPolygon
    case _  =>
      throw new IllegalArgumentException("Invalid dimension: " + dimension)
  }

  /**
   * Creates a deep copy of the input {link Geometry}. The {link CoordinateSequenceFactory} defined
   * for this factory is used to copy the {link CoordinateSequence}s of the input geometry. <p> This
   * is a convenient way to change the <tt>CoordinateSequence</tt> used to represent a geometry, or
   * to change the factory used for a geometry. <p> {link Geometry#copy()} can also be used to make
   * a deep copy, but it does not allow changing the CoordinateSequence type.
   *
   * return a deep copy of the input geometry, using the CoordinateSequence type of this factory
   * @see
   *   Geometry#copy()
   */
  def createGeometry(g: Geometry): Geometry = {
    val editor = new GeometryEditor(this)
    editor.edit(g, new GeometryFactory.CoordSeqCloneOp(coordinateSequenceFactory))
  }

  /**
   * Gets the SRID value defined for this factory.
   *
   * return the factory SRID value
   */
  def getSRID: Int = SRID

  def getCoordinateSequenceFactory: CoordinateSequenceFactory = coordinateSequenceFactory
}
