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
import org.locationtech.jts.util.Assert

/**
 * A class which supports creating new {link Geometry}s which are modifications of existing ones,
 * maintaining the same type structure. Geometry objects are intended to be treated as immutable.
 * This class "modifies" Geometrys by traversing them, applying a user-defined {link
 * GeometryEditorOperation}, {link CoordinateSequenceOperation} or {link CoordinateOperation} and
 * creating new Geometrys with the same structure but (possibly) modified components. <p> Examples
 * of the kinds of modifications which can be made are: <ul> <li>the values of the coordinates may
 * be changed. The editor does not check whether changing coordinate values makes the result
 * Geometry invalid <li>the coordinate lists may be changed (e.g. by adding, deleting or modifying
 * coordinates). The modified coordinate lists must be consistent with their original parent
 * component (e.g. a <tt>LinearRing</tt> must always have at least 4 coordinates, and the first and
 * last coordinate must be equal) <li>components of the original geometry may be deleted (e.g. holes
 * may be removed from a Polygon, or LineStrings removed from a MultiLineString). Deletions will be
 * propagated up the component tree appropriately. </ul> All changes must be consistent with the
 * original Geometry's structure (e.g. a <tt>Polygon</tt> cannot be collapsed into a
 * <tt>LineString</tt>). If changing the structure is required, use a {link GeometryTransformer}.
 * <p> This class supports creating an edited Geometry using a different
 * <code>GeometryFactory</code> via the {link #GeometryEditor(GeometryFactory)} constructor.
 * Examples of situations where this is required is if the geometry is transformed to a new SRID
 * and/or a new PrecisionModel. <p> <b>Usage Notes</b> <ul> <li>The resulting Geometry is not
 * checked for validity. If validity needs to be enforced, the new Geometry's {link
 * Geometry#isValid} method should be called. <li>By default the UserData of the input geometry is
 * not copied to the result. </ul>
 *
 * @see
 *   GeometryTransformer
 * @see
 *   Geometry#isValid
 * @version 1.7
 */
object GeometryEditor {

  /**
   * A interface which specifies an edit operation for Geometries.
   *
   * @version 1.7
   */
  trait GeometryEditorOperation {

    /**
     * Edits a Geometry by returning a new Geometry with a modification. The returned geometry may
     * be: <ul> <li>the input geometry itself. The returned Geometry might be the same as the
     * Geometry passed in. <li><code>null</code> if the geometry is to be deleted. </ul>
     *
     * @param geometry
     *   the Geometry to modify
     * @param factory
     *   the factory with which to construct the modified Geometry (may be different to the factory
     *   of the input geometry) return a new Geometry which is a modification of the input Geometry
     *   return null if the Geometry is to be deleted completely
     */
    def edit(geometry: Geometry, factory: GeometryFactory): Geometry
  }

  /**
   * A GeometryEditorOperation which does not modify the input geometry. This can be used for simple
   * changes of GeometryFactory (including PrecisionModel and SRID).
   *
   * @author
   *   mbdavis
   */
  class NoOpGeometryOperation extends GeometryEditor.GeometryEditorOperation {
    override def edit(geometry: Geometry, factory: GeometryFactory): Geometry = geometry
  }

  /**
   * A {link GeometryEditorOperation} which edits the coordinate list of a {link Geometry}. Operates
   * on Geometry subclasses which contains a single coordinate list.
   */
  abstract class CoordinateOperation extends GeometryEditor.GeometryEditorOperation {
    override final def edit(geometry: Geometry, factory: GeometryFactory): Geometry = {
      if (geometry.isInstanceOf[LinearRing])
        return factory.createLinearRing(edit(geometry.getCoordinates, geometry))
      if (geometry.isInstanceOf[LineString])
        return factory.createLineString(edit(geometry.getCoordinates, geometry))
      if (geometry.isInstanceOf[Point]) {
        val newCoordinates = edit(geometry.getCoordinates, geometry)
        return factory.createPoint(
          if (newCoordinates.length > 0) newCoordinates(0)
          else null
        )
      }
      geometry
    }

    /**
     * Edits the array of {link Coordinate}s from a {link Geometry}. <p> If it is desired to
     * preserve the immutability of Geometrys, if the coordinates are changed a new array should be
     * created and returned.
     *
     * @param coordinates
     *   the coordinate array to operate on
     * @param geometry
     *   the geometry containing the coordinate list return an edited coordinate array (which may be
     *   the same as the input)
     */
    def edit(coordinates: Array[Coordinate], geometry: Geometry): Array[Coordinate]
  }

  /**
   * A {link GeometryEditorOperation} which edits the {link CoordinateSequence} of a {link
   * Geometry}. Operates on Geometry subclasses which contains a single coordinate list.
   */
  abstract class CoordinateSequenceOperation extends GeometryEditor.GeometryEditorOperation {
    override final def edit(geometry: Geometry, factory: GeometryFactory): Geometry = {
      if (geometry.isInstanceOf[LinearRing])
        return factory.createLinearRing(
          edit(geometry.asInstanceOf[LinearRing].getCoordinateSequence, geometry)
        )
      if (geometry.isInstanceOf[LineString])
        return factory.createLineString(
          edit(geometry.asInstanceOf[LineString].getCoordinateSequence, geometry)
        )
      if (geometry.isInstanceOf[Point])
        return factory.createPoint(
          edit(geometry.asInstanceOf[Point].getCoordinateSequence, geometry)
        )
      geometry
    }

    /**
     * Edits a {link CoordinateSequence} from a {link Geometry}.
     *
     * @param coordSeq
     *   the coordinate array to operate on
     * @param geometry
     *   the geometry containing the coordinate list return an edited coordinate sequence (which may
     *   be the same as the input)
     */
    def edit(coordSeq: CoordinateSequence, geometry: Geometry): CoordinateSequence
  }

}

class GeometryEditor(var factory: GeometryFactory) {

  /**
   * Creates a new GeometryEditor object which will create edited {link Geometry}s with the same
   * {link GeometryFactory} as the input Geometry.
   */
  /**
   * The factory used to create the modified Geometry. If <tt>null</tt> the GeometryFactory of the
   * input is used.
   */
  private var isUserDataCopied = false

  /**
   * Creates a new GeometryEditor object which will create edited {link Geometry}s with the given
   * {link GeometryFactory}.
   *
   * @param factory
   *   the GeometryFactory to create edited Geometrys with
   */
//  def this {
//    this()
//    this.factory = factory
//  }
//
  /**
   * Sets whether the User Data is copied to the edit result. Only the object reference is copied.
   *
   * @param isUserDataCopied
   *   true if the input user data should be copied.
   */
  def setCopyUserData(isUserDataCopied: Boolean): Unit = this.isUserDataCopied = isUserDataCopied

  /**
   * Edit the input {link Geometry} with the given edit operation. Clients can create subclasses of
   * {link GeometryEditorOperation} or {link CoordinateOperation} to perform required modifications.
   *
   * @param geometry
   *   the Geometry to edit
   * @param operation
   *   the edit operation to carry out return a new { @link Geometry} which is the result of the
   *   editing (which may be empty)
   */
  def edit(geometry: Geometry, operation: GeometryEditor.GeometryEditorOperation): Geometry = { // nothing to do
    if (geometry == null) return null
    val result = editInternal(geometry, operation)
    if (isUserDataCopied) result.setUserData(geometry.getUserData)
    result
  }

  private def editInternal(
    geometry:  Geometry,
    operation: GeometryEditor.GeometryEditorOperation
  ): Geometry = { // if client did not supply a GeometryFactory, use the one from the input Geometry
    if (factory == null) factory = geometry.getFactory
    if (geometry.isInstanceOf[GeometryCollection])
      return editGeometryCollection(geometry.asInstanceOf[GeometryCollection], operation)
    if (geometry.isInstanceOf[Polygon])
      return editPolygon(geometry.asInstanceOf[Polygon], operation)
    if (geometry.isInstanceOf[Point]) return operation.edit(geometry, factory)
    if (geometry.isInstanceOf[LineString]) return operation.edit(geometry, factory)
    Assert.shouldNeverReachHere("Unsupported Geometry class: " + geometry.getClass.getName)
    null
  }

  private def editPolygon(
    polygon:   Polygon,
    operation: GeometryEditor.GeometryEditorOperation
  ): Polygon = {
    var newPolygon = operation.edit(polygon, factory).asInstanceOf[Polygon]
    // create one if needed
    if (newPolygon == null) newPolygon = factory.createPolygon
    if (newPolygon.isEmpty) { // RemoveSelectedPlugIn relies on this behaviour. [Jon Aquino]
      return newPolygon
    }
    val shell      = edit(newPolygon.getExteriorRing, operation).asInstanceOf[LinearRing]
    if (shell == null || shell.isEmpty) return factory.createPolygon
    val holes      = new util.ArrayList[LinearRing]
    var i          = 0
    while (i < newPolygon.getNumInteriorRing) {
      val hole = edit(newPolygon.getInteriorRingN(i), operation).asInstanceOf[LinearRing]
      if (!(hole == null || hole.isEmpty))
        holes.add(hole)
      i += 1
    }
    factory.createPolygon(shell, holes.toArray(Array.empty[LinearRing]))
  }

  private def editGeometryCollection(
    collection: GeometryCollection,
    operation:  GeometryEditor.GeometryEditorOperation
  ): GeometryCollection = { // first edit the entire collection
    // MD - not sure why this is done - could just check original collection?
    val collectionForType = operation.edit(collection, factory).asInstanceOf[GeometryCollection]
    // edit the component geometries
    val geometries        = new util.ArrayList[Geometry]
    var i                 = 0
    while (i < collectionForType.getNumGeometries) {
      val geometry = edit(collectionForType.getGeometryN(i), operation)
      if (!(geometry == null || geometry.isEmpty))
        geometries.add(geometry)
      i += 1
    }
    if (collectionForType.getClass eq classOf[MultiPoint])
      return factory.createMultiPoint(geometries.toArray(Array.empty[Point]))
    if (collectionForType.getClass eq classOf[MultiLineString])
      return factory.createMultiLineString(geometries.toArray(Array.empty[LineString]))
    if (collectionForType.getClass eq classOf[MultiPolygon])
      return factory.createMultiPolygon(geometries.toArray(Array.empty[Polygon]))
    factory.createGeometryCollection(geometries.toArray(Array.empty[Geometry]))

  }
}
