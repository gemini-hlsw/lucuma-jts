// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.precision

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.util.GeometryTransformer
import org.locationtech.jts.operation.overlayng.PrecisionReducer

object PrecisionReducerTransformer {
  def reduce(geom: Geometry, targetPM: PrecisionModel): Geometry = reduce(geom, targetPM, false)

  def reduce(geom: Geometry, targetPM: PrecisionModel, isPointwise: Boolean): Geometry = {
    val trans = new PrecisionReducerTransformer(targetPM, isPointwise)
    trans.transform(geom)
  }
}

class PrecisionReducerTransformer private[precision] (
  var targetPM:    PrecisionModel,
  val isPointwise: Boolean
) extends GeometryTransformer {

  def this(targetPM: PrecisionModel) =
    this(targetPM, false)

  override protected def transformCoordinates(
    coordinates: CoordinateSequence,
    parent:      Geometry
  ): CoordinateSequence = {
    if (coordinates.size == 0) return null
    val coordsReduce =
      if (isPointwise) reducePointwise(coordinates) else reduceCompress(coordinates)

    /**
     * Check to see if the removal of repeated points collapsed the coordinate List to an invalid
     * length for the type of the parent geometry. It is not necessary to check for Point collapses,
     * since the coordinate list can never collapse to less than one point. If the length is
     * invalid, return the full-length coordinate array first computed, or null if collapses are
     * being removed. (This may create an invalid geometry - the client must handle this.)
     */
    var minLength = 0
    if (parent.isInstanceOf[LineString]) minLength = 2
    if (parent.isInstanceOf[LinearRing]) minLength = 4
    // collapse - return null so parent is removed or empty
    if (coordsReduce.length < minLength) return null
    factory.getCoordinateSequenceFactory.create(coordsReduce)
  }

  private def reduceCompress(coordinates: CoordinateSequence) = {
    val noRepeatCoordList = new CoordinateList
    // copy coordinates and reduce
    for (i <- 0 until coordinates.size) {
      val coord = coordinates.getCoordinate(i).copy
      targetPM.makePrecise(coord)
      noRepeatCoordList.add(coord, false)
    }
    // remove repeated points, to simplify returned geometry as much as possible
    val noRepeatCoords = noRepeatCoordList.toCoordinateArray
    noRepeatCoords
  }

  private def reducePointwise(coordinates: CoordinateSequence) = {
    val coordReduce = new Array[Coordinate](coordinates.size)
    for (i <- 0 until coordinates.size) {
      val coord = coordinates.getCoordinate(i).copy
      targetPM.makePrecise(coord)
      coordReduce(i) = coord
    }
    coordReduce
  }

  override protected def transformPolygon(geom: Polygon, parent: Geometry): Geometry = {
    if (isPointwise) {
      val trans = super.transformPolygon(geom, parent)

      /**
       * For some reason the base transformer may return non-polygonal geoms here. Check this and
       * return an empty polygon instead.
       */
      if (trans.isInstanceOf[Polygon]) return trans
      return factory.createPolygon
    }
    reduceArea(geom)
  }

  override protected def transformMultiPolygon(geom: MultiPolygon, parent: Geometry): Geometry = {
    if (isPointwise) return super.transformMultiPolygon(geom, parent)
    reduceArea(geom)
  }

  private def reduceArea(geom: Geometry) = {
    val reduced = PrecisionReducer.reducePrecision(geom, targetPM)
    reduced
  }
}
