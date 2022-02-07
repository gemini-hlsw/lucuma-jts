// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2020 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.algorithm.construct

import java.util.PriorityQueue

import org.locationtech.jts.algorithm.construct.MaximumInscribedCircle.Cell
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.operation.distance.IndexedFacetDistance

/**
 * Constructs the Maximum Inscribed Circle for a polygonal {link Geometry}, up to a specified
 * tolerance. The Maximum Inscribed Circle is determined by a point in the interior of the area
 * which has the farthest distance from the area boundary, along with a boundary point at that
 * distance. <p> In the context of geography the center of the Maximum Inscribed Circle is known as
 * the <b>Pole of Inaccessibility</b>. A cartographic use case is to determine a suitable point to
 * place a map label within a polygon. <p> The radius length of the Maximum Inscribed Circle is a
 * measure of how "narrow" a polygon is. It is the distance at which the negative buffer becomes
 * empty. <p> The class supports polygons with holes and multipolygons. <p> The implementation uses
 * a successive-approximation technique over a grid of square cells covering the area geometry. The
 * grid is refined using a branch-and-bound algorithm. Point containment and distance are computed
 * in a performant way by using spatial indexes.
 *
 * <h3>Future Enhancements</h3> <ul> <li>Support a polygonal constraint on placement of center </ul>
 *
 * @author
 *   Martin Davis
 */
object MaximumInscribedCircle {

  /**
   * Computes the center point of the Maximum Inscribed Circle of a polygonal geometry, up to a
   * given tolerance distance.
   *
   * @param polygonal
   *   a polygonal geometry
   * @param tolerance
   *   the distance tolerance for computing the center point return the center point of the maximum
   *   inscribed circle
   */
  def getCenter(polygonal: Geometry, tolerance: Double) = {
    val mic = new MaximumInscribedCircle(polygonal, tolerance)
    mic.getCenter
  }

  /**
   * Computes a radius line of the Maximum Inscribed Circle of a polygonal geometry, up to a given
   * tolerance distance.
   *
   * @param polygonal
   *   a polygonal geometry
   * @param tolerance
   *   the distance tolerance for computing the center point return a line from the center to a
   *   point on the circle
   */
  def getRadiusLine(polygonal: Geometry, tolerance: Double) = {
    val mic = new MaximumInscribedCircle(polygonal, tolerance)
    mic.getRadiusLine
  }

  /**
   * A square grid cell centered on a given point, with a given half-side size, and having a given
   * distance to the area boundary. The maximum possible distance from any point in the cell to the
   * boundary can be computed, and is used as the ordering and upper-bound function in the
   * branch-and-bound algorithm.
   */
  private object Cell {
    private val SQRT2 = 1.4142135623730951
  }

  private class Cell(
    var x:        Double,
    var y:        Double,
    var hSide:    Double,
    var distance: Double
  ) // cell center x
  // cell center y
  // half the cell size
      extends Comparable[MaximumInscribedCircle.Cell] { // the distance from cell center to area boundary
    // the maximum possible distance to area boundary for points in this cell
    private var maxDist = .0
    this.maxDist = distance + hSide * Cell.SQRT2

    def getEnvelope = new Envelope(x - hSide, x + hSide, y - hSide, y + hSide)

    def getMaxDistance = maxDist

    def getDistance = distance

    def getHSide = hSide

    def getX = x

    def getY = y

    /**
     * A cell is greater iff its maximum possible distance is larger.
     */
    override def compareTo(o: MaximumInscribedCircle.Cell) = (o.maxDist - this.maxDist).toInt
  }

}

class MaximumInscribedCircle(var inputGeom: Geometry, var tolerance: Double) {

  /**
   * Creates a new instance of a Maximum Inscribed Circle computation.
   *
   * @param polygonal
   *   an areal geometry
   * @param tolerance
   *   the distance tolerance for computing the centre point
   */
  if (!(inputGeom.isInstanceOf[Polygon] || inputGeom.isInstanceOf[MultiPolygon]))
    throw new IllegalArgumentException("Input geometry must be a Polygon or MultiPolygon")
  if (inputGeom.isEmpty) throw new IllegalArgumentException("Empty input geometry is not supported")
  private var factory: GeometryFactory              = inputGeom.getFactory
  private val ptLocater: IndexedPointInAreaLocator  = new IndexedPointInAreaLocator(inputGeom)
  private val indexedDistance: IndexedFacetDistance = new IndexedFacetDistance(
    inputGeom.getBoundary
  )
  private var centerCell: Cell                      = null
  private var centerPt: Coordinate                  = null
  private var radiusPt: Coordinate                  = null
  private var centerPoint: Point                    = null
  private var radiusPoint: Point                    = null
  this.factory = inputGeom.getFactory

  /**
   * Gets the center point of the maximum inscribed circle (up to the tolerance distance).
   *
   * return the center point of the maximum inscribed circle
   */
  def getCenter = {
    compute()
    centerPoint
  }

  /**
   * Gets a point defining the radius of the Maximum Inscribed Circle. This is a point on the
   * boundary which is nearest to the computed center of the Maximum Inscribed Circle. The line
   * segment from the center to this point is a radius of the constructed circle, and this point
   * lies on the boundary of the circle.
   *
   * return a point defining the radius of the Maximum Inscribed Circle
   */
  def getRadiusPoint = {
    compute()
    radiusPoint
  }

  /**
   * Gets a line representing a radius of the Largest Empty Circle.
   *
   * return a line from the center of the circle to a point on the edge
   */
  def getRadiusLine = {
    compute()
    val radiusLine = factory.createLineString(Array[Coordinate](centerPt.copy, radiusPt.copy))
    radiusLine
  }

  /**
   * Computes the signed distance from a point to the area boundary. Points outside the polygon are
   * assigned a negative distance. Their containing cells will be last in the priority queue (but
   * may still end up being tested since they may need to be refined).
   *
   * @param p
   *   the point to compute the distance for return the signed distance to the area boundary
   *   (negative indicates outside the area)
   */
  private def distanceToBoundary(p: Point): Double = {
    val dist     = indexedDistance.distance(p)
    val isOutide = Location.EXTERIOR == ptLocater.locate(p.getCoordinate)
    if (isOutide) return -dist
    dist
  }

  private def distanceToBoundary(x: Double, y: Double): Double = {
    val coord = new Coordinate(x, y)
    val pt    = factory.createPoint(coord)
    distanceToBoundary(pt)
  }

  private def compute(): Unit = { // check if already computed
    if (centerCell != null) return
    // Priority queue of cells, ordered by maximum distance from boundary
    val cellQueue    = new PriorityQueue[MaximumInscribedCircle.Cell]
    createInitialGrid(inputGeom.getEnvelopeInternal, cellQueue)
    // use the area centroid as the initial candidate center point
    var farthestCell = createCentroidCell(inputGeom)
    // int totalCells = cellQueue.size();
    /**
     * Carry out the branch-and-bound search of the cell space
     */
    while (!cellQueue.isEmpty) { // pick the most promising cell from the queue
      val cell = cellQueue.remove()
      // System.out.println(factory.toGeometry(cell.getEnvelope()));
      // update the center cell if the candidate is further from the boundary
      if (cell.getDistance > farthestCell.getDistance) farthestCell = cell

      /**
       * Refine this cell if the potential distance improvement is greater than the required
       * tolerance. Otherwise the cell is pruned (not investigated further), since no point in it is
       * further than the current farthest distance.
       */
      val potentialIncrease = cell.getMaxDistance - farthestCell.getDistance
      if (potentialIncrease > tolerance) { // split the cell into four sub-cells
        val h2 = cell.getHSide / 2
        cellQueue.add(createCell(cell.getX - h2, cell.getY - h2, h2))
        cellQueue.add(createCell(cell.getX + h2, cell.getY - h2, h2))
        cellQueue.add(createCell(cell.getX - h2, cell.getY + h2, h2))
        cellQueue.add(createCell(cell.getX + h2, cell.getY + h2, h2))
        // totalCells += 4;
      }
    }
    // the farthest cell is the best approximation to the MIC center
    centerCell = farthestCell
    centerPt = new Coordinate(centerCell.getX, centerCell.getY)
    centerPoint = factory.createPoint(centerPt)
    // compute radius point
    val nearestPts   = indexedDistance.nearestPoints(centerPoint)
    radiusPt = nearestPts(0).copy
    radiusPoint = factory.createPoint(radiusPt)
  }

  /**
   * Initializes the queue with a grid of cells covering the extent of the area.
   *
   * @param env
   *   the area extent to cover
   * @param cellQueue
   *   the queue to initialize
   */
  private def createInitialGrid(
    env:       Envelope,
    cellQueue: PriorityQueue[MaximumInscribedCircle.Cell]
  ) = {
    val minX     = env.getMinX
    val maxX     = env.getMaxX
    val minY     = env.getMinY
    val maxY     = env.getMaxY
    val width    = env.getWidth
    val height   = env.getHeight
    val cellSize = Math.min(width, height)
    val hSide    = cellSize / 2.0
    // compute initial grid of cells to cover area
    var x        = minX
    while (x < maxX) {
      var y = minY
      while (y < maxY) {
        cellQueue.add(createCell(x + hSide, y + hSide, hSide))
        y += cellSize
      }
      x += cellSize
    }
  }

  private def createCell(x: Double, y: Double, hSide: Double) =
    new MaximumInscribedCircle.Cell(x, y, hSide, distanceToBoundary(x, y))

  // create a cell centered on area centroid
  private def createCentroidCell(geom: Geometry) = {
    val p = geom.getCentroid
    new MaximumInscribedCircle.Cell(p.getX, p.getY, 0, distanceToBoundary(p))
  }
}
