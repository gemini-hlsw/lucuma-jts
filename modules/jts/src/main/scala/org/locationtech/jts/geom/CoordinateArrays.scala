// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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

import org.locationtech.jts.math.MathUtil

import java.util.Comparator

/**
 * Useful utility functions for handling Coordinate arrays
 *
 * @version 1.7
 */
object CoordinateArrays {
  private val coordArrayType = new Array[Coordinate](0)

  /**
   * Determine dimension based on subclass of {link Coordinate}.
   *
   * @param pts
   *   supplied coordinates return number of ordinates recorded
   */
  def dimension(pts: Array[Coordinate]): Int = {
    if (pts == null || pts.length == 0) return 3 // unknown, assume default
    var dimension = 0
    for (coordinate <- pts)
      dimension = Math.max(dimension, Coordinates.dimension(coordinate))
    dimension
  }

  /**
   * Determine number of measures based on subclass of {link Coordinate}.
   *
   * @param pts
   *   supplied coordinates return number of measures recorded
   */
  def measures(pts: Array[Coordinate]): Int = {
    if (pts == null || pts.length == 0) return 0
    var measures = 0
    for (coordinate <- pts)
      measures = Math.max(measures, Coordinates.measures(coordinate))
    measures
  }

  /**
   * Utility method ensuring array contents are of consistent dimension and measures. <p> Array is
   * modified in place if required, coordinates are replaced in the array as required to ensure all
   * coordinates have the same dimension and measures. The final dimension and measures used are the
   * maximum found when checking the array. </p>
   *
   * @param array
   *   Modified in place to coordinates of consistent dimension and measures.
   */
  def enforceConsistency(array: Array[Coordinate]): Unit = {
    if (array == null) {
      return
    }
    // step one check
    var maxDimension = -1
    var maxMeasures  = -1
    var isConsistent = true
    for (i <- 0 to array.length) yield {
      val coordinate = array(i)
      if (coordinate != null) {
        val d = Coordinates.dimension(coordinate)
        val m = Coordinates.measures(coordinate)
        if (maxDimension == -1) {
          maxDimension = d
          maxMeasures = m
        } else if (d != maxDimension || m != maxMeasures) {
          isConsistent = false
          maxDimension = Math.max(maxDimension, d)
          maxMeasures = Math.max(maxMeasures, m)
        }
      }
    }
    if (!isConsistent) {
      // step two fix
      val sample = Coordinates.create(maxDimension, maxMeasures)
      val clz    = sample.getClass()

      for (i <- 0 to array.length) yield {
        val coordinate = array(i)
        if (coordinate != null && !coordinate.getClass().equals(clz)) {
          val duplicate = Coordinates.create(maxDimension, maxMeasures)
          duplicate.setCoordinate(coordinate)
          array(i) = duplicate
        }
      }
    }
  }

  /**
   * Utility method ensuring array contents are of the specified dimension and measures. <p> Array
   * is returned unmodified if consistent, or a copy of the array is made with each inconsistent
   * coordinate duplicated into an instance of the correct dimension and measures. </p></>
   *
   * @param array
   *   coordinate array
   * @param dimension
   * @param measures
   * @return
   *   array returned, or copy created if required to enforce consistency.
   */
  def enforceConsistency(
    array:     Array[Coordinate],
    dimension: Int,
    measures:  Int
  ): Array[Coordinate] = {
    val sample       = Coordinates.create(dimension, measures)
    val clz          = sample.getClass()
    var isConsistent = true
    for {
      i <- 0 to array.length
      if isConsistent
    } yield {
      val coordinate = array(i)
      if (coordinate != null && !coordinate.getClass().equals(clz)) {
        isConsistent = false
      }
    }
    if (isConsistent) {
      return array
    } else {
      val coordinateType = sample.getClass()
      val copy           = java.lang.reflect.Array
        .newInstance(coordinateType, array.length)
        .asInstanceOf[Array[Coordinate]]
      for (i <- 0 to copy.length) {
        val coordinate = array(i)
        if (coordinate != null && !coordinate.getClass().equals(clz)) {
          val duplicate = Coordinates.create(dimension, measures)
          duplicate.setCoordinate(coordinate)
          copy(i) = duplicate
        } else {
          copy(i) = coordinate
        }
      }
      return copy
    }
  }

  /**
   * Tests whether an array of {link Coordinate}s forms a ring, by checking length and closure.
   * Self-intersection is not checked.
   *
   * @param pts
   *   an array of Coordinates return true if the coordinate form a ring.
   */
  def isRing(pts: Array[Coordinate]): Boolean = {
    if (pts.length < 4) return false
    if (!pts(0).equals2D(pts(pts.length - 1))) return false
    true
  }

  /**
   * Finds a point in a list of points which is not contained in another list of points
   *
   * @param testPts
   *   the { @link Coordinate}s to test
   * @param pts
   *   an array of { @link Coordinate}s to test the input points against return a { @link
   *   Coordinate} from <code>testPts</code> which is not in <code>pts</code>, ' or
   *   <code>null</code>
   */
  def ptNotInList(testPts: Array[Coordinate], pts: Array[Coordinate]): Coordinate =
    testPts.find(CoordinateArrays.indexOf(_, pts) < 0).orNull

  /**
   * Compares two {link Coordinate} arrays in the forward direction of their coordinates, using
   * lexicographic ordering.
   *
   * @param pts1
   * @param pts2
   *   return an integer indicating the order
   */
  def compare(pts1: Array[Coordinate], pts2: Array[Coordinate]): Int = {
    var i = 0
    while (i < pts1.length && i < pts2.length) {
      val compare = pts1(i).compareTo(pts2(i))
      if (compare != 0) return compare
      i += 1
    }
    // handle situation when arrays are of different length
    if (i < pts2.length) return -1
    if (i < pts1.length) return 1
    0
  }

  /**
   * A {link Comparator} for {link Coordinate} arrays in the forward direction of their coordinates,
   * using lexicographic ordering.
   */
  class ForwardComparator extends Comparator[Array[Coordinate]] {
    override def compare(o1: Array[Coordinate], o2: Array[Coordinate]): Int =
      CoordinateArrays.compare(o1, o2)
  }

  /**
   * Determines which orientation of the {link Coordinate} array is (overall) increasing. In other
   * words, determines which end of the array is "smaller" (using the standard ordering on {link
   * Coordinate}). Returns an integer indicating the increasing direction. If the sequence is a
   * palindrome, it is defined to be oriented in a positive direction.
   *
   * @param pts
   *   the array of Coordinates to test return <code>1</code> if the array is smaller at the start
   *   or is a palindrome, <code>-1</code> if smaller at the end
   */
  def increasingDirection(pts: Array[Coordinate]): Int = {
    var i = 0
    while (i < pts.length / 2) {
      val j    = pts.length - 1 - i
      // skip equal points on both ends
      val comp = pts(i).compareTo(pts(j))
      if (comp != 0) return comp
      i += 1
    }
    // array must be a palindrome - defined to be in positive direction
    1
  }

  /**
   * Determines whether two {link Coordinate} arrays of equal length are equal in opposite
   * directions.
   *
   * @param pts1
   * @param pts2
   *   return <code>true</code> if the two arrays are equal in opposite directions.
   */
  private def isEqualReversed(pts1: Array[Coordinate], pts2: Array[Coordinate]): Boolean = {
    var i = 0
    while (i < pts1.length) {
      val p1 = pts1(i)
      val p2 = pts2(pts1.length - i - 1)
      if (p1.compareTo(p2) != 0) return false
      i += 1
    }
    true
  }

  /**
   * A {link Comparator} for {link Coordinate} arrays modulo their directionality. E.g. if two
   * coordinate arrays are identical but reversed they will compare as equal under this ordering. If
   * the arrays are not equal, the ordering returned is the ordering in the forward direction.
   */
  class BidirectionalComparator extends Comparator[Array[Coordinate]] {
    override def compare(pts1: Array[Coordinate], pts2: Array[Coordinate]): Int = {
      if (pts1.length < pts2.length) return -1
      if (pts1.length > pts2.length) return 1
      if (pts1.length == 0) return 0
      val forwardComp = CoordinateArrays.compare(pts1, pts2)
      val isEqualRev  = isEqualReversed(pts1, pts2)
      if (isEqualRev) return 0
      forwardComp
    }

    def OLDcompare(o1: Any, o2: Any): Int = {
      val pts1 = o1.asInstanceOf[Array[Coordinate]]
      val pts2 = o2.asInstanceOf[Array[Coordinate]]
      if (pts1.length < pts2.length) return -1
      if (pts1.length > pts2.length) return 1
      if (pts1.length == 0) return 0
      val dir1 = increasingDirection(pts1)
      val dir2 = increasingDirection(pts2)
      var i1   =
        if (dir1 > 0) 0
        else pts1.length - 1
      var i2   =
        if (dir2 > 0) 0
        else pts1.length - 1
      var i    = 0
      while (i < pts1.length) {
        val comparePt = pts1(i1).compareTo(pts2(i2))
        if (comparePt != 0) return comparePt
        i1 += dir1
        i2 += dir2
        i += 1
      }
      0
    }
  }

  /**
   * Creates a deep copy of the argument {link Coordinate} array.
   *
   * @param coordinates
   *   an array of Coordinates return a deep copy of the input
   */
  def copyDeep(coordinates: Array[Coordinate]): Array[Coordinate] = {
    val copy = new Array[Coordinate](coordinates.length)
    var i    = 0
    while (i < coordinates.length) {
      copy(i) = coordinates(i).copy
      i += 1
    }
    copy
  }

  /**
   * Creates a deep copy of a given section of a source {link Coordinate} array into a destination
   * Coordinate array. The destination array must be an appropriate size to receive the copied
   * coordinates.
   *
   * @param src
   *   an array of Coordinates
   * @param srcStart
   *   the index to start copying from
   * @param dest
   *   the
   * @param destStart
   *   the destination index to start copying to
   * @param length
   *   the number of items to copy
   */
  def copyDeep(
    src:       Array[Coordinate],
    srcStart:  Int,
    dest:      Array[Coordinate],
    destStart: Int,
    length:    Int
  ): Unit = {
    var i = 0
    while (i < length) {
      dest(destStart + i) = src(srcStart + i).copy
      i += 1
    }
  }

  /**
   * Converts the given Collection of Coordinates into a Coordinate array.
   */
  def toCoordinateArray(coordList: java.util.Collection[_]): Array[Coordinate] =
    coordList.toArray(coordArrayType).asInstanceOf[Array[Coordinate]]

  /**
   * Returns whether #equals returns true for any two consecutive Coordinates in the given array.
   */
  def hasRepeatedPoints(coord: Array[Coordinate]): Boolean = {
    var i = 1
    while (i < coord.length) {
      if (coord(i - 1) == coord(i)) return true
      i += 1
    }
    false
  }

  /**
   * Returns either the given coordinate array if its length is greater than the given amount, or an
   * empty coordinate array.
   */
  def atLeastNCoordinatesOrNothing(n: Int, c: Array[Coordinate]): Array[Coordinate] = if (
    c.length >= n
  ) c
  else Array[Coordinate]()

  /**
   * If the coordinate array argument has repeated points, constructs a new array containing no
   * repeated points. Otherwise, returns the argument.
   *
   * @see
   *   #hasRepeatedPoints(Coordinate[])
   */
  def removeRepeatedPoints(coord: Array[Coordinate]): Array[Coordinate] = {
    if (!hasRepeatedPoints(coord)) return coord
    val coordList = new CoordinateList(coord, false)
    coordList.toCoordinateArray
  }

  /**
   * Collapses a coordinate array to remove all null elements.
   *
   * @param coord
   *   the coordinate array to collapse return an array containing only non-null elements
   */
  def removeNull(coord: Array[Coordinate]): Array[Coordinate] = {
    var nonNull  = 0
    var i        = 0
    while (i < coord.length) {
      if (coord(i) != null) {
        nonNull += 1; nonNull - 1
      }
      i += 1
    }
    val newCoord = new Array[Coordinate](nonNull)
    // empty case
    if (nonNull == 0) return newCoord
    var j        = 0
    i = 0
    while (i < coord.length) {
      if (coord(i) != null) newCoord {
        j += 1; j - 1
      } = coord(i)
      i += 1
    }
    newCoord
  }

  /**
   * Reverses the coordinates in an array in-place.
   */
  def reverse(coord: Array[Coordinate]): Unit = {
    val last = coord.length - 1
    val mid  = last / 2
    var i    = 0
    while (i <= mid) {
      val tmp = coord(i)
      coord(i) = coord(last - i)
      coord(last - i) = tmp
      i += 1
    }
  }

  /**
   * Returns true if the two arrays are identical, both null, or pointwise equal (as compared using
   * Coordinate#equals)
   *
   * @see
   *   Coordinate#equals(Object)
   */
  def equals(coord1: Array[Coordinate], coord2: Array[Coordinate]): Boolean = {
    if (coord1 eq coord2) return true
    if (coord1 == null || coord2 == null) return false
    if (coord1.length != coord2.length) return false
    var i = 0
    while (i < coord1.length) {
      if (!(coord1(i) == coord2(i))) return false
      i += 1
    }
    true
  }

  /**
   * Returns true if the two arrays are identical, both null, or pointwise equal, using a
   * user-defined {link Comparator} for {link Coordinate} s
   *
   * @param coord1
   *   an array of Coordinates
   * @param coord2
   *   an array of Coordinates
   * @param coordinateComparator
   *   a Comparator for Coordinates
   */
  def equals(
    coord1:               Array[Coordinate],
    coord2:               Array[Coordinate],
    coordinateComparator: Comparator[Coordinate]
  ): Boolean = {
    if (coord1 eq coord2) return true
    if (coord1 == null || coord2 == null) return false
    if (coord1.length != coord2.length) return false
    var i = 0
    while (i < coord1.length) {
      if (coordinateComparator.compare(coord1(i), coord2(i)) != 0) return false
      i += 1
    }
    true
  }

  /**
   * Returns the minimum coordinate, using the usual lexicographic comparison.
   *
   * @param coordinates
   *   the array to search return the minimum coordinate in the array, found using
   *   <code>compareTo</code>
   * @see
   *   Coordinate#compareTo(Object)
   */
  def minCoordinate(coordinates: Array[Coordinate]): Coordinate = {
    var minCoord: Coordinate = null
    var i                    = 0
    while (i < coordinates.length) {
      if (minCoord == null || minCoord.compareTo(coordinates(i)) > 0) minCoord = coordinates(i)
      i += 1
    }
    minCoord
  }

  /**
   * Shifts the positions of the coordinates until <code>firstCoordinate</code> is first.
   *
   * @param coordinates
   *   the array to rearrange
   * @param firstCoordinate
   *   the coordinate to make first
   */
  def scroll(coordinates: Array[Coordinate], firstCoordinate: Coordinate): Unit = {
    val i = indexOf(firstCoordinate, coordinates)
    scroll(coordinates, i);
  }

  /**
   * Shifts the positions of the coordinates until the coordinate at <code>firstCoordinate</code> is
   * first.
   *
   * @param coordinates
   *   the array to rearrange
   * @param indexOfFirstCoordinate
   *   the index of the coordinate to make first
   */
  def scroll(coordinates: Array[Coordinate], indexOfFirstCoordinate: Int): Unit =
    scroll(coordinates, indexOfFirstCoordinate, CoordinateArrays.isRing(coordinates))

  /**
   * Shifts the positions of the coordinates until the coordinate at
   * <code>indexOfFirstCoordinate</code> is first. <p/> If {@code ensureRing} is {@code true}, first
   * and last coordinate of the returned array are equal.
   *
   * @param coordinates
   *   the array to rearrange
   * @param indexOfFirstCoordinate
   *   the index of the coordinate to make first
   * @param ensureRing
   *   flag indicating if returned array should form a ring.
   */
  def scroll(
    coordinates:            Array[Coordinate],
    indexOfFirstCoordinate: Int,
    ensureRing:             Boolean
  ): Unit = {
    val i              = indexOfFirstCoordinate
    if (i <= 0) return
    val newCoordinates = new Array[Coordinate](coordinates.length)
    System.arraycopy(coordinates, 0, newCoordinates, coordinates.length - i, i)
    if (!ensureRing) {
      System.arraycopy(coordinates, i, newCoordinates, 0, coordinates.length - i)
      System.arraycopy(coordinates, 0, newCoordinates, coordinates.length - i, i)
    } else {
      val last = coordinates.length - 1

      // fill in values
      for {
        j <- 0 to last
      } yield newCoordinates(j) = coordinates((i + j) % last)

      // Fix the ring (first == last)
      newCoordinates(last - 1) = newCoordinates(0).copy
    }
  }

  /**
   * Returns the index of <code>coordinate</code> in <code>coordinates</code>. The first position is
   * 0; the second, 1; etc.
   *
   * @param coordinate
   *   the <code>Coordinate</code> to search for
   * @param coordinates
   *   the array to search return the position of <code>coordinate</code>, or -1 if it is not found
   */
  def indexOf(coordinate: Coordinate, coordinates: Array[Coordinate]): Int = {
    var i = 0
    while (i < coordinates.length) {
      if (coordinate == coordinates(i)) return i
      i += 1
    }
    -1
  }

  /**
   * Extracts a subsequence of the input {link Coordinate} array from indices <code>start</code> to
   * <code>end</code> (inclusive). The input indices are clamped to the array size; If the end index
   * is less than the start index, the extracted array will be empty.
   *
   * @param pts
   *   the input array
   * @param start
   *   the index of the start of the subsequence to extract
   * @param end
   *   the index of the end of the subsequence to extract return a subsequence of the input array
   */
  def extract(pts: Array[Coordinate], startArg: Int, endArg: Int): Array[Coordinate] = {
    val start      = MathUtil.clamp(startArg, 0, pts.length)
    val end        = MathUtil.clamp(endArg, -1, pts.length)
    var npts       = end - start + 1
    if (end < 0) npts = 0
    if (start >= pts.length) npts = 0
    if (end < start) npts = 0
    val extractPts = new Array[Coordinate](npts)
    if (npts == 0) return extractPts
    var iPts       = 0
    var i          = start
    while (i <= end) {
      extractPts {
        iPts += 1; iPts - 1
      } = pts(i)
      i += 1
    }
    extractPts
  }

  /**
   * Computes the envelope of the coordinates.
   *
   * @param coordinates
   *   the coordinates to scan return the envelope of the coordinates
   */
  def envelope(coordinates: Array[Coordinate]): Envelope = {
    val env = new Envelope()
    coordinates.foreach(env.expandToInclude)
    env
  }

  /**
   * Extracts the coordinates which intersect an {link Envelope}
   *
   * @param coordinates
   *   the coordinates to scan
   * @param env
   *   the envelope to intersect with return an array of the coordinates which intersect the
   *   envelope
   */
  def intersection(coordinates: Array[Coordinate], env: Envelope): Array[Coordinate] = {

    val coordList = new CoordinateList(Array[Coordinate]())
    coordinates.foreach { c =>
      if (env.intersects(c)) coordList.add(c, allowRepeated = true)
    }
    coordList.toCoordinateArray
  }
}
