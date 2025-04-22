// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

/**
 * Defines a rectangular region of the 2D coordinate plane. It is often used to represent the
 * bounding box of a {link Geometry}, e.g. the minimum and maximum x and y values of the {link
 * Coordinate}s. <p> Envelopes support infinite or half-infinite regions, by using the values of
 * <code>Double.POSITIVE_INFINITY</code> and <code>Double.NEGATIVE_INFINITY</code>. Envelope objects
 * may have a null value. <p> When Envelope objects are created or initialized, the supplies extent
 * values are automatically sorted into the correct order.
 *
 * @version 1.7
 */
@SerialVersionUID(5873921885273102420L)
object Envelope {

  /**
   * Test the point q to see whether it intersects the Envelope defined by p1-p2
   *
   * @param p1
   *   one extremal point of the envelope
   * @param p2
   *   another extremal point of the envelope
   * @param q
   *   the point to test for intersection return <code>true</code> if q intersects the envelope
   *   p1-p2
   */
  def intersects(p1: Coordinate, p2: Coordinate, q: Coordinate): Boolean = { // OptimizeIt shows that Math#min and Math#max here are a bottleneck.
    // Replace with direct comparisons. [Jon Aquino]
    if (
      ((q.x >= (if (p1.x < p2.x) p1.x
                else p2.x)) && (q.x <= (if (p1.x > p2.x) p1.x
                                        else p2.x))) && ((q.y >= (if (p1.y < p2.y) p1.y
                                                                  else
                                                                    p2.y)) && (q.y <= (if (
                                                                                         p1.y > p2.y
                                                                                       ) p1.y
                                                                                       else p2.y)))
    ) return true
    false
  }

  /**
   * Tests whether the envelope defined by p1-p2 and the envelope defined by q1-q2 intersect.
   *
   * @param p1
   *   one extremal point of the envelope P
   * @param p2
   *   another extremal point of the envelope P
   * @param q1
   *   one extremal point of the envelope Q
   * @param q2
   *   another extremal point of the envelope Q return <code>true</code> if Q intersects P
   */
  def intersects(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate): Boolean = {
    var minq = Math.min(q1.x, q2.x)
    var maxq = Math.max(q1.x, q2.x)
    var minp = Math.min(p1.x, p2.x)
    var maxp = Math.max(p1.x, p2.x)
    if (minp > maxq) return false
    if (maxp < minq) return false
    minq = Math.min(q1.y, q2.y)
    maxq = Math.max(q1.y, q2.y)
    minp = Math.min(p1.y, p2.y)
    maxp = Math.max(p1.y, p2.y)
    if (minp > maxq) return false
    if (maxp < minq) return false
    true
  }
}

@SerialVersionUID(5873921885273102420L)
class Envelope() extends Comparable[Envelope] with Serializable {

  override def hashCode: Int = { // Algorithm from Effective Java by Joshua Bloch [Jon Aquino]
    var result = 17
    result = 37 * result + Coordinate.hashCode(minx)
    result = 37 * result + Coordinate.hashCode(maxx)
    result = 37 * result + Coordinate.hashCode(miny)
    result = 37 * result + Coordinate.hashCode(maxy)
    result
  }

  /**
   * the minimum x-coordinate
   */
  private var minx = .0

  /**
   * the maximum x-coordinate
   */
  private var maxx = .0

  /**
   * the minimum y-coordinate
   */
  private var miny = .0

  /**
   * the maximum y-coordinate
   */
  private var maxy = .0
  init()

  /**
   * Creates an <code>Envelope</code> for a region defined by maximum and minimum values.
   *
   * @param x1
   *   the first x-value
   * @param x2
   *   the second x-value
   * @param y1
   *   the first y-value
   * @param y2
   *   the second y-value
   */
  def this(x1: Double, x2: Double, y1: Double, y2: Double) = {
    this()
    init(x1, x2, y1, y2)
  }

  /**
   * Creates an <code>Envelope</code> for a region defined by two Coordinates.
   *
   * @param p1
   *   the first Coordinate
   * @param p2
   *   the second Coordinate
   */
  def this(p1: Coordinate, p2: Coordinate) = {
    this()
    init(p1.x, p2.x, p1.y, p2.y)
  }

  /**
   * Creates an <code>Envelope</code> for a region defined by a single Coordinate.
   *
   * @param p
   *   the Coordinate
   */
  def this(p: Coordinate) = {
    this()
    init(p.x, p.x, p.y, p.y)
  }

  /**
   * Create an <code>Envelope</code> from an existing Envelope.
   *
   * @param env
   *   the Envelope to initialize from
   */
  def this(env: Envelope) = {
    this()
    init(env)
  }

  /**
   * Initialize to a null <code>Envelope</code>.
   */
  def init(): Unit = setToNull()

  /**
   * Initialize an <code>Envelope</code> for a region defined by maximum and minimum values.
   *
   * @param x1
   *   the first x-value
   * @param x2
   *   the second x-value
   * @param y1
   *   the first y-value
   * @param y2
   *   the second y-value
   */
  def init(x1: Double, x2: Double, y1: Double, y2: Double): Unit = {
    if (x1 < x2) {
      minx = x1
      maxx = x2
    } else {
      minx = x2
      maxx = x1
    }
    if (y1 < y2) {
      miny = y1
      maxy = y2
    } else {
      miny = y2
      maxy = y1
    }
  }

  /**
   * Creates a copy of this envelope object.
   *
   * return a copy of this envelope
   */
  def copy = new Envelope(this)

  /**
   * Initialize an <code>Envelope</code> to a region defined by two Coordinates.
   *
   * @param p1
   *   the first Coordinate
   * @param p2
   *   the second Coordinate
   */
  def init(p1: Coordinate, p2: Coordinate): Unit = init(p1.x, p2.x, p1.y, p2.y)

  /**
   * Initialize an <code>Envelope</code> to a region defined by a single Coordinate.
   *
   * @param p
   *   the coordinate
   */
  def init(p: Coordinate): Unit = init(p.x, p.x, p.y, p.y)

  /**
   * Initialize an <code>Envelope</code> from an existing Envelope.
   *
   * @param env
   *   the Envelope to initialize from
   */
  def init(env: Envelope): Unit = {
    this.minx = env.minx
    this.maxx = env.maxx
    this.miny = env.miny
    this.maxy = env.maxy
  }

  /**
   * Makes this <code>Envelope</code> a "null" envelope, that is, the envelope of the empty
   * geometry.
   */
  def setToNull(): Unit = {
    minx = 0
    maxx = -1
    miny = 0
    maxy = -1
  }

  /**
   * Returns <code>true</code> if this <code>Envelope</code> is a "null" envelope.
   *
   * return <code>true</code> if this <code>Envelope</code> is uninitialized or is the envelope of
   * the empty geometry.
   */
  def isNull: Boolean = maxx < minx

  /**
   * Returns the difference between the maximum and minimum x values.
   *
   * return max x - min x, or 0 if this is a null <code>Envelope</code>
   */
  def getWidth: Double = {
    if (isNull) return 0
    maxx - minx
  }

  /**
   * Returns the difference between the maximum and minimum y values.
   *
   * return max y - min y, or 0 if this is a null <code>Envelope</code>
   */
  def getHeight: Double = {
    if (isNull) return 0
    maxy - miny
  }

  /**
   * Gets the length of the diameter (diagonal) of the envelope.
   *
   * return the diameter length
   */
  def getDiameter: Double = {
    if (isNull) return 0
    val w = getWidth
    val h = getHeight
    Math.sqrt(w * w + h * h)
  }

  /**
   * Returns the <code>Envelope</code>s minimum x-value. min x &gt; max x indicates that this is a
   * null <code>Envelope</code>.
   *
   * return the minimum x-coordinate
   */
  def getMinX: Double = minx

  /**
   * Returns the <code>Envelope</code>s maximum x-value. min x &gt; max x indicates that this is a
   * null <code>Envelope</code>.
   *
   * return the maximum x-coordinate
   */
  def getMaxX: Double = maxx

  /**
   * Returns the <code>Envelope</code>s minimum y-value. min y &gt; max y indicates that this is a
   * null <code>Envelope</code>.
   *
   * return the minimum y-coordinate
   */
  def getMinY: Double = miny

  /**
   * Returns the <code>Envelope</code>s maximum y-value. min y &gt; max y indicates that this is a
   * null <code>Envelope</code>.
   *
   * return the maximum y-coordinate
   */
  def getMaxY: Double = maxy

  /**
   * Gets the area of this envelope.
   *
   * return the area of the envelope return 0.0 if the envelope is null
   */
  def getArea: Double = getWidth * getHeight

  /**
   * Gets the minimum extent of this envelope across both dimensions.
   *
   * return the minimum extent of this envelope
   */
  def minExtent: Double = {
    if (isNull) return 0.0
    val w = getWidth
    val h = getHeight
    if (w < h) return w
    h
  }

  /**
   * Gets the maximum extent of this envelope across both dimensions.
   *
   * return the maximum extent of this envelope
   */
  def maxExtent: Double = {
    if (isNull) return 0.0
    val w = getWidth
    val h = getHeight
    if (w > h) return w
    h
  }

  /**
   * Enlarges this <code>Envelope</code> so that it contains the given {link Coordinate}. Has no
   * effect if the point is already on or within the envelope.
   *
   * @param p
   *   the Coordinate to expand to include
   */
  def expandToInclude(p: Coordinate): Unit = expandToInclude(p.x, p.y)

  /**
   * Expands this envelope by a given distance in all directions. Both positive and negative
   * distances are supported.
   *
   * @param distance
   *   the distance to expand the envelope
   */
  def expandBy(distance: Double): Unit = expandBy(distance, distance)

  /**
   * Expands this envelope by a given distance in all directions. Both positive and negative
   * distances are supported.
   *
   * @param deltaX
   *   the distance to expand the envelope along the the X axis
   * @param deltaY
   *   the distance to expand the envelope along the the Y axis
   */
  def expandBy(deltaX: Double, deltaY: Double): Unit = {
    if (isNull) return
    minx -= deltaX
    maxx += deltaX
    miny -= deltaY
    maxy += deltaY
    // check for envelope disappearing
    if (minx > maxx || miny > maxy) setToNull()
  }

  /**
   * Enlarges this <code>Envelope</code> so that it contains the given point. Has no effect if the
   * point is already on or within the envelope.
   *
   * @param x
   *   the value to lower the minimum x to or to raise the maximum x to
   * @param y
   *   the value to lower the minimum y to or to raise the maximum y to
   */
  def expandToInclude(x: Double, y: Double): Unit = if (isNull) {
    minx = x
    maxx = x
    miny = y
    maxy = y
  } else {
    if (x < minx) minx = x
    if (x > maxx) maxx = x
    if (y < miny) miny = y
    if (y > maxy) maxy = y
  }

  /**
   * Enlarges this <code>Envelope</code> so that it contains the <code>other</code> Envelope. Has no
   * effect if <code>other</code> is wholly on or within the envelope.
   *
   * @param other
   *   the <code>Envelope</code> to expand to include
   */
  def expandToInclude(other: Envelope): Unit = {
    if (other.isNull) return
    if (isNull) {
      minx = other.getMinX
      maxx = other.getMaxX
      miny = other.getMinY
      maxy = other.getMaxY
    } else {
      if (other.minx < minx) minx = other.minx
      if (other.maxx > maxx) maxx = other.maxx
      if (other.miny < miny) miny = other.miny
      if (other.maxy > maxy) maxy = other.maxy
    }
  }

  /**
   * Translates this envelope by given amounts in the X and Y direction.
   *
   * @param transX
   *   the amount to translate along the X axis
   * @param transY
   *   the amount to translate along the Y axis
   */
  def translate(transX: Double, transY: Double): Unit = {
    if (isNull) return
    init(getMinX + transX, getMaxX + transX, getMinY + transY, getMaxY + transY)
  }

  /**
   * Computes the coordinate of the centre of this envelope (as long as it is non-null
   *
   * return the centre coordinate of this envelope <code>null</code> if the envelope is null
   */
  def centre: Coordinate = {
    if (isNull) return null
    new Coordinate((getMinX + getMaxX) / 2.0, (getMinY + getMaxY) / 2.0)
  }

  /**
   * Computes the intersection of two {link Envelope}s.
   *
   * @param env
   *   the envelope to intersect with return a new Envelope representing the intersection of the
   *   envelopes (this will be the null envelope if either argument is null, or they do not
   *   intersect
   */
  def intersection(env: Envelope): Envelope = {
    if (isNull || env.isNull || !intersects(env)) return new Envelope
    val intMinX =
      if (minx > env.minx) minx
      else env.minx
    val intMinY =
      if (miny > env.miny) miny
      else env.miny
    val intMaxX =
      if (maxx < env.maxx) maxx
      else env.maxx
    val intMaxY =
      if (maxy < env.maxy) maxy
      else env.maxy
    new Envelope(intMinX, intMaxX, intMinY, intMaxY)
  }

  /**
   * Tests if the region defined by <code>other</code> intersects the region of this
   * <code>Envelope</code>.
   *
   * @param other
   *   the <code>Envelope</code> which this <code>Envelope</code> is being checked for intersecting
   *   return <code>true</code> if the <code>Envelope</code>s intersect
   */
  def intersects(other: Envelope): Boolean = {
    if (isNull || other.isNull) return false
    !(other.minx > maxx || other.maxx < minx || other.miny > maxy || other.maxy < miny)
  }

  /**
   * Tests if the extent defined by two extremal points intersects the extent of this
   * <code>Envelope</code>.
   *
   * @param a
   *   a point
   * @param b
   *   another point return <code>true</code> if the extents intersect
   */
  def intersects(a: Coordinate, b: Coordinate): Boolean = {
    if (isNull) return false
    val envminx =
      if (a.x < b.x) a.x
      else b.x
    if (envminx > maxx) return false
    val envmaxx =
      if (a.x > b.x) a.x
      else b.x
    if (envmaxx < minx) return false
    val envminy =
      if (a.y < b.y) a.y
      else b.y
    if (envminy > maxy) return false
    val envmaxy =
      if (a.y > b.y) a.y
      else b.y
    if (envmaxy < miny) return false
    true
  }

  /**
   * Tests if the region defined by <code>other</code> is disjoint from the region of this
   * <code>Envelope</code>.
   *
   * @param other
   *   the <code>Envelope</code> being checked for disjointness return <code>true</code> if the
   *   <code>Envelope</code>s are disjoint
   * @see
   *   #intersects(Envelope)
   */
  def disjoint(other: Envelope): Boolean = {
    if (isNull || other.isNull) return true
    other.minx > maxx || other.maxx < minx || other.miny > maxy || other.maxy < miny
  }

  /**
   * @deprecated
   *   Use #intersects instead. In the future, #overlaps may be changed to be a true overlap check;
   *   that is, whether the intersection is two-dimensional.
   */
  def overlaps(other: Envelope): Boolean = intersects(other)

  /**
   * Tests if the point <code>p</code> intersects (lies inside) the region of this
   * <code>Envelope</code>.
   *
   * @param p
   *   the <code>Coordinate</code> to be tested return <code>true</code> if the point intersects
   *   this <code>Envelope</code>
   */
  def intersects(p: Coordinate): Boolean = intersects(p.x, p.y)

  /**
   * @deprecated
   *   Use #intersects instead.
   */
  def overlaps(p: Coordinate): Boolean = intersects(p)

  /**
   * Check if the point <code>(x, y)</code> intersects (lies inside) the region of this
   * <code>Envelope</code>.
   *
   * @param x
   *   the x-ordinate of the point
   * @param y
   *   the y-ordinate of the point return <code>true</code> if the point overlaps this
   *   <code>Envelope</code>
   */
  def intersects(x: Double, y: Double): Boolean = {
    if (isNull) return false
    !(x > maxx || x < minx || y > maxy || y < miny)
  }

  def overlaps(x: Double, y: Double): Boolean = intersects(x, y)

  /**
   * Tests if the <code>Envelope other</code> lies wholely inside this <code>Envelope</code>
   * (inclusive of the boundary). <p> Note that this is <b>not</b> the same definition as the SFS
   * <tt>contains</tt>, which would exclude the envelope boundary.
   *
   * @param other
   *   the <code>Envelope</code> to check return true if <code>other</code> is contained in this
   *   <code>Envelope</code>
   * @see
   *   #covers(Envelope)
   */
  def contains(other: Envelope): Boolean = covers(other)

  /**
   * Tests if the given point lies in or on the envelope. <p> Note that this is <b>not</b> the same
   * definition as the SFS <tt>contains</tt>, which would exclude the envelope boundary.
   *
   * @param p
   *   the point which this <code>Envelope</code> is being checked for containing return
   *   <code>true</code> if the point lies in the interior or on the boundary of this
   *   <code>Envelope</code>.
   * @see
   *   #covers(Coordinate)
   */
  def contains(p: Coordinate): Boolean = covers(p)

  /**
   * Tests if the given point lies in or on the envelope. <p> Note that this is <b>not</b> the same
   * definition as the SFS <tt>contains</tt>, which would exclude the envelope boundary.
   *
   * @param x
   *   the x-coordinate of the point which this <code>Envelope</code> is being checked for
   *   containing
   * @param y
   *   the y-coordinate of the point which this <code>Envelope</code> is being checked for
   *   containing return <code>true</code> if <code>(x, y)</code> lies in the interior or on the
   *   boundary of this <code>Envelope</code>.
   * @see
   *   #covers(double, double)
   */
  def contains(x: Double, y: Double): Boolean = covers(x, y)

  /**
   * Tests if the given point lies in or on the envelope.
   *
   * @param x
   *   the x-coordinate of the point which this <code>Envelope</code> is being checked for
   *   containing
   * @param y
   *   the y-coordinate of the point which this <code>Envelope</code> is being checked for
   *   containing return <code>true</code> if <code>(x, y)</code> lies in the interior or on the
   *   boundary of this <code>Envelope</code>.
   */
  def covers(x: Double, y: Double): Boolean = {
    if (isNull) return false
    x >= minx && x <= maxx && y >= miny && y <= maxy
  }

  /**
   * Tests if the given point lies in or on the envelope.
   *
   * @param p
   *   the point which this <code>Envelope</code> is being checked for containing return
   *   <code>true</code> if the point lies in the interior or on the boundary of this
   *   <code>Envelope</code>.
   */
  def covers(p: Coordinate): Boolean = covers(p.x, p.y)

  /**
   * Tests if the <code>Envelope other</code> lies wholely inside this <code>Envelope</code>
   * (inclusive of the boundary).
   *
   * @param other
   *   the <code>Envelope</code> to check return true if this <code>Envelope</code> covers the
   *   <code>other</code>
   */
  def covers(other: Envelope): Boolean = {
    if (isNull || other.isNull) return false
    other.getMinX >= minx && other.getMaxX <= maxx && other.getMinY >= miny && other.getMaxY <= maxy
  }

  /**
   * Computes the distance between this and another <code>Envelope</code>. The distance between
   * overlapping Envelopes is 0. Otherwise, the distance is the Euclidean distance between the
   * closest points.
   */
  def distance(env: Envelope): Double = {
    if (intersects(env)) return 0
    var dx = 0.0
    if (maxx < env.minx) dx = env.minx - maxx
    else if (minx > env.maxx) dx = minx - env.maxx
    var dy = 0.0
    if (maxy < env.miny) dy = env.miny - maxy
    else if (miny > env.maxy) dy = miny - env.maxy
    // if either is zero, the envelopes overlap either vertically or horizontally
    if (dx == 0.0) return dy
    if (dy == 0.0) return dx
    Math.sqrt(dx * dx + dy * dy)
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Envelope]) return false
    val otherEnvelope = other.asInstanceOf[Envelope]
    if (isNull) return otherEnvelope.isNull
    maxx == otherEnvelope.getMaxX && maxy == otherEnvelope.getMaxY && minx == otherEnvelope.getMinX && miny == otherEnvelope.getMinY
  }

  override def toString: String = "Env[" + minx + " : " + maxx + ", " + miny + " : " + maxy + "]"

  /**
   * Compares two envelopes using lexicographic ordering. The ordering comparison is based on the
   * usual numerical comparison between the sequence of ordinates. Null envelopes are less than all
   * non-null envelopes.
   *
   * @param o
   *   an Envelope object
   */
  override def compareTo(o: Envelope): Int = {
    val env = o
    // compare nulls if present
    if (isNull) {
      if (env.isNull) return 0
      return -1
    } else if (env.isNull) return 1
    // compare based on numerical ordering of ordinates
    if (minx < env.minx) return -1
    if (minx > env.minx) return 1
    if (miny < env.miny) return -1
    if (miny > env.miny) return 1
    if (maxx < env.maxx) return -1
    if (maxx > env.maxx) return 1
    if (maxy < env.maxy) return -1
    if (maxy > env.maxy) return 1
    0
  }
}
