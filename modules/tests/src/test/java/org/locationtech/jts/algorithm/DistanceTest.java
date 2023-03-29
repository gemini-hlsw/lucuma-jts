/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import org.locationtech.jts.geom.Coordinate;

import junit.textui.TestRunner;
import test.jts.GeometryTestCase;

public class DistanceTest extends GeometryTestCase {
  public static void main(String args[]) {
    TestRunner.run(DistanceTest.class);
  }

  public DistanceTest(String name) { super(name); }
  
  public void testDistancePointLinePerpendicular() {
    assertEquals(0.5, Distance.pointToLinePerpendicular(
        new Coordinate(0.5, 0.5), new Coordinate(0,0), new Coordinate(1,0)), 0.000001);
    assertEquals(0.5, Distance.pointToLinePerpendicular(
        new Coordinate(3.5, 0.5), new Coordinate(0,0), new Coordinate(1,0)), 0.000001);
    assertEquals(0.707106, Distance.pointToLinePerpendicular(
        new Coordinate(1,0), new Coordinate(0,0), new Coordinate(1,1)), 0.000001);
  }

  public void testDistancePointLine() {
    assertEquals(0.5, Distance.pointToSegment(
        new Coordinate(0.5, 0.5), new Coordinate(0,0), new Coordinate(1,0)), 0.000001);
    assertEquals(1.0, Distance.pointToSegment(
        new Coordinate(2, 0), new Coordinate(0,0), new Coordinate(1,0)), 0.000001);
  }

  public void testDistanceLineLineDisjointCollinear() {
    assertEquals(1.999699, Distance.segmentToSegment(
        new Coordinate(0,0), new Coordinate(9.9, 1.4), 
        new Coordinate(11.88, 1.68), new Coordinate(21.78, 3.08)), 0.000001);
  }
}
