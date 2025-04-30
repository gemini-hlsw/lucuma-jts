/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;

import junit.framework.TestCase;
import junit.textui.TestRunner;
import test.jts.GeometryTestCase;

/**
 * Tests {@link PointLocation#isOnLine(Coordinate, Coordinate[])}.
 * 
 * @version 1.15
 */
public class PointLocationOnLineTest extends GeometryTestCase {
  public static void main(String args[]) {
    TestRunner.run(PointLocationOnLineTest.class);
  }

  public PointLocationOnLineTest(String name) {
    super(name);
  }

  public void testOnVertex() throws Exception {
    checkOnLine(20, 20, "LINESTRING (0 00, 20 20, 30 30)", true);
  }

  public void testOnSegment() throws Exception {
    checkOnLine(10, 10, "LINESTRING (0 0, 20 20, 0 40)", true);
    checkOnLine(10, 30, "LINESTRING (0 0, 20 20, 0 40)", true);
  }

  public void testNotOnLine() throws Exception {
    checkOnLine(0, 100, "LINESTRING (10 10, 20 10, 30 10)", false);
  }

  void checkOnLine(double x, double y, String wktLine, boolean expected) {
    LineString line = (LineString) read(wktLine);
    assertTrue(expected == PointLocation.isOnLine(new Coordinate(x,y), line.getCoordinates()));
    
    assertTrue(expected == PointLocation.isOnLine(new Coordinate(x,y), line.getCoordinateSequence()));
  }

}
