/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;

import junit.textui.TestRunner;
import test.jts.GeometryTestCase;

/**
 * Tests Orientation isCCW
 * @version 1.7
 */
public class OrientationIsCCWTest extends GeometryTestCase {

  public static void main(String args[]) {
    TestRunner.run(OrientationIsCCWTest.class);
  }

  public OrientationIsCCWTest(String name) { super(name); }

  public void testTooFewPoints() {
    Coordinate[] pts = new Coordinate[] {
      new Coordinate(0, 0),
      new Coordinate(1, 1),
      new Coordinate(2, 2)
    };
    boolean hasError = false;
    try {
      boolean isCCW = Orientation.isCCW(pts);
    }
    catch (IllegalArgumentException ex) {
      hasError = true;
    }
    assertTrue(hasError);
  }
  
  public void testCCW() {
    checkCCW(true, "POLYGON ((60 180, 140 120, 100 180, 140 240, 60 180))");
  }
  
  public void testRingCW() {
    checkCCW(false, "POLYGON ((60 180, 140 240, 100 180, 140 120, 60 180))");
  }
  
  public void testCCWSmall() {
    checkCCW(true, "POLYGON ((1 1, 9 1, 5 9, 1 1))");
  }
  
  public void testDuplicateTopPoint() {
    checkCCW(true, "POLYGON ((60 180, 140 120, 100 180, 140 240, 140 240, 60 180))");
  }
  
  public void testFlatTopSegment() {
    checkCCW(false, "POLYGON ((100 200, 200 200, 200 100, 100 100, 100 200))");
  }
  
  public void testFlatMultipleTopSegment() {
    checkCCW(false, "POLYGON ((100 200, 127 200, 151 200, 173 200, 200 200, 100 100, 100 200))");
  }
  
  public void testDegenerateRingHorizontal() {
    checkCCW(false, "POLYGON ((100 200, 100 200, 200 200, 100 200))");
  }
  
  public void testDegenerateRingAngled() {
    checkCCW(false, "POLYGON ((100 100, 100 100, 200 200, 100 100))");
  }
  
  public void testDegenerateRingVertical() {
    checkCCW(false, "POLYGON ((200 100, 200 100, 200 200, 200 100))");
  }
  
  /**
   * This case is an invalid ring, so answer is a default value
   */
  public void testTopAngledSegmentCollapse() {
    checkCCW(false, "POLYGON ((10 20, 61 20, 20 30, 50 60, 10 20))");
  }
  
  public void testABATopFlatSegmentCollapse() {
    checkCCW(true, "POLYGON ((71 0, 40 40, 70 40, 40 40, 20 0, 71 0))");
  }
  
  public void testABATopFlatSegmentCollapseMiddleStart() {
    checkCCW(true, "POLYGON ((90 90, 50 90, 10 10, 90 10, 50 90, 90 90))");
  }
  
  public void testMultipleTopFlatSegmentCollapseSinglePoint() {
    checkCCW(true, "POLYGON ((100 100, 200 100, 150 200, 170 200, 200 200, 100 200, 150 200, 100 100))");
  }
  
  public void testMultipleTopFlatSegmentCollapseFlatTop() {
    checkCCW(true, "POLYGON ((10 10, 90 10, 70 70, 90 70, 10 70, 30 70, 50 70, 10 10))");
  }
  
  /**
   * Signed-area orientation returns orientation of largest enclosed area
   */
  public void testBowTieByArea() {
    checkCCWArea(true, "POLYGON ((10 10, 50 10, 25 35, 35 35, 10 10))");
  }
  
  private void checkCCW(boolean expectedCCW, String wkt) {
    Coordinate[] pts2x = getCoordinates(wkt);
    assertEquals("Coordinate array isCCW: ", expectedCCW, Orientation.isCCW(pts2x) );
    CoordinateSequence seq2x = getCoordinateSequence(wkt);
    assertEquals("CoordinateSequence isCCW: ", expectedCCW, Orientation.isCCW(seq2x) );
  }

  private void checkCCWArea(boolean expectedCCW, String wkt) {
    Coordinate[] pts = getCoordinates(wkt);
    assertEquals("Coordinate array isCCW: ", expectedCCW, Orientation.isCCWArea(pts) );
  }
  
  private Coordinate[] getCoordinates(String wkt)
  {
    Geometry geom = read(wkt);
    return geom.getCoordinates();
  }
  private CoordinateSequence getCoordinateSequence(String wkt)
  {
    Geometry geom = read(wkt);
    if (geom.getGeometryType() != "Polygon")
      throw new IllegalArgumentException("wkt");
    Polygon poly = (Polygon)geom;
    return ((Polygon) geom).getExteriorRing().getCoordinateSequence();
  }
}
