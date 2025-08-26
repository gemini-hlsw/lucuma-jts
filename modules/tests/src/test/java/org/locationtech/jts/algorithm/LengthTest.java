/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.LineString;

import junit.textui.TestRunner;
import test.jts.GeometryTestCase;

public class LengthTest extends GeometryTestCase {
  public static void main(String args[]) {
    TestRunner.run(LengthTest.class);
  }

  public LengthTest(String name) { super(name); }

  public void testArea() {
    checkLengthOfLine("LINESTRING (100 200, 200 200, 200 100, 100 100, 100 200)", 400.0);
  }
  
  void checkLengthOfLine(String wkt, double expectedLen) {
    LineString ring = (LineString) read(wkt);

    CoordinateSequence pts = ring.getCoordinateSequence();
    double actual = Length.ofLine(pts);
    assertEquals(actual, expectedLen);
  }
}
