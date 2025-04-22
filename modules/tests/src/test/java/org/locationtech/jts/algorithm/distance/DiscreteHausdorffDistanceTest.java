/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm.distance;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;

import junit.framework.TestCase;
import junit.textui.TestRunner;
import test.jts.util.IOUtil;


public class DiscreteHausdorffDistanceTest
extends TestCase
{
  public static void main(String args[]) {
    TestRunner.run(DiscreteHausdorffDistanceTest.class);
  }

  public DiscreteHausdorffDistanceTest(String name) { super(name); }

  public void testLineSegments() throws Exception
  {
    runTest("LINESTRING (0 0, 2 1)", "LINESTRING (0 0, 2 0)", 1.0);
  }

  public void testLineSegments2() throws Exception
  {
    runTest("LINESTRING (0 0, 2 0)", "LINESTRING (0 1, 1 2, 2 1)", 2.0);
  }

  public void testLinePoints() throws Exception
  {
    runTest("LINESTRING (0 0, 2 0)", "MULTIPOINT (0 1, 1 0, 2 1)", 1.0);
  }

  /**
   * Shows effects of limiting HD to vertices
   * Answer is not true Hausdorff distance.
   *
   * @throws Exception
   */
  public void testLinesShowingDiscretenessEffect() throws Exception
  {
    runTest("LINESTRING (130 0, 0 0, 0 150)", "LINESTRING (10 10, 10 150, 130 10)", 14.142135623730951);
    // densifying provides accurate HD
    runTest("LINESTRING (130 0, 0 0, 0 150)", "LINESTRING (10 10, 10 150, 130 10)", 0.5, 70.0);
  }

  private static final double TOLERANCE = 0.00001;

  private void runTest(String wkt1, String wkt2, double expectedDistance)
  throws ParseException
  {
    Geometry g1 = IOUtil.readWKT(wkt1);
    Geometry g2 = IOUtil.readWKT(wkt2);

    double distance = DiscreteHausdorffDistance$.MODULE$.distance(g1, g2);
    assertEquals(distance, expectedDistance, TOLERANCE);
  }
  private void runTest(String wkt1, String wkt2, double densifyFrac, double expectedDistance)
  throws ParseException
  {
    Geometry g1 = IOUtil.readWKT(wkt1);
    Geometry g2 = IOUtil.readWKT(wkt2);

    double distance = DiscreteHausdorffDistance$.MODULE$.distance(g1, g2, densifyFrac);
    assertEquals(distance, expectedDistance, TOLERANCE);
  }

}
