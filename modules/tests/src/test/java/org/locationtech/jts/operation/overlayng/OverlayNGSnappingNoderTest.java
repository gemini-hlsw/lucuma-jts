/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.operation.overlayng;

import static org.locationtech.jts.operation.overlayng.OverlayNG.INTERSECTION;
import static org.locationtech.jts.operation.overlayng.OverlayNG.UNION;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.noding.Noder;
import org.locationtech.jts.noding.SegmentString;
import org.locationtech.jts.noding.ValidatingNoder;
import org.locationtech.jts.noding.snap.SnappingNoder;

import junit.textui.TestRunner;
import test.jts.GeometryTestCase;

/**
 * Tests {@link OverlayNG} using the {@link SnappingNoder}.
 *
 * @author mdavis
 *
 */
public class OverlayNGSnappingNoderTest extends GeometryTestCase {

  public static void main(String args[]) {
    TestRunner.run(OverlayNGSnappingNoderTest.class);
  }

  public OverlayNGSnappingNoderTest(String name) { super(name); }

  public void testRectanglesOneAjarUnion() {
    Geometry a = read("POLYGON ((10 10, 10 5, 5 5, 5 10, 10 10))");
    Geometry b = read("POLYGON ((10 15, 15 15, 15 7, 10.01 7, 10 15))");
    Geometry expected = read("POLYGON ((5 5, 5 10, 10 10, 10 15, 15 15, 15 7, 10.01 7, 10 5, 5 5))");
    checkEqual(expected, union(a, b, 1));
  }

  public void testRectanglesBothAjarUnion() {
    Geometry a = read("POLYGON ((10.01 10, 10 5, 5 5, 5 10, 10.01 10))");
    Geometry b = read("POLYGON ((10 15, 15 15, 15 7, 10.01 7, 10 15))");
    Geometry expected = read("POLYGON ((5 5, 5 10, 10.01 10, 10 15, 15 15, 15 7, 10.01 7, 10 5, 5 5))");
    checkEqual(expected, union(a, b, 1));
  }

  public void testRandomUnion() {
    Geometry a = read("POLYGON ((85.55954154387994 100, 92.87214039753759 100, 94.7254728121147 100, 98.69765702432045 96.38825885127041, 85.55954154387994 100))");
    Geometry b = read("POLYGON ((80.20688423699171 99.99999999999999, 100.00000000000003 99.99999999999997, 100.00000000000003 88.87471526860915, 80.20688423699171 99.99999999999999))");
    Geometry expected = read("POLYGON ((80.20688423699171 99.99999999999999, 85.55954154387994 100, 92.87214039753759 100, 94.7254728121147 100, 100.00000000000003 99.99999999999997, 100.00000000000003 88.87471526860915, 80.20688423699171 99.99999999999999))");
    checkEqual(expected, union(a, b, 0.00000001));
  }

  public void testTrianglesBSegmentsDisplacedSmallTolUnion() {
    Geometry a = read("POLYGON ((100 200, 200 0, 300 200, 100 200))");
    Geometry b = read("POLYGON ((150 200.01, 200 200.01, 260 200.01, 200 100, 150 200.01))");
    Geometry expected = read("POLYGON ((150 200.01, 200 200.01, 260 200.01, 300 200, 200 0, 100 200, 150 200.01))");
    checkEqual(expected, union(a, b, 0.01));
  }
  public void testTrianglesBSegmentsDisplacedUnion() {
    Geometry a = read("POLYGON ((100 200, 200 0, 300 200, 100 200))");
    Geometry b = read("POLYGON ((150 200.01, 200 200.01, 260 200.01, 200 100, 150 200.01))");
    Geometry expected = read("POLYGON ((100 200, 150 200.01, 200 200.01, 260 200.01, 300 200, 200 0, 100 200))");
    checkEqual(expected, union(a, b, 0.1));
  }

  public static Geometry union(Geometry a, Geometry b, double tolerance) {
    Noder<SegmentString> noder = getNoder(tolerance);
    return OverlayNG.overlay(a, b, UNION(), null, noder );
  }

  private static Noder<SegmentString> getNoder(double tolerance) {
    SnappingNoder snapNoder = new SnappingNoder(tolerance);
    return new ValidatingNoder(snapNoder);
  }

}
