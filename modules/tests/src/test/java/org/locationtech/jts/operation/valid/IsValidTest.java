/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.operation.valid;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.io.WKTReader;

import junit.framework.TestCase;
import junit.textui.TestRunner;



/**
 * @version 1.7
 */
public class IsValidTest extends TestCase {

  private PrecisionModel precisionModel = new PrecisionModel();
  private GeometryFactory geometryFactory = new GeometryFactory(precisionModel, 0);
  WKTReader reader = new WKTReader(geometryFactory);

  public static void main(String args[]) {
    TestRunner.run(IsValidTest.class);
  }

  public IsValidTest(String name) { super(name); }

  public void testInvalidCoordinate() throws Exception
  {
    Coordinate badCoord = new Coordinate(1.0, Double.NaN);
    Coordinate[] pts = { new Coordinate(0.0, 0.0), badCoord };
    Geometry line = geometryFactory.createLineString(pts);
    IsValidOp isValidOp = new IsValidOp(line);
    boolean valid = isValidOp.isValid();
    TopologyValidationError err = isValidOp.getValidationError();
    Coordinate errCoord = err.getCoordinate();

    assertEquals(TopologyValidationError.INVALID_COORDINATE(), err.getErrorType());
    assertTrue(Double.isNaN(errCoord.y()));
    assertEquals(false, valid);
  }

  public void testZeroAreaPolygon() throws Exception {
    Geometry g = reader.read(
          "POLYGON((0 0, 0 0, 0 0, 0 0, 0 0))");
    g.isValid();
    assertTrue(true); //No exception thrown [Jon Aquino]
  }

  public void testLineString() throws Exception {
    Geometry g = reader.read(
          "LINESTRING(0 0, 0 0)");
    g.isValid();
    assertTrue(true); //No exception thrown [Jon Aquino]
  }

}
