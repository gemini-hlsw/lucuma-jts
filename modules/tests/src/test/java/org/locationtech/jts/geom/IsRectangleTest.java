/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.geom;

import org.locationtech.jts.io.WKTReader;

import junit.framework.TestCase;
import junit.textui.TestRunner;


/**
 * Test named predicate short-circuits
 */
/**
 * @version 1.7
 */
public class IsRectangleTest extends TestCase {

  WKTReader rdr = new WKTReader();

  public static void main(String args[]) {
    TestRunner.run(IsRectangleTest.class);
  }

  public IsRectangleTest(String name) { super(name); }


  public void testValidRectangle() throws Exception
  {
    assertTrue(isRectangle("POLYGON ((0 0, 0 100, 100 100, 100 0, 0 0))"));
  }

  public void testValidRectangle2() throws Exception
  {
    assertTrue(isRectangle("POLYGON ((0 0, 0 200, 100 200, 100 0, 0 0))"));
  }

  public void testRectangleWithHole() throws Exception
  {
    assertTrue(! isRectangle("POLYGON ((0 0, 0 100, 100 100, 100 0, 0 0), (10 10, 10 90, 90 90, 90 10, 10 10) ))"));
  }

  public void testNotRectilinear() throws Exception
  {
    assertTrue(! isRectangle("POLYGON ((0 0, 0 100, 99 100, 100 0, 0 0))"));
  }

  public void testTooManyPoints() throws Exception
  {
    assertTrue(! isRectangle("POLYGON ((0 0, 0 100, 100 50, 100 100, 100 0, 0 0))"));
  }

  public void testTooFewPoints() throws Exception
  {
    assertTrue(! isRectangle("POLYGON ((0 0, 0 100, 100 0, 0 0))"));
  }

  public void testRectangularLinestring() throws Exception
  {
    assertTrue(! isRectangle("LINESTRING (0 0, 0 100, 100 100, 100 0, 0 0)"));
  }

  public void testPointsInWrongOrder() throws Exception
  {
    assertTrue(! isRectangle("POLYGON ((0 0, 0 100, 100 0, 100 100, 0 0))"));
  }

  public boolean isRectangle(String wkt)
      throws Exception
  {
    Geometry a = rdr.read(wkt);
    return a.isRectangle();
  }
}
