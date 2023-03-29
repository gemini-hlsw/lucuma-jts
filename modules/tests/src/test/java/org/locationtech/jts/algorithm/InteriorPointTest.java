/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.List;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTFileReader;
import org.locationtech.jts.io.WKTReader;
//import org.locationtech.jts.util.Stopwatch;

import junit.framework.TestCase;
import junit.textui.TestRunner;
import test.jts.GeometryTestCase;
import test.jts.TestFiles;


public class InteriorPointTest extends GeometryTestCase
{
  public static void main(String args[])
  {
    TestRunner.run(InteriorPointTest.class);
  }

  WKTReader rdr = new WKTReader();

  public InteriorPointTest(String name)
  {
    super(name);
  }

  public void testPolygonZeroArea() {
    checkInteriorPoint(read("POLYGON ((10 10, 10 10, 10 10, 10 10))"), new Coordinate(10, 10));
  }

  public void testAll() throws Exception
  {
    checkInteriorPointFile(TestFiles.getResourceFilePath("world.wkt"));
    //checkInteriorPointFile(TestFiles.getResourceFilePath("africa.wkt"));
    //checkInteriorPointFile("../../../../../data/africa.wkt");
  }

  void checkInteriorPointFile(String file) throws Exception
  {
    WKTFileReader fileRdr = new WKTFileReader(new FileReader(file), rdr);
    checkInteriorPointFile(fileRdr);
  }

  void checkInteriorPointResource(String resource) throws Exception
  {
    InputStream is = this.getClass().getResourceAsStream(resource);
    WKTFileReader fileRdr = new WKTFileReader(new InputStreamReader(is), rdr);
    checkInteriorPointFile(fileRdr);
  }

  private void checkInteriorPointFile(WKTFileReader fileRdr) throws IOException, ParseException
  {
    List<?> polys = fileRdr.read();
    checkInteriorPoint(polys);
  }

  void checkInteriorPoint(List<?> geoms)
  {
    //Stopwatch sw = new Stopwatch();
    for (Iterator<?> i = geoms.iterator(); i.hasNext();) {
      Geometry g = (Geometry) i.next();
      checkInteriorPoint(g);
      //System.out.print(".");
    }
    //System.out.println();
    //System.out.println("  " + sw.getTimeString());
  }

  private void checkInteriorPoint(Geometry g)
  {
    Point ip = g.getInteriorPoint();
    assertTrue(g.contains(ip));
  }

  private void checkInteriorPoint(Geometry g, Coordinate expectedPt)
  {
    Point ip = g.getInteriorPoint();
    assertTrue(ip.getCoordinate().equals2D(expectedPt));
  }

}
