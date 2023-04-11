/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package test.jts.util;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTFileReader;
import org.locationtech.jts.io.WKTReader;

public class IOUtil {
	  public static Geometry read(String wkt)
	  {
		  WKTReader rdr = new WKTReader();
	    try {
	      return rdr.read(wkt);
	    }
	    catch (ParseException ex) {
	      throw new RuntimeException(ex);
	    }
	  }

    public static List readWKT(String[] inputWKT)
    throws ParseException
    {
      ArrayList geometries = new ArrayList();
      for (int i = 0; i < inputWKT.length; i++) {
          geometries.add(IOUtil.reader.read(inputWKT[i]));
      }
      return geometries;
    }

    public static Geometry readWKT(String inputWKT)
    throws ParseException
    {
    	return IOUtil.reader.read(inputWKT);
    }

    public static Collection readWKTFile(String filename) 
    throws IOException, ParseException
    {
      WKTFileReader fileRdr = new WKTFileReader(filename, IOUtil.reader);
      List geoms = fileRdr.read();
      return geoms;
    }

    public static Collection readWKTFile(Reader rdr) 
    throws IOException, ParseException
    {
      WKTFileReader fileRdr = new WKTFileReader(rdr, IOUtil.reader);
      List geoms = fileRdr.read();
      return geoms;
    }

    public static WKTReader reader = new WKTReader();

}
