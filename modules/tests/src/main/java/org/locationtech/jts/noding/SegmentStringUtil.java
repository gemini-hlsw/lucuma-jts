/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

/*
 * Copyright (c) 2016 Vivid Solutions.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.locationtech.jts.noding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.MultiLineString;
import org.locationtech.jts.geom.util.LinearComponentExtracter;
import org.locationtech.jts.noding.NodedSegmentString;
import org.locationtech.jts.noding.SegmentString;


/**
 * Utility methods for processing {@link SegmentString}s.
 * 
 * @author Martin Davis
 *
 */
public class SegmentStringUtil 
{
  /**
   * Extracts all linear components from a given {@link Geometry}
   * to {@link SegmentString}s.
   * The SegmentString data item is set to be the source Geometry.
   * 
   * @param geom the geometry to extract from
   * @return a List of SegmentStrings
   */
  public static List<NodedSegmentString> extractSegmentStrings(Geometry geom)
  {
    return extractNodedSegmentStrings(geom);
  }

  /**
   * Extracts all linear components from a given {@link Geometry}
   * to {@link SegmentString}s.
   * The SegmentString data item is set to be the source Geometry.
   * 
   * @param geom the geometry to extract from
   * @return a List of SegmentStrings
   */
  public static List<NodedSegmentString> extractNodedSegmentStrings(Geometry geom)
  {
    List<NodedSegmentString> segStr = new ArrayList<>();
    List<Geometry> lines = LinearComponentExtracter.getLines(geom);
    for (Iterator<Geometry> i = lines.iterator(); i.hasNext(); ) {
      LineString line = (LineString) i.next();
      Coordinate[] pts = line.getCoordinates();
      segStr.add(new NodedSegmentString(pts, geom));
    }
    return segStr;
  }

  /**
   * Converts a collection of {@link SegmentString}s into a {@link Geometry}.
   * The geometry will be either a {@link LineString} or a {@link MultiLineString} (possibly empty).
   *
   * @param segStrings a collection of SegmentStrings
   * @return a LineString or MultiLineString
   */
  public static Geometry toGeometry(Collection<SegmentString> segStrings, GeometryFactory geomFact)
  {
    LineString[] lines = new LineString[segStrings.size()];
    int index = 0;
    for (Iterator<SegmentString> i = segStrings.iterator(); i.hasNext(); ) {
      SegmentString ss = i.next();
      LineString line = geomFact.createLineString(ss.getCoordinates());
      lines[index++] = line;
    }
    if (lines.length == 1) return lines[0];
    return geomFact.createMultiLineString(lines);
  }

  public static String toString(List<SegmentString> segStrings)
  {
	StringBuffer buf = new StringBuffer();
    for (Iterator<SegmentString> i = segStrings.iterator(); i.hasNext(); ) {
        SegmentString segStr = i.next();
        buf.append(segStr.toString());
        buf.append("\n");
        
    }
    return buf.toString();
  }
}
