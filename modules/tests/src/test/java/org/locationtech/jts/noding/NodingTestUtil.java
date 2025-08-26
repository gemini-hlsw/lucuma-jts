/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.noding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.MultiLineString;
import org.locationtech.jts.geom.util.LineStringExtracter;

public class NodingTestUtil {

  public static Geometry toLines(Collection<NodedSegmentString> nodedList,
      GeometryFactory geomFact) {
    LineString[] lines = new LineString[ nodedList.size() ];
    int i = 0;
    for (NodedSegmentString nss : nodedList) {
      Coordinate[] pts = nss.getCoordinates();
      LineString line = geomFact.createLineString(pts);
      lines[i++] = line;
    }
    if (lines.length == 1) return lines[0];
    return geomFact.createMultiLineString(lines);
  }

  public static List<NodedSegmentString> toSegmentStrings(List<LineString> lines) {
    List<NodedSegmentString> nssList = new ArrayList<NodedSegmentString>();
    for (LineString line : lines) {
      NodedSegmentString nss = new NodedSegmentString(line.getCoordinates(), line);
      nssList.add(nss);
    }
    return nssList;
  }

  /**
   * Runs a noder on one or two sets of input geometries
   * and validates that the result is fully noded.
   *
   * @param geom1 a geometry
   * @param geom2 a geometry, which may be null
   * @param noder the noder to use
   * @return the fully noded linework
   *
   * @throws TopologyException
   */
  public static Geometry nodeValidated(Geometry geom1, Geometry geom2, Noder<NodedSegmentString> noder) {
    List<LineString> lines = LineStringExtracter.getLines(geom1);
    if (geom2 != null) {
      lines.addAll( LineStringExtracter.getLines(geom2) );
    }
    List<NodedSegmentString> ssList = toSegmentStrings(lines);

    Noder<NodedSegmentString> noderValid = new ValidatingNoder<NodedSegmentString>(noder);
    noderValid.computeNodes(ssList);
    Collection<NodedSegmentString> nodedList = noder.getNodedSubstrings();

    Geometry result = toLines(nodedList, geom1.getFactory());
    return result;
  }
}
