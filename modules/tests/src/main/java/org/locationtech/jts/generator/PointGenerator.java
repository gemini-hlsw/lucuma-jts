/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.generator;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;

/**
 * 
 * Creates a point based on the bounding box. 
 * 
 * This implementation returns the centroid.
 *
 * @author David Zwiers, Vivid Solutions. 
 */
public class PointGenerator extends GeometryGenerator {

	/**
	 * @see org.locationtech.jts.generator.GeometryGenerator#create()
	 * @throws NullPointerException when either the Geometry Factory, or the Bounding Box are undefined.
	 */
	public Geometry create() {
		if(geometryFactory == null){
			throw new NullPointerException("GeometryFactory is not declared");
		}
		if(boundingBox == null || boundingBox.isNull()){
			throw new NullPointerException("Bounding Box is not declared");
		}
		
		Point p = geometryFactory.toGeometry(boundingBox).getCentroid();
		geometryFactory.getPrecisionModel().makePrecise(p.getCoordinate());
		return p;
	}

}
