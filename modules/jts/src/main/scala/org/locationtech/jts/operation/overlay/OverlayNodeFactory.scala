// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
package org.locationtech.jts.operation.overlay

/**
 * @version 1.7
 */

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geomgraph.DirectedEdgeStar
import org.locationtech.jts.geomgraph.Node
import org.locationtech.jts.geomgraph.NodeFactory

/**
 * Creates nodes for use in the {link PlanarGraph}s constructed during overlay operations.
 *
 * @version 1.7
 */
class OverlayNodeFactory extends NodeFactory {
  override def createNode(coord: Coordinate): Node = new Node(coord, new DirectedEdgeStar)
}
