// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.precision

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Polygonal
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.util.GeometryEditor
import org.locationtech.jts.operation.overlayng.PrecisionReducer

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * Reduces the precision of a {@link Geometry} according to the supplied {@link PrecisionModel},
 * ensuring that the result is valid (unless specified otherwise). <p> By default the reduced result
 * is topologically valid (i.e. {@link Geometry# isValid ( )} is true). To ensure this a polygonal
 * geometry is reduced in a topologically valid fashion (technically, by using snap-rounding). It
 * can be forced to be reduced pointwise by using {@link # setPointwise ( boolean )}. Note that in
 * this case the result geometry may be invalid. Linear and point geometry is always reduced
 * pointwise (i.e. without further change to its topology or stucture), since this does not change
 * validity. <p> By default the geometry precision model is not changed. This can be overridden by
 * using {@link # setChangePrecisionModel ( boolean )}. <p> Normally collapsed components (e.g.
 * lines collapsing to a point) are not included in the result. This behavior can be changed by
 * using {@link # setRemoveCollapsedComponents ( boolean )}.
 *
 * @version 1.12
 */
object GeometryPrecisionReducer {

  /**
   * Convenience method for doing precision reduction on a single geometry, with collapses removed
   * and keeping the geometry precision model the same, and preserving polygonal topology.
   *
   * @param g
   *   the geometry to reduce
   * @param precModel
   *   the precision model to use
   * @return
   *   the reduced geometry
   */
  def reduce(g: Geometry, precModel: PrecisionModel): Geometry = {
    val reducer = new GeometryPrecisionReducer(precModel)
    reducer.reduce(g)
  }

  /**
   * Convenience method for doing pointwise precision reduction on a single geometry, with collapses
   * removed and keeping the geometry precision model the same, but NOT preserving valid polygonal
   * topology.
   *
   * @param g
   *   the geometry to reduce
   * @param precModel
   *   the precision model to use
   * @return
   *   the reduced geometry
   */
  def reducePointwise(g: Geometry, precModel: PrecisionModel): Geometry = {
    val reducer = new GeometryPrecisionReducer(precModel)
    reducer.setPointwise(true)
    reducer.reduce(g)
  }
}

class GeometryPrecisionReducer(var targetPM: PrecisionModel) {
  private var removeCollapsed      = true
  private var changePrecisionModel = false
  private var isPointwise          = false

  /**
   * Sets whether the reduction will result in collapsed components being removed completely, or
   * simply being collapsed to an (invalid) Geometry of the same type. The default is to remove
   * collapsed components.
   *
   * @param removeCollapsed
   *   if <code>true</code> collapsed components will be removed
   */
  def setRemoveCollapsedComponents(removeCollapsed: Boolean): Unit =
    this.removeCollapsed = removeCollapsed

  /**
   * Sets whether the {@link PrecisionModel} of the new reduced Geometry will be changed to be the
   * {@link PrecisionModel} supplied to specify the precision reduction. <p> The default is to
   * <b>not</b> change the precision model
   *
   * @param changePrecisionModel
   *   if <code>true</code> the precision model of the created Geometry will be the the
   *   precisionModel supplied in the constructor.
   */
  def setChangePrecisionModel(changePrecisionModel: Boolean): Unit =
    this.changePrecisionModel = changePrecisionModel

  /**
   * Sets whether the precision reduction will be done in pointwise fashion only. Pointwise
   * precision reduction reduces the precision of the individual coordinates only, but does not
   * attempt to recreate valid topology. This is only relevant for geometries containing polygonal
   * components.
   *
   * @param isPointwise
   *   if reduction should be done pointwise only
   */
  def setPointwise(isPointwise: Boolean): Unit =
    this.isPointwise = isPointwise

  def reduce(geom: Geometry): Geometry = {
    val reduced = PrecisionReducerTransformer.reduce(geom, targetPM, isPointwise)
    if (changePrecisionModel) changePM(reduced, targetPM) else reduced
  }

  private def reducePointwise(geom: Geometry) = {
    var geomEdit: GeometryEditor = null
    if (changePrecisionModel) {
      val newFactory = createFactory(geom.getFactory, targetPM)
      geomEdit = new GeometryEditor(newFactory)
    } else { // don't change geometry factory
      geomEdit = new GeometryEditor()
    }

    /**
     * For polygonal geometries, collapses are always removed, in order to produce correct topology
     */
    var finalRemoveCollapsed = removeCollapsed
    if (geom.getDimension >= 2) finalRemoveCollapsed = true
    val reduceGeom           =
      geomEdit.edit(geom, new PrecisionReducerCoordinateOperation(targetPM, finalRemoveCollapsed))
    reduceGeom
  }

  /**
   * Duplicates a geometry to one that uses a different PrecisionModel, without changing any
   * coordinate values.
   *
   * @param geom
   *   the geometry to duplicate
   * @param newPM
   *   the precision model to use
   * @return
   *   the geometry value with a new precision model
   */
  private def changePM(geom: Geometry, newPM: PrecisionModel) = {
    val geomEditor = createEditor(geom.getFactory, newPM)
    // this operation changes the PM for the entire geometry tree
    geomEditor.edit(geom, new GeometryEditor.NoOpGeometryOperation)
  }

  private def createEditor(geomFactory: GeometryFactory, newPM: PrecisionModel): GeometryEditor = { // no need to change if precision model is the same
    if (geomFactory.getPrecisionModel eq newPM) return new GeometryEditor
    // otherwise create a geometry editor which changes PrecisionModel
    val newFactory = createFactory(geomFactory, newPM)
    val geomEdit   = new GeometryEditor(newFactory)
    geomEdit
  }

  private def createFactory(inputFactory: GeometryFactory, pm: PrecisionModel) = {
    val newFactory =
      new GeometryFactory(pm, inputFactory.getSRID, inputFactory.getCoordinateSequenceFactory)
    newFactory
  }
}
