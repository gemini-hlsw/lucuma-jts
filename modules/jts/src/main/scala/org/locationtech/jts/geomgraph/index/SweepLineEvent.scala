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
package org.locationtech.jts.geomgraph.index

/**
 * @version 1.7
 */
object SweepLineEvent {
  private val INSERT = 1
  private val DELETE = 2
}

class SweepLineEvent(val label: Any, val x: Double, val obj: Any)
    extends Comparable[SweepLineEvent] {
//  private var label: AnyRef = null // used for red-blue intersection detection
  private val xValue                      = x
  private var eventType                   = SweepLineEvent.INSERT
  private var insertEvent: SweepLineEvent = null // null if this is an INSERT event
  private var deleteEventIndex            = 0
//  private var obj = null

  /**
   * Creates an INSERT event.
   *
   * @param label
   *   the edge set label for this object
   * @param x
   *   the event location
   * @param obj
   *   the object being inserted
   */
//  def this {
//    this()
//    this.eventType = SweepLineEvent.INSERT
//    this.label = label
//    xValue = x
//    this.obj = obj
//  }

  /**
   * Creates a DELETE event.
   *
   * @param x
   *   the event location
   * @param insertEvent
   *   the corresponding INSERT event
   */
  def this(x: Double, insertEventArg: SweepLineEvent) = {
    this(null, x, null)
    eventType = SweepLineEvent.DELETE
//    xValue = x
    this.insertEvent = insertEventArg
  }

  def isInsert: Boolean = eventType == SweepLineEvent.INSERT

  def isDelete: Boolean = eventType == SweepLineEvent.DELETE

  def getInsertEvent: SweepLineEvent = insertEvent

  def getDeleteEventIndex: Int = deleteEventIndex

  def setDeleteEventIndex(deleteEventIndex: Int): Unit = this.deleteEventIndex = deleteEventIndex

  def getObject: Any = obj

  def isSameLabel(ev: SweepLineEvent): Boolean = { // no label set indicates single group
    if (label == null) return false
    label == ev.label
  }

  /**
   * Events are ordered first by their x-value, and then by their eventType. Insert events are
   * sorted before Delete events, so that items whose Insert and Delete events occur at the same
   * x-value will be correctly handled.
   */
  override def compareTo(pe: SweepLineEvent): Int = {
    if (xValue < pe.xValue) return -1
    if (xValue > pe.xValue) return 1
    if (eventType < pe.eventType) return -1
    if (eventType > pe.eventType) return 1
    0
  }
}
