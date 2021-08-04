/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.util

import java.util

/**
 * An extendable array of primitive <code>int</code> values.
 *
 * @author Martin Davis
 */
class IntArrayList(val initialCapacity: Int) {

  /**
   * Constructs an empty list with the specified initial capacity
   *
   * @param initialCapacity the initial capacity of the list
   */
  private var data  = new Array[Int](initialCapacity)
  private var vsize = 0

  /**
   * Constructs an empty list.
   */
  def this() = {
    this(10)
  }

  /**
   * Returns the number of values in this list.
   *
   * return the number of values in the list
   */
  def size: Int = vsize

  /**
   * Increases the capacity of this list instance, if necessary,
   * to ensure that it can hold at least the number of elements
   * specified by the capacity argument.
   *
   * @param capacity the desired capacity
   */
  def ensureCapacity(capacity: Int): Unit = {
    if (capacity <= data.length) return
    val newLength = Math.max(capacity, data.length * 2)
    //System.out.println("IntArrayList: copying " + size + " ints to new array of length " + capacity);
    data = util.Arrays.copyOf(data, newLength)
  }

  /**
   * Adds a value to the end of this list.
   *
   * @param value the value to add
   */
  def add(value: Int): Unit = {
    ensureCapacity(vsize + 1)
    data(vsize) = value
    vsize += 1
  }

  /**
   * Adds all values in an array to the end of this list.
   *
   * @param values an array of values
   */
  def addAll(values: Array[Int]): Unit = {
    if (values == null) return
    if (values.length == 0) return
    ensureCapacity(vsize + values.length)
    System.arraycopy(values, 0, data, vsize, values.length)
    vsize += values.length
  }

  /**
   * Returns a int array containing a copy of
   * the values in this list.
   *
   * return an array containing the values in this list
   */
  def toArray: Array[Int] = {
    val array = new Array[Int](vsize)
    System.arraycopy(data, 0, array, 0, vsize)
    array
  }
}
