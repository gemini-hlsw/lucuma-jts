// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.util

/**
 * A utility for making programming assertions.
 *
 * @version 1.7
 */
object Assert {

  /**
   * Throws an <code>AssertionFailedException</code> if the given assertion is not true.
   *
   * @param assertion
   *   a condition that is supposed to be true throws AssertionFailedException if the condition is
   *   false
   */
  def isTrue(assertion: Boolean): Unit = isTrue(assertion, null)

  /**
   * Throws an <code>AssertionFailedException</code> with the given message if the given assertion
   * is not true.
   *
   * @param assertion
   *   a condition that is supposed to be true
   * @param message
   *   a description of the assertion throws AssertionFailedException if the condition is false
   */
  def isTrue(assertion: Boolean, message: String): Unit = if (!assertion)
    if (message == null) throw new AssertionFailedException
    else throw new AssertionFailedException(message)

  /**
   * Throws an <code>AssertionFailedException</code> if the given objects are not equal, according
   * to the <code>equals</code> method.
   *
   * @param expectedValue
   *   the correct value
   * @param actualValue
   *   the value being checked throws AssertionFailedException if the two objects are not equal
   */
  def equals(expectedValue: Any, actualValue: Any): Unit = equals(expectedValue, actualValue, null)

  /**
   * Throws an <code>AssertionFailedException</code> with the given message if the given objects are
   * not equal, according to the <code>equals</code> method.
   *
   * @param expectedValue
   *   the correct value
   * @param actualValue
   *   the value being checked
   * @param message
   *   a description of the assertion throws AssertionFailedException if the two objects are not
   *   equal
   */
  def equals(expectedValue: Any, actualValue: Any, message: String): Unit = if (
    !(actualValue == expectedValue)
  )
    throw new AssertionFailedException(
      "Expected " + expectedValue + " but encountered " + actualValue + (if (message != null)
                                                                           ": " + message
                                                                         else "")
    )

  /**
   * Always throws an <code>AssertionFailedException</code>.
   *
   * throws AssertionFailedException thrown always
   */
  def shouldNeverReachHere(): Unit = shouldNeverReachHere(null)

  /**
   * Always throws an <code>AssertionFailedException</code> with the given message.
   *
   * @param message
   *   a description of the assertion throws AssertionFailedException thrown always
   */
  def shouldNeverReachHere(message: String): Unit = throw new AssertionFailedException(
    "Should never reach here" + (if (message != null) ": " + message
                                 else "")
  )
}

class Assert {}
