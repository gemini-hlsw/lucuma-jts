// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.precision

/**
 * Determines the maximum number of common most-significant bits in the mantissa of one or numbers.
 * Can be used to compute the double-precision number which is represented by the common bits. If
 * there are no common bits, the number computed is 0.0.
 *
 * @version 1.7
 */
object CommonBits {

  /**
   * Computes the bit pattern for the sign and exponent of a double-precision number.
   *
   * @param num
   *   return the bit pattern for the sign and exponent
   */
  def signExpBits(num: Long): Long = num >> 52

  /**
   * This computes the number of common most-significant bits in the mantissas of two
   * double-precision numbers. It does not count the hidden bit, which is always 1. It does not
   * determine whether the numbers have the same exponent - if they do not, the value computed by
   * this function is meaningless.
   *
   * @param num1
   *   the first number
   * @param num2
   *   the second number return the number of common most-significant mantissa bits
   */
  def numCommonMostSigMantissaBits(num1: Long, num2: Long): Int = {
    var count = 0
    var i     = 52
    while (i >= 0) {
      if (getBit(num1, i) != getBit(num2, i)) return count
      count += 1
      i -= 1
    }
    52
  }

  /**
   * Zeroes the lower n bits of a bitstring.
   *
   * @param bits
   *   the bitstring to alter return the zeroed bitstring
   */
  def zeroLowerBits(bits: Long, nBits: Int): Long = {
    val invMask = (1L << nBits) - 1L
    val mask    = ~invMask
    val zeroed  = bits & mask
    zeroed
  }

  /**
   * Extracts the i'th bit of a bitstring.
   *
   * @param bits
   *   the bitstring to extract from
   * @param i
   *   the bit to extract return the value of the extracted bit
   */
  def getBit(bits: Long, i: Int): Int = {
    val mask = 1L << i
    if ((bits & mask) != 0) 1
    else 0
  }
}

class CommonBits() {
  private var isFirst                 = true
  private var commonMantissaBitsCount = 53
  private var commonBits              = 0L
  private var commonSignExp           = 0L

  def add(num: Double): Unit = {
    val numBits    = java.lang.Double.doubleToLongBits(num)
    if (isFirst) {
      commonBits = numBits
      commonSignExp = CommonBits.signExpBits(commonBits)
      isFirst = false
      return
    }
    val numSignExp = CommonBits.signExpBits(numBits)
    if (numSignExp != commonSignExp) {
      commonBits = 0
      return
    }
    //    System.out.println(toString(commonBits));
    //    System.out.println(toString(numBits));
    commonMantissaBitsCount = CommonBits.numCommonMostSigMantissaBits(commonBits, numBits)
    commonBits = CommonBits.zeroLowerBits(commonBits, 64 - (12 + commonMantissaBitsCount))
  }

  def getCommon = java.lang.Double.longBitsToDouble(commonBits)

  /**
   * A representation of the Double bits formatted for easy readability
   */
  def toString(bits: Long): String = {
    val x      = java.lang.Double.longBitsToDouble(bits)
    val numStr = java.lang.Long.toBinaryString(bits)
    val padStr = "0000000000000000000000000000000000000000000000000000000000000000" + numStr
    val bitStr = padStr.substring(padStr.length - 64)
    val str    = bitStr.substring(0, 1) + "  " + bitStr.substring(1, 12) + "(exp) " + bitStr.substring(
      12
    ) + " [ " + x + " ]"
    str
  }
}
