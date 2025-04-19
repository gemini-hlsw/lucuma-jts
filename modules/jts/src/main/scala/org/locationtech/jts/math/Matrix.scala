// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.math

/**
 * Implements some 2D matrix operations (in particular, solving systems of linear equations).
 *
 * @author
 *   Martin Davis
 */
object Matrix {
  private def swapRows(m: Array[Array[Double]], i: Int, j: Int): Unit = {
    if (i == j) return
    var col = 0
    while (col < m(0).length) {
      val temp = m(i)(col)
      m(i)(col) = m(j)(col)
      m(j)(col) = temp
      col += 1
    }
  }

  private def swapRows(m: Array[Double], i: Int, j: Int): Unit = {
    if (i == j) return
    val temp = m(i)
    m(i) = m(j)
    m(j) = temp
  }

  /**
   * Solves a system of equations using Gaussian Elimination. In order to avoid overhead the
   * algorithm runs in-place on A - if A should not be modified the client must supply a copy.
   *
   * @param a
   *   an nxn matrix in row/column order )modified by this method)
   * @param b
   *   a vector of length n return a vector containing the solution (if any) or null if the system
   *   has no or no unique solution throws IllegalArgumentException if the matrix is the wrong size
   */
  def solve(a: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val n = b.length
    if (a.length != n || a(0).length != n)
      throw new IllegalArgumentException("Matrix A is incorrectly sized")

    // Use Gaussian Elimination with partial pivoting.
    // Iterate over each row
    for (i <- 0 until n) {
      // Find the largest pivot in the rows below the current one.
      var maxElementRow = i
      for (j <- i + 1 until n)
        if (math.abs(a(j)(i)) > math.abs(a(maxElementRow)(i))) {
          maxElementRow = j
        }

      if (a(maxElementRow)(i) == 0.0) {
        return null
      }

      // Exchange current row and maxElementRow in A and b.
      swapRows(a, i, maxElementRow)
      swapRows(b, i, maxElementRow)

      // Eliminate using row i
      for (j <- i + 1 until n) {
        val rowFactor = a(j)(i) / a(i)(i)
        for (k <- n - 1 to i by -1)
          a(j)(k) -= a(i)(k) * rowFactor
        b(j) -= b(i) * rowFactor
      }
    }

    /**
     * A is now (virtually) in upper-triangular form. The solution vector is determined by
     * back-substitution.
     */
    val solution = Array.fill(n)(0.0)
    for (j <- n - 1 to 0 by -1) {
      var t = 0.0
      for (k <- j + 1 until n)
        t += a(j)(k) * solution(k)
      solution(j) = (b(j) - t) / a(j)(j)
    }
    solution
  }

}
