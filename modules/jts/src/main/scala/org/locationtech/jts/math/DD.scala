// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */ /*
 * Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */
package org.locationtech.jts.math

import java.io.Serializable

/**
 * Implements extended-precision floating-point numbers
 * which maintain 106 bits (approximately 30 decimal digits) of precision.
 * <p>
 * A DoubleDouble uses a representation containing two double-precision values.
 * A number x is represented as a pair of doubles, x.hi and x.lo,
 * such that the number represented by x is x.hi + x.lo, where
 * <pre>
 * |x.lo| &lt;= 0.5*ulp(x.hi)
 * </pre>
 * and ulp(y) means "unit in the last place of y".
 * The basic arithmetic operations are implemented using
 * convenient properties of IEEE-754 floating-point arithmetic.
 * <p>
 * The range of values which can be represented is the same as in IEEE-754.
 * The precision of the representable numbers
 * is twice as great as IEEE-754 double precision.
 * <p>
 * The correctness of the arithmetic algorithms relies on operations
 * being performed with standard IEEE-754 double precision and rounding.
 * This is the Java standard arithmetic model, but for performance reasons
 * Java implementations are not
 * constrained to using this standard by default.
 * Some processors (notably the Intel Pentium architecture) perform
 * floating point operations in (non-IEEE-754-standard) extended-precision.
 * A JVM implementation may choose to use the non-standard extended-precision
 * as its default arithmetic mode.
 * To prevent this from happening, this code uses the
 * Java <tt>strictfp</tt> modifier,
 * which forces all operations to take place in the standard IEEE-754 rounding model.
 * <p>
 * The API provides both a set of value-oriented operations
 * and a set of mutating operations.
 * Value-oriented operations treat DoubleDouble values as
 * immutable; operations on them return new objects carrying the result
 * of the operation.  This provides a simple and safe semantics for
 * writing DoubleDouble expressions.  However, there is a performance
 * penalty for the object allocations required.
 * The mutable interface updates object values in-place.
 * It provides optimum memory performance, but requires
 * care to ensure that aliasing errors are not created
 * and constant values are not changed.
 * <p>
 * For example, the following code example constructs three DD instances:
 * two to hold the input values and one to hold the result of the addition.
 * <pre>
 * DD a = new DD(2.0);
 * DD b = new DD(3.0);
 * DD c = a.add(b);
 * </pre>
 * In contrast, the following approach uses only one object:
 * <pre>
 * DD a = new DD(2.0);
 *     a.selfAdd(3.0);
 * </pre>
 * <p>
 * This implementation uses algorithms originally designed variously by
 * Knuth, Kahan, Dekker, and Linnainmaa.
 * Douglas Priest developed the first C implementation of these techniques.
 * Other more recent C++ implementation are due to Keith M. Briggs and David Bailey et al.
 *
 * <h3>References</h3>
 * <ul>
 * <li>Priest, D., <i>Algorithms for Arbitrary Precision Floating Point Arithmetic</i>,
 * in P. Kornerup and D. Matula, Eds., Proc. 10th Symposium on Computer Arithmetic,
 * IEEE Computer Society Press, Los Alamitos, Calif., 1991.
 * <li>Yozo Hida, Xiaoye S. Li and David H. Bailey,
 * <i>Quad-Double Arithmetic: Algorithms, Implementation, and Application</i>,
 * manuscript, Oct 2000; Lawrence Berkeley National Laboratory Report BNL-46996.
 * <li>David Bailey, <i>High Precision Software Directory</i>;
 * <tt>http://crd.lbl.gov/~dhbailey/mpdist/index.html</tt>
 * </ul>
 *
 * @author Martin Davis
 */

/**
 * Creates a new DoubleDouble with value 0.0.
 */
final class DD(private var hi: Double = 0.0, private var lo: Double = 0.0)
    extends Serializable
    with Comparable[DD]
    with Cloneable {

  init(hi, lo)

  /**
   * The high-order component of the double-double precision value.
   */
//    private var hi = 0.0
  /**
   * The low-order component of the double-double precision value.
   */
//    private var lo = 0.0

  /**
   * Creates a new DoubleDouble with value x.
   *
   * @param x the value to initialize
   */
  def this(x: Double) = {
    this()
    init(x)
  }

  /**
   * Creates a new DoubleDouble with value (hi, lo).
   *
   * @param hi the high-order component
   * @param lo the high-order component
   */
//    def this(hi: Double, lo: Double) = {
//      this(hi, lo)
//      init(hi, lo)
//    }

  /**
   * Creates a new DoubleDouble with value equal to the argument.
   *
   * @param dd the value to initialize
   */
  def this(dd: DD) = {
    this()
    init(dd)
  }

  /**
   * Creates a new DoubleDouble with value equal to the argument.
   *
   * @param str the value to initialize by
   * throws NumberFormatException if <tt>str</tt> is not a valid representation of a number
   */
//    def this(str: String) = {
//      this(DD.parse(str))
//    }

  /**
   * Creates and returns a copy of this value.
   *
   * return a copy of this value
   */
  override def clone: AnyRef = try super.clone
  catch {
    case _: CloneNotSupportedException =>
      // should never reach here
      null
  }

  final private def init(x: Double): Unit = {
    this.hi = x
    this.lo = 0.0
  }

  final private def init(hi: Double, lo: Double): Unit = {
    this.hi = hi
    this.lo = lo
  }

  final private def init(dd: DD): Unit = {
    hi = dd.hi
    lo = dd.lo
  }

  /**
   * Set the value for the DD object. This method supports the mutating
   * operations concept described in the class documentation (see above).
   *
   * @param value a DD instance supplying an extended-precision value.
   * return a self-reference to the DD instance.
   */
  def setValue(value: DD): DD = {
    init(value)
    this
  }

  /**
   * Set the value for the DD object. This method supports the mutating
   * operations concept described in the class documentation (see above).
   *
   * @param value a floating point value to be stored in the instance.
   * return a self-reference to the DD instance.
   */
  def setValue(value: Double): DD = {
    init(value)
    this
  }

  /**
   * Returns a new DoubleDouble whose value is <tt>(this + y)</tt>.
   *
   * @param y the addend
   * return <tt>(this + y)</tt>
   */
  final def add(y: DD): DD = DD.copy(this).selfAdd(y)

  final def add(y: Double): DD = DD.copy(this).selfAdd(y)

  /**
   * Adds the argument to the value of <tt>this</tt>.
   * To prevent altering constants,
   * this method <b>must only</b> be used on values known to
   * be newly created.
   *
   * @param y the addend
   * return this object, increased by y
   */
  final def selfAdd(y: DD): DD = selfAdd(y.hi, y.lo)

  final def selfAdd(y: Double): DD = {
    var H = .0
    var h = .0
    var S = .0
    var s = .0
    var e = .0
    var f = .0
    S = hi + y
    e = S - hi
    s = S - e
    s = (y - e) + (hi - s)
    f = s + lo
    H = S + f
    h = f + (S - H)
    hi = H + h
    lo = h + (H - hi)
    this
    // return selfAdd(y, 0.0);
  }

  final private def selfAdd(yhi: Double, ylo: Double) = {
    var H   = .0
    var h   = .0
    var T   = .0
    var t   = .0
    var S   = .0
    var s   = .0
    var e   = .0
    var f   = .0
    S = hi + yhi
    T = lo + ylo
    e = S - hi
    f = T - lo
    s = S - e
    t = T - f
    s = (yhi - e) + (hi - s)
    t = (ylo - f) + (lo - t)
    e = s + T
    H = S + e
    h = e + (S - H)
    e = t + h
    val zhi = H + e
    val zlo = e + (H - zhi)
    hi = zhi
    lo = zlo
    this
  }

  /**
   * Computes a new DoubleDouble object whose value is <tt>(this - y)</tt>.
   *
   * @param y the subtrahend
   * return <tt>(this - y)</tt>
   */
  final def subtract(y: DD): DD = add(y.negate)

  final def subtract(y: Double): DD = add(-y)

  /**
   * Subtracts the argument from the value of <tt>this</tt>.
   * To prevent altering constants,
   * this method <b>must only</b> be used on values known to
   * be newly created.
   *
   * @param y the addend
   * return this object, decreased by y
   */
  final def selfSubtract(y: DD): DD = {
    if (isNaN) return this
    selfAdd(-y.hi, -y.lo)
  }

  final def selfSubtract(y: Double): DD = {
    if (isNaN) return this
    selfAdd(-y, 0.0)
  }

  /**
   * Returns a new DoubleDouble whose value is <tt>-this</tt>.
   *
   * return <tt>-this</tt>
   */
  final def negate: DD = {
    if (isNaN) return this
    new DD(-hi, -lo)
  }

  /**
   * Returns a new DoubleDouble whose value is <tt>(this * y)</tt>.
   *
   * @param y the multiplicand
   * return <tt>(this * y)</tt>
   */
  final def multiply(y: DD): DD = {
    if (y.isNaN) return DD.createNaN
    DD.copy(this).selfMultiply(y)
  }

  final def multiply(y: Double): DD = {
    if (java.lang.Double.isNaN(y)) return DD.createNaN
    DD.copy(this).selfMultiply(y, 0.0)
  }

  /**
   * Multiplies this object by the argument, returning <tt>this</tt>.
   * To prevent altering constants,
   * this method <b>must only</b> be used on values known to
   * be newly created.
   *
   * @param y the value to multiply by
   * return this object, multiplied by y
   */
  final def selfMultiply(y: DD): DD = selfMultiply(y.hi, y.lo)

  final def selfMultiply(y: Double): DD = selfMultiply(y, 0.0)

  final private def selfMultiply(yhi: Double, ylo: Double) = {
    var hx  = .0
    var tx  = .0
    var hy  = .0
    var ty  = .0
    var C   = .0
    var c   = .0
    C = DD.SPLIT * hi
    hx = C - hi
    c = DD.SPLIT * yhi
    hx = C - hx
    tx = hi - hx
    hy = c - yhi
    C = hi * yhi
    hy = c - hy
    ty = yhi - hy
    c = ((((hx * hy - C) + hx * ty) + tx * hy) + tx * ty) + (hi * ylo + lo * yhi)
    val zhi = C + c
    hx = C - zhi
    val zlo = c + hx
    hi = zhi
    lo = zlo
    this
  }

  /**
   * Computes a new DoubleDouble whose value is <tt>(this / y)</tt>.
   *
   * @param y the divisor
   * return a new object with the value <tt>(this / y)</tt>
   */
  final def divide(y: DD): DD = {
    var hc  = .0
    var tc  = .0
    var hy  = .0
    var ty  = .0
    var C   = .0
    var c   = .0
    var U   = .0
    var u   = .0
    C = hi / y.hi
    c = DD.SPLIT * C
    hc = c - C
    u = DD.SPLIT * y.hi
    hc = c - hc
    tc = C - hc
    hy = u - y.hi
    U = C * y.hi
    hy = u - hy
    ty = y.hi - hy
    u = (((hc * hy - U) + hc * ty) + tc * hy) + tc * ty
    c = ((((hi - U) - u) + lo) - C * y.lo) / y.hi
    u = C + c
    val zhi = u
    val zlo = (C - u) + c
    new DD(zhi, zlo)
  }

  final def divide(y: Double): DD = {
    if (java.lang.Double.isNaN(y)) return DD.createNaN
    DD.copy(this).selfDivide(y, 0.0)
  }

  /**
   * Divides this object by the argument, returning <tt>this</tt>.
   * To prevent altering constants,
   * this method <b>must only</b> be used on values known to
   * be newly created.
   *
   * @param y the value to divide by
   * return this object, divided by y
   */
  final def selfDivide(y: DD): DD = selfDivide(y.hi, y.lo)

  final def selfDivide(y: Double): DD = selfDivide(y, 0.0)

  final private def selfDivide(yhi: Double, ylo: Double) = {
    var hc = .0
    var tc = .0
    var hy = .0
    var ty = .0
    var C  = .0
    var c  = .0
    var U  = .0
    var u  = .0
    C = hi / yhi
    c = DD.SPLIT * C
    hc = c - C
    u = DD.SPLIT * yhi
    hc = c - hc
    tc = C - hc
    hy = u - yhi
    U = C * yhi
    hy = u - hy
    ty = yhi - hy
    u = (((hc * hy - U) + hc * ty) + tc * hy) + tc * ty
    c = ((((hi - U) - u) + lo) - C * ylo) / yhi
    u = C + c
    hi = u
    lo = (C - u) + c
    this
  }

  /**
   * Returns a DoubleDouble whose value is  <tt>1 / this</tt>.
   *
   * return the reciprocal of this value
   */
  final def reciprocal: DD = {
    var hc  = .0
    var tc  = .0
    var hy  = .0
    var ty  = .0
    var C   = .0
    var c   = .0
    var U   = .0
    var u   = .0
    C = 1.0 / hi
    c = DD.SPLIT * C
    hc = c - C
    u = DD.SPLIT * hi
    hc = c - hc
    tc = C - hc
    hy = u - hi
    U = C * hi
    hy = u - hy
    ty = hi - hy
    u = (((hc * hy - U) + hc * ty) + tc * hy) + tc * ty
    c = ((1.0 - U) - u - C * lo) / hi
    val zhi = C + c
    val zlo = (C - zhi) + c
    new DD(zhi, zlo)
  }

  /**
   * Returns the largest (closest to positive infinity)
   * value that is not greater than the argument
   * and is equal to a mathematical integer.
   * Special cases:
   * <ul>
   * <li>If this value is NaN, returns NaN.
   * </ul>
   *
   * return the largest (closest to positive infinity)
   *         value that is not greater than the argument
   *         and is equal to a mathematical integer.
   */
  def floor: DD = {
    if (isNaN) return DD.NaN
    val fhi = Math.floor(hi)
    var flo = 0.0
    // Hi is already integral.  Floor the low word
    if (fhi == hi) flo = Math.floor(lo)
    // do we need to renormalize here?
    new DD(fhi, flo)
  }

  /**
   * Returns the smallest (closest to negative infinity) value
   * that is not less than the argument and is equal to a mathematical integer.
   * Special cases:
   * <ul>
   * <li>If this value is NaN, returns NaN.
   * </ul>
   *
   * return the smallest (closest to negative infinity) value
   *         that is not less than the argument and is equal to a mathematical integer.
   */
  def ceil: DD = {
    if (isNaN) return DD.NaN
    val fhi = Math.ceil(hi)
    var flo = 0.0
    // Hi is already integral.  Ceil the low word
    if (fhi == hi) {
      flo = Math.ceil(lo)
      // do we need to renormalize here?
    }
    new DD(fhi, flo)
  }

  /**
   * Returns an integer indicating the sign of this value.
   * <ul>
   * <li>if this value is &gt; 0, returns 1
   * <li>if this value is &lt; 0, returns -1
   * <li>if this value is = 0, returns 0
   * <li>if this value is NaN, returns 0
   * </ul>
   *
   * return an integer indicating the sign of this value
   */
  def signum: Int = {
    if (hi > 0) return 1
    if (hi < 0) return -1
    if (lo > 0) return 1
    if (lo < 0) return -1
    0
  }

  /**
   * Rounds this value to the nearest integer.
   * The value is rounded to an integer by adding 1/2 and taking the floor of the result.
   * Special cases:
   * <ul>
   * <li>If this value is NaN, returns NaN.
   * </ul>
   *
   * return this value rounded to the nearest integer
   */
  def rint: DD = {
    if (isNaN) return this
    // may not be 100% correct
    val plus5 = this.add(0.5)
    plus5.floor
  }

  /**
   * Returns the integer which is largest in absolute value and not further
   * from zero than this value.
   * Special cases:
   * <ul>
   * <li>If this value is NaN, returns NaN.
   * </ul>
   *
   * return the integer which is largest in absolute value and not further from zero than this value
   */
  def trunc: DD = {
    if (isNaN) return DD.NaN
    if (isPositive) floor
    else ceil
  }

  /**
   * Returns the absolute value of this value.
   * Special cases:
   * <ul>
   * <li>If this value is NaN, it is returned.
   * </ul>
   *
   * return the absolute value of this value
   */
  def abs: DD = {
    if (isNaN) return DD.NaN
    if (isNegative) return negate
    new DD(this)
  }

  def sqr: DD = this.multiply(this)

  /**
   * Squares this object.
   * To prevent altering constants,
   * this method <b>must only</b> be used on values known to
   * be newly created.
   *
   * return the square of this value.
   */
  def selfSqr: DD = this.selfMultiply(this)

  /**
   * Computes the positive square root of this value.
   * If the number is NaN or negative, NaN is returned.
   *
   * return the positive square root of this number.
   *         If the argument is NaN or less than zero, the result is NaN.
   */
  def sqrt: DD = {
    /* Strategy:  Use Karp's trick:  if x is an approximation
         to sqrt(a), then

            sqrt(a) = a*x + [a - (a*x)^2] * x / 2   (approx)

         The approximation is accurate to twice the accuracy of x.
         Also, the multiplication (a*x) and [-]*x can be done with
         only half the precision.
     */
    if (isZero) return DD.valueOf(0.0)
    if (isNegative) return DD.NaN
    val x      = 1.0 / Math.sqrt(hi)
    val ax     = hi * x
    val axdd   = DD.valueOf(ax)
    val diffSq = this.subtract(axdd.sqr)
    val d2     = diffSq.hi * (x * 0.5)
    axdd.add(d2)
  }

  /**
   * Computes the value of this number raised to an integral power.
   * Follows semantics of Java Math.pow as closely as possible.
   *
   * @param exp the integer exponent
   * return x raised to the integral power exp
   */
  def pow(exp: Int): DD = {
    if (exp == 0.0) return DD.valueOf(1.0)
    var r = new DD(this)
    var s = DD.valueOf(1.0)
    var n = Math.abs(exp)
    if (n > 1) /* Use binary exponentiation */ while (n > 0) {
      if (n % 2 == 1) s.selfMultiply(r)
      n /= 2
      if (n > 0) r = r.sqr
    }
    else s = r
    /* Compute the reciprocal if n is negative. */
    if (exp < 0) return s.reciprocal
    s
  }

  /**
   * Computes the minimum of this and another DD number.
   *
   * @param x a DD number
   * return the minimum of the two numbers
   */
  def min(x: DD): DD = if (this.le(x)) this
  else x

  /**
   * Computes the maximum of this and another DD number.
   *
   * @param x a DD number
   * return the maximum of the two numbers
   */
  def max(x: DD): DD = if (this.ge(x)) this
  else x

  /**
   * Converts this value to the nearest double-precision number.
   *
   * return the nearest double-precision number to this value
   */
  def doubleValue: Double = hi + lo

  /**
   * Converts this value to the nearest integer.
   *
   * return the nearest integer to this value
   */
  def intValue: Int = hi.toInt

  /**
   * Tests whether this value is equal to 0.
   *
   * return true if this value is equal to 0
   */
  def isZero: Boolean = hi == 0.0 && lo == 0.0

  /**
   * Tests whether this value is less than 0.
   *
   * return true if this value is less than 0
   */
  def isNegative: Boolean = hi < 0.0 || (hi == 0.0 && lo < 0.0)

  /**
   * Tests whether this value is greater than 0.
   *
   * return true if this value is greater than 0
   */
  def isPositive: Boolean = hi > 0.0 || (hi == 0.0 && lo > 0.0)

  /**
   * Tests whether this value is NaN.
   *
   * return true if this value is NaN
   */
  def isNaN = java.lang.Double.isNaN(hi)

  /**
   * Tests whether this value is equal to another <tt>DoubleDouble</tt> value.
   *
   * @param y a DoubleDouble value
   * return true if this value = y
   */
  def equals(y: DD): Boolean = hi == y.hi && lo == y.lo

  /**
   * Tests whether this value is greater than another <tt>DoubleDouble</tt> value.
   *
   * @param y a DoubleDouble value
   * return true if this value &gt; y
   */
  def gt(y: DD): Boolean = (hi > y.hi) || (hi == y.hi && lo > y.lo)

  /**
   * Tests whether this value is greater than or equals to another <tt>DoubleDouble</tt> value.
   *
   * @param y a DoubleDouble value
   * return true if this value &gt;= y
   */
  def ge(y: DD): Boolean = (hi > y.hi) || (hi == y.hi && lo >= y.lo)

  /**
   * Tests whether this value is less than another <tt>DoubleDouble</tt> value.
   *
   * @param y a DoubleDouble value
   * return true if this value &lt; y
   */
  def lt(y: DD): Boolean = (hi < y.hi) || (hi == y.hi && lo < y.lo)

  /**
   * Tests whether this value is less than or equal to another <tt>DoubleDouble</tt> value.
   *
   * @param y a DoubleDouble value
   * return true if this value &lt;= y
   */
  def le(y: DD): Boolean = (hi < y.hi) || (hi == y.hi && lo <= y.lo)

  /**
   * Compares two DoubleDouble objects numerically.
   *
   * return -1,0 or 1 depending on whether this value is less than, equal to
   *         or greater than the value of <tt>o</tt>
   */
  override def compareTo(other: DD): Int = {
    if (hi < other.hi) return -1
    if (hi > other.hi) return 1
    if (lo < other.lo) return -1
    if (lo > other.lo) return 1
    0
  }

  /**
   * Dumps the components of this number to a string.
   *
   * return a string showing the components of the number
   */
  def dump: String = "DD<" + hi + ", " + lo + ">"

  // /**
  //  * Returns a string representation of this number, in either standard or scientific notation.
  //  * If the magnitude of the number is in the range [ 10<sup>-3</sup>, 10<sup>8</sup> ]
  //  * standard notation will be used.  Otherwise, scientific notation will be used.
  //  *
  //  * return a string representation of this number
  //  */
//    override def toString: String = {
//      val mag = DD.magnitude(hi)
//      if (mag >= -3 && mag <= 20) return toStandardNotation
//      toSciNotation
//    }

  // /**
  //  * Returns the string representation of this value in standard notation.
  //  *
  //  * return the string representation in standard notation
  //  */
//    def toStandardNotation: String = {
//      val specialStr = getSpecialNumberString
//      if (specialStr != null) return specialStr
//      val magnitude = new Array[Int](1)
//      val sigDigits = extractSignificantDigits(true, magnitude)
//      val decimalPointPos = magnitude(0) + 1
//      var num = sigDigits
//      // add a leading 0 if the decimal point is the first char
//      if (sigDigits.charAt(0) == '.') num = "0" + sigDigits
//      else if (decimalPointPos < 0) num = "0." + DD.stringOfChar('0', -decimalPointPos) + sigDigits
//      else if (sigDigits.indexOf('.') == -1) { // no point inserted - sig digits must be smaller than magnitude of number
//        // add zeroes to end to make number the correct size
//        val numZeroes = decimalPointPos - sigDigits.length
//        val zeroes = DD.stringOfChar('0', numZeroes)
//        num = sigDigits + zeroes + ".0"
//      }
//      if (this.isNegative) return "-" + num
//      num
//    }

//     /**
//      * Returns the string representation of this value in scientific notation.
//      *
//      * return the string representation in scientific notation
//      */
// //    def toSciNotation: String = { // special case zero, to allow as
//      if (isZero) return DD.SCI_NOT_ZERO
//      val specialStr = getSpecialNumberString
//      if (specialStr != null) return specialStr
//      val magnitude = new Array[Int](1)
//      val digits = extractSignificantDigits(false, magnitude)
//      val expStr = DD.SCI_NOT_EXPONENT_CHAR + magnitude(0)
//      // should never have leading zeroes
//      // MD - is this correct?  Or should we simply strip them if they are present?
//      if (digits.charAt(0) == '0') throw new IllegalStateException("Found leading zero: " + digits)
//      // add decimal point
//      var trailingDigits = ""
//      if (digits.length > 1) trailingDigits = digits.substring(1)
//      val digitsWithDecimal = digits.charAt(0) + "." + trailingDigits
//      if (this.isNegative) return "-" + digitsWithDecimal + expStr
//      digitsWithDecimal + expStr
//    }

//    private def extractSignificantDigits(insertDecimalPoint: Boolean, magnitude: Array[Int]): String = {
//      var y = this.abs
//      // compute *correct* magnitude of y
//      var mag = DD.magnitude(y.hi)
//      val scale = DD.TEN.pow(mag)
//      y = y.divide(scale)
//      // fix magnitude if off by one
//      if (y.gt(DD.TEN)) {
//        y = y.divide(DD.TEN)
//        mag += 1
//      }
//      else if (y.lt(DD.ONE)) {
//        y = y.multiply(DD.TEN)
//        mag -= 1
//      }
//      val decimalPointPos = mag + 1
//      val buf = new StringBuffer
//      val numDigits = DD.MAX_PRINT_DIGITS - 1
//      var i = 0
//      import util.control.Breaks._
//      breakable {
//      while ( i <= numDigits ) {
//        if (insertDecimalPoint && i == decimalPointPos) buf.append('.')
//        val digit = y.hi.toInt
//        //      System.out.println("printDump: [" + i + "] digit: " + digit + "  y: " + y.dump() + "  buf: " + buf);
//        /**
//         * This should never happen, due to heuristic checks on remainder below
//         */
//        if (digit < 0 || digit > 9) {
//          //        System.out.println("digit > 10 : " + digit);
//          //        throw new IllegalStateException("Internal errror: found digit = " + digit);
//        }
//
//        /**
//         * If a negative remainder is encountered, simply terminate the extraction.
//         * This is robust, but maybe slightly inaccurate.
//         * My current hypothesis is that negative remainders only occur for very small lo components,
//         * so the inaccuracy is tolerable
//         */
//        if (digit < 0) {
//          break //todo: break is not supported
//          // throw new IllegalStateException("Internal errror: found digit = " + digit);
//        }
//        var rebiasBy10 = false
//        var digitChar = 0
//        if (digit > 9) { // set flag to re-bias after next 10-shift
//          rebiasBy10 = true
//          // output digit will end up being '9'
//          digitChar = '9'
//        }
//        else digitChar = ('0' + digit).toChar
//        buf.append(digitChar)
//        y = y.subtract(DD.valueOf(digit)).multiply(DD.TEN)
//        if (rebiasBy10) y.selfAdd(DD.TEN)
//        var continueExtractingDigits = true
//
//        /**
//         * Heuristic check: if the remaining portion of
//         * y is non-positive, assume that output is complete
//         */
//        //      if (y.hi <= 0.0)
//        //        if (y.hi < 0.0)
//        //        continueExtractingDigits = false;
//        /**
//         * Check if remaining digits will be 0, and if so don't output them.
//         * Do this by comparing the magnitude of the remainder with the expected precision.
//         */
//        val remMag = DD.magnitude(y.hi)
//        if (remMag < 0 && Math.abs(remMag) >= (numDigits - i)) continueExtractingDigits = false
//        if (!continueExtractingDigits) {
//          break //todo: break is not supported}
//        }
//        i += 1
//        magnitude(0) = mag
//        return buf.toString
//      }}
//
//    }

//    private def getSpecialNumberString: String = {
//      if (isZero) return "0.0"
//      if (isNaN) return "NaN "
//      null
//    }
}

object DD {

  /**
   * The value nearest to the constant Pi.
   */
  val PI = new DD(3.141592653589793116e+00, 1.224646799147353207e-16)

  /**
   * The value nearest to the constant 2 * Pi.
   */
  val TWO_PI = new DD(6.283185307179586232e+00, 2.449293598294706414e-16)

  /**
   * The value nearest to the constant Pi / 2.
   */
  val PI_2 = new DD(1.570796326794896558e+00, 6.123233995736766036e-17)

  /**
   * The value nearest to the constant e (the natural logarithm base).
   */
  val E = new DD(2.718281828459045091e+00, 1.445646891729250158e-16)

  /**
   * A value representing the result of an operation which does not return a valid number.
   */
  val NaN = new DD(Double.NaN, Double.NaN)

  /**
   * The smallest representable relative difference between two {link @ DoubleDouble} values
   */
  val EPS = 1.23259516440783e-32

  private def createNaN = new DD(Double.NaN, Double.NaN)

  /**
   * Converts the string argument to a DoubleDouble number.
   *
   * @param str a string containing a representation of a numeric value
   * return the extended precision version of the value
   * throws NumberFormatException if <tt>s</tt> is not a valid representation of a number
   */
//  throws[NumberFormatException]
//  def valueOf(str: String): DD = parse(str)

  /**
   * Converts the <tt>double</tt> argument to a DoubleDouble number.
   *
   * @param x a numeric value
   * return the extended precision version of the value
   */
  def valueOf(x: Double): DD = new DD(x)

  /**
   * The value to split a double-precision value on during multiplication
   */
  private val SPLIT = 134217729.0d // 2^27+1, for IEEE double
  /**
   * Creates a new DoubleDouble with the value of the argument.
   *
   * @param dd the DoubleDouble value to copy
   * return a copy of the input value
   */
  def copy(dd: DD): DD = new DD(dd)

  /**
   * Computes the square of this value.
   *
   * return the square of this value.
   */
  def sqr(x: Double): DD = valueOf(x).selfMultiply(x)

  def sqrt(x: Double): DD = valueOf(x).sqrt

  /**
   * Computes the determinant of the 2x2 matrix with the given entries.
   *
   * @param x1 a double value
   * @param y1 a double value
   * @param x2 a double value
   * @param y2 a double value
   * return the determinant of the values
   */
  def determinant(x1: Double, y1: Double, x2: Double, y2: Double): DD =
    determinant(valueOf(x1), valueOf(y1), valueOf(x2), valueOf(y2))

  /**
   * Computes the determinant of the 2x2 matrix with the given entries.
   *
   * @param x1 a matrix entry
   * @param y1 a matrix entry
   * @param x2 a matrix entry
   * @param y2 a matrix entry
   * return the determinant of the matrix of values
   */
  def determinant(x1: DD, y1: DD, x2: DD, y2: DD): DD = {
    val det = x1.multiply(y2).selfSubtract(y1.multiply(x2))
    det
  }

//  private val MAX_PRINT_DIGITS = 32
//  private val TEN = valueOf(10.0)
//  private val ONE = valueOf(1.0)
//  private val SCI_NOT_EXPONENT_CHAR = "E"
//  private val SCI_NOT_ZERO = "0.0E0"

//   /**
//    * Creates a string of a given length containing the given character
//    *
//    * @param ch  the character to be repeated
//    * @param len the len of the desired string
//    * return the string
//    */
// //  private def stringOfChar(ch: Char, len: Int): String = {
//    val buf = new StringBuffer
//    var i = 0
//    while ( {
//      i < len
//    }) {
//      buf.append(ch)
//      i += 1
//    }
//    buf.toString
//  }

//  private def magnitude(x: Double): Int = {
//    val xAbs = Math.abs(x)
//    val xLog10 = Math.log(xAbs) / Math.log(10)
//    var xMag = Math.floor(xLog10).toInt
//    /**
//     * Since log computation is inexact, there may be an off-by-one error
//     * in the computed magnitude.
//     * Following tests that magnitude is correct, and adjusts it if not
//     */
//    val xApprox = Math.pow(10, xMag)
//    if (xApprox * 10 <= xAbs) xMag += 1
//    xMag
//  }

//  throws[NumberFormatException]
//  def parse(str: String): DD = {
//    var i = 0
//    val strlen = str.length
//    // skip leading whitespace
//    while ( {
//      Character.isWhitespace(str.charAt(i))
//    }) {
//      i += 1;
//      i - 1
//    }
//    // check for sign
//    var isNegative = false
//    if (i < strlen) {
//      val signCh = str.charAt(i)
//      if (signCh == '-' || signCh == '+') {
//        i += 1
//        if (signCh == '-') isNegative = true
//      }
//    }
//    // scan all digits and accumulate into an integral value
//    // Keep track of the location of the decimal point (if any) to allow scaling later
//    val `val` = new DD
//    var numDigits = 0
//    var numBeforeDec = 0
//    var exp = 0
//    var hasDecimalChar = false
//    while ( {
//      true
//    }) {
//      if (i >= strlen) {
//        break //todo: break is not supported}
//        val ch = str.charAt(i)
//        i += 1
//        if (Character.isDigit(ch)) {
//          val d = ch - '0'
//          `val`.selfMultiply(TEN)
//          // MD: need to optimize this
//          `val`.selfAdd(d)
//          numDigits += 1
//          continue //todo: continue is not supported
//        }
//        if (ch == '.') {
//          numBeforeDec = numDigits
//          hasDecimalChar = true
//          continue //todo: continue is not supported
//        }
//        if (ch == 'e' || ch == 'E') {
//          val expStr = str.substring(i)
//          // this should catch any format problems with the exponent
//          try exp = expStr.toInt
//          catch {
//            case _: NumberFormatException =>
//              throw new NumberFormatException("Invalid exponent " + expStr + " in string " + str)
//          }
//          break //todo: break is not supported
//        }
//        throw new NumberFormatException("Unexpected character '" + ch + "' at position " + i + " in string " + str)
//      }
//      var val2 = `val`
//      // correct number of digits before decimal sign if we don't have a decimal sign in the string
//      if (!hasDecimalChar) numBeforeDec = numDigits
//      // scale the number correctly
//      val numDecPlaces = numDigits - numBeforeDec - exp
//      if (numDecPlaces == 0) val2 = `val`
//      else if (numDecPlaces > 0) {
//        val scale = TEN.pow(numDecPlaces)
//        val2 = `val`.divide(scale)
//      }
//      else if (numDecPlaces < 0) {
//        val scale = TEN.pow(-numDecPlaces)
//        val2 = `val`.multiply(scale)
//      }
//      // apply leading sign, if any
//      if (isNegative) return val2.negate
//      return val2
//    }
//  }
}
