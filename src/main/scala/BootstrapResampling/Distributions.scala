/*
   Picomath public domain implementation of normal distribution CDF
   http://picomath.org/scala/Phi.scala.html
   http://picomath.org/scala/NormalCDFInverse.scala.html
 */

package BootstrapResampling

import scala.math


protected[BootstrapResampling] class Distributions {

  def phi(x: Double): Double = {
    // constants
    val a1: Double =  0.254829592;
    val a2: Double = -0.284496736;
    val a3: Double =  1.421413741;
    val a4: Double = -1.453152027;
    val a5: Double =  1.061405429;
    val p: Double =  0.3275911;

    // Save the sign of x
    val sign = if (x < 0) -1 else 1
    val absqx = math.abs(x)/math.sqrt(2.0);

    // A&S formula 7.1.26, rational approximation of error function
    val t: Double = 1.0/(1.0 + p*absqx);
    val y: Double = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) *
                          t * math.exp(-absqx * absqx);

    0.5 * (1.0 + sign * y)
  }

  //Only defined for 0 < p < 1
  def normalCDFInverse(p: Double): Double = {
    //This is good for 0 < p \leq 0.5
    //This is the rational approximation for the
    //complementary cumulative distribution function
    def rationalApproximation(t: Double) = {
      // Abramowitz and Stegun formula 26.2.23.
      // The absolute value of the error should be less than 4.5 e-4.
      val c: Array[Double] = Array(2.515517, 0.802853, 0.010328)
      val d: Array[Double] = Array(1.432788, 0.189269, 0.001308)
      val numerator: Double = (c(2)*t + c(1))*t + c(0);
      val denominator: Double = ((d(2)*t + d(1))*t + d(0))*t + 1.0;

      t - numerator / denominator
    }

    require(p > 0.0 && p < 1)
    // See article above for explanation of this section.
    if (p < 0.5) {
      // F^-1(p) = - G^-1(p)
      return -rationalApproximation(math.sqrt(-2.0 * math.log(p)));
    } else {
      // F^-1(p) = G^-1(1-p)
      return rationalApproximation(math.sqrt(-2.0 * math.log(1.0 - p)));
    }
  }
}

protected[BootstrapResampling] object Distributions extends Distributions
