package BootstrapResampling

import scala.math._


protected[BootstrapResampling] object ConfidenceInterval {

  def percentile( t_tilde: SingleSample
                , B: Int
                , alpha: Double = 0.05
                ): ConfInt = {

    def conservativePercentile(t_tilde: SingleSample, q: Double): N = {
      val monotonic_t = t_tilde.sorted
      val index = {
        val index_r = B * q : Double
        if (q < 0.5) { ceil(index_r) }
        else { floor(index_r)  - 1.0 }
      }

      monotonic_t(index.toInt)
    }

    val low = conservativePercentile(t_tilde, alpha/2)
    val high = conservativePercentile(t_tilde, 1 - alpha/2)

    (low, high)
  }

  def bca(
      data: SingleSample
    , t_tilde: SingleSample
    , t_star: N
    , statistic: SingleSample => N
    , n: Int
    , B: Int
    , alpha: Double = 0.05
  ): ConfInt = {

    def intervalIndexes(
        data: SingleSample
      , t_tilde: SingleSample
      , t_star: N
      , statistic: SingleSample => N
      , n: Int
      , alpha: Double
      ): (Int, Int) = {

      def biasCorrection(t_tilde: SingleSample, t_star: N): Double = {
        Distributions.normalCDFInverse((t_tilde.filter(_ < t_star).length) /
                                       B.toDouble)
      }

      def accel( data: SingleSample
               , statistic: SingleSample => N
               ): Double = {

        val theta_dot = Jackknife.jackknife(data, statistic, n)
        val theta_i = Jackknife.jackknifeList(data, statistic, n)
        val theta_diffs = theta_i.map(x => theta_dot - x)

        val num = theta_diffs
          .foldLeft(0.0)((acc, th_d) => acc + pow(th_d, 3))
        val den = 6 * pow(theta_diffs
          .foldLeft(0.0)((acc, th_d) => acc + pow(th_d, 2)), 3.0 / 2.0)

        num / den
      }

      def intervalEndpoint(z0: Double, za: Double, a: Double): Double = {
        Distributions.phi(z0 + ((z0 + za) / (1 - a * (z0 + za))))
      }

      val z0 = biasCorrection(t_tilde, t_star)
      val a = accel(data, statistic)

      val zla = Distributions.normalCDFInverse(alpha / 2)
      val zha = Distributions.normalCDFInverse(1 - alpha / 2)

      val a1 = intervalEndpoint(z0, zla, a)
      val a2 = intervalEndpoint(z0, zha, a)

      val low_index = floor(a1 * B).toInt
      val high_index = ceil(a2 * B).toInt - 1

      (low_index, high_index)
    }

    val (low_index, high_index) = intervalIndexes(data, t_tilde, t_star,
                                                  statistic, n, alpha)
    val t_tilde_sorted = t_tilde.sorted

    (t_tilde_sorted(low_index), t_tilde_sorted(high_index))
  }

}
