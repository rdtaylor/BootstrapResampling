package BootstrapResampling


object Jackknife {

  def jackknifeList( s: SingleSample
                   , statistic: SingleSample => N
                   , n: Int
                   ): SingleSample = {

    def drop(s: SingleSample, i: Int, n: Int): SingleSample = {
      s.slice(0, i) ++ s.slice(i + 1, n)
    }

    val t_tilde = (0 to (n - 1)).map(i => {
      val s_i = drop(s, i, n)
      statistic(s_i)
    })

    toSample(t_tilde)
  }

  def jackknife(s: SingleSample, statistic: SingleSample => N, n: Int): N = {
    val t_tilde = jackknifeList(s, statistic, n)
    t_tilde.foldLeft(0.0)((a, b) => a + b) / s.length
  }
}
