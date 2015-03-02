package BootstrapResampling


class Bootstrapper(data: SingleSample, statistic: SingleSample => N,
                   B_in: Int, seed: Int) {

  def this(data: SingleSample, statistic: SingleSample => N, B_in: Int) =
    this(data, statistic, B_in, default_seed)

  def this(data: SingleSample, statistic: SingleSample => N) =
    this(data, statistic, default_B)

  // B bootstrap samples must be a whole number
  val B = if (B_in < 1) default_B
          else B_in
  val n = data.length: Int
  val r = new RandState(seed)

  protected def bootSample(data: SingleSample, r: RandState): Samples = {

    def singleSample(data: SingleSample, r: RandState): SingleSample = {

      val single_sample = (1 to this.n).map(b => {
        val i = r.nextInt(this.n)
        data(i)
      })

      toSample(single_sample)
    }

    val samples = (1 to this.B).map(b => {
      singleSample(data, r)
    })

    toSample(samples)
  }

  protected def calcStatistics(samples: Samples,
                     statistic: SingleSample => N): SingleSample = {

    // Use parallelism here for speedier computation on complex statistics
    val statistics = samples.par.map(s => {
      statistic(s)
    })

    toSample(statistics)
  }

  def CI(method: String): Either[String, ConfInt] = method match {
    case "bca"        => Right(ConfidenceInterval.bca( this.data
                                                     , this.t_tilde
                                                     , this.t_star
                                                     , this.statistic
                                                     , this.n
                                                     , this.B
                                                     ))
    case "percentile" => Right(ConfidenceInterval.percentile( this.t_tilde
                                                            , this.B
                                                            ))
    case _            => Left("Invalid bootstrapping method supplied.")
  }

  def CI(): Either[String, ConfInt] = CI("bca")

  protected def bootBias(t_tilde: SingleSample, t_star: N, B: Int): N = {
    t_tilde.foldLeft(0.0)((a, b) => a + b - t_star) / B
  }


  val samples = bootSample(data, r)
  val t_tilde = calcStatistics(samples, statistic)
  val t_star = statistic(data)
  lazy val bias = bootBias(t_tilde, t_star, B)
}
