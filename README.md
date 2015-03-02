# BootstrapResampling

This library permits simple bootstrap resampling of given data.


## Usage:

Example vignette:

```
import BootstrapResampling._


object Example {

  def mean(x: Vector[Double]) = {
    x.foldLeft(0.0)((a, b) => a + b) / x.length
  }

  def main(args: Array[String]): Unit = {
    val data = Vector(1.0, 2.0, 3.0, 4.0, 5.0)
    val boot = new Bootstrapper(data, mean, 2500, 1453)
    val ci = boot.CI("bca")

    ci match {
      case Left(err) => sys.error(err)
      
      case Right((low, high)) => {
        println("The low is: %.3f".format(low))
        println("The high is: %.3f".format(high))
      }
    }
  }

}

```

* `new Bootstrapper(data: Vector[Double], statistic: Vector[Double] => Double, B: Int, seed: Int)`

This line instantiates a new Bootstrapper object.  The parameters are:
> * `data`, which contains the data to be bootstrapped
> * `statistic`, the function returning a statistic "statistic"
> * `B` (optional with a default of 1000), the number of bootstrap samples to take
> * `seed` (optional with a default of 1), the seed to start the pseudo-random number generator

* `Bootstrapper.CI(method: String): Either[String, (Double, Double)]`

CI() calculates a confidence interval for the given object.  The potential values are:
> * "bca" -- This option is the default when omitted.  This method provides BCa, or bias-corrected and accelerated, interval
> * "percentile" -- This choice gives a percentile-based interval

The return value will be a Right((low: Double, high: Double)) tuple or Left(err: String) containing the error encountered.

* Other exposed members in the Bootstrapper class:
> * `samples`: Vector[Vector[Double]] -- A vector containing the bootstrap samples randomly chosen
> * `t_tilde`: Vector[Double] -- A vector containing the calculated statistics for the bootstrap samples
> * `t_star`: Double -- The calculated statistic for the base `data` parameter
> * `bias`: Double -- A lazy bias estimate based on the average difference between `t_tilde` and `t_star`

* Jackknife

Simple jackknifing functions are also provided publicly.  They are:

> * `Jackknife.jackknifeList(s: Vector[Double], statistic: Vector[Double] => Double, n: Int): Vector[Double]`

> This method takes a sample s, a statistic statistic, and an integer containing the length of sample s (i.e. ```s.length```) and returns the jackknifed values of the statistic for each element omitted from the sample.

> * `Jackknife.jackknife(s: Vector[Double], statistic: Vector[Double] => Double, n: Int): Double`

> This method takes the same parameters as the above and calls it.  Once the jackknifed values are returned, it simplly returns the mean of the statistics.


See tests in src/test for examples.


# Compilation:

In sbt, type `assembly` to create the .jar file.  It will be placed in target/scala-${SCALA_VERSION}/BootstrapResampling.jar.  Then, include the .jar in your project for use.
