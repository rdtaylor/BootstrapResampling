package BootstrapResampling

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter


class BootstrapResamplingTest extends FunSuite with BeforeAndAfter {

  def mean(x: Vector[Double]) = {
    x.foldLeft(0.0)((a, b) => a + b) / x.length
  }

  def variance(x: Vector[Double]) = {
    val mu = mean(x)

    x.foldLeft(0.0)((a, b) => a + math.pow(mu - b, 2)) / (x.length - 1)
  }

  val data = Vector(22.8283028, 41.6764537, 14.3581104, 20.4552237, 19.0313830,
                    12.4496791,1.2669327, 29.6890167, 9.4031684, 17.0438982,
                    0.1387493, -0.7459116,2.4532079, -0.7734217, -3.2601918,
                    1.3305351, 0.8977259, -1.0393727,-2.2172627, -3.8445457,
                    0.4352354, 1.4622797, -1.0556765, -2.8137459,0.8129859,
                    1.9504362, 0.4503604, -9.8128885, 2.4238871, 0.3365611)
  var b: Bootstrapper = _
  val B = 5000
  val seed = 1184

  val perc_low = 2.120656563333334
  val per_high = 10.103015473333334
  val bca_low = 2.62535025
  val bca_high = 10.79515345666667

  before {
    b = new Bootstrapper(data, mean, B, seed)
  }

  test("Correctly finds mean") {
    val true_mean = 5.844371
    assert(mean(b.t_tilde) - true_mean < 0.02)
  }

  test("Finds correct percentile CIs for mean") {
    val perc_ci = b.CI("percentile")
    perc_ci match {
      case Left(err) => sys.error(err)
      case Right((low, high)) => {
        assert(low == perc_low)
        assert(high == per_high)
      }
    }
  }

  test("Finds correct BCa CIs for mean") {
    val bca_ci = b.CI("bca")
    bca_ci match {
      case Left(err) => sys.error(err)
      case Right(confInt: ConfInt) => {
        assert(confInt._1 == bca_low)
        assert(confInt._2 == bca_high)
      }
    }
  }

  test("Finds correct percentile CIs for variance") {
    b = new Bootstrapper(data, variance, B, seed)

    val var_perc_low = 49.82842715300962
    val var_perc_high = 220.28869068888378

    val perc_ci = b.CI("percentile")
    perc_ci match {
      case Left(err) => sys.error(err)
      case Right(confInt: ConfInt) => {
        assert(confInt._1 == var_perc_low)
        assert(confInt._2 == var_perc_high)
      }
    }  }

  test("Finds correct BCa CIs for variance") {
    b = new Bootstrapper(data, variance, B, seed)

    val var_bca_low = 67.61649769675655
    val var_bca_high = 286.02288870943335

    val bca_ci = b.CI("bca")
    bca_ci match {
      case Left(err) => sys.error(err)
      case Right(confInt: ConfInt) => {
        assert(confInt._1 == var_bca_low)
        assert(confInt._2 == var_bca_high)
      }
    }
  }

  test("Default CI used") {
    val bca_ci = b.CI()
    bca_ci match {
      case Left(err) => sys.error(err)
      case Right(confInt: ConfInt) => {
        assert(confInt._1 == bca_low)
        assert(confInt._2 == bca_high)
      }
    }
  }
  
  test("Returns Left(err) for bad CI type") {
    val bad_ci = b.CI("KingOfOoo")
    bad_ci match {
      case Left(err) => assert(true)
      case Right(confInt: ConfInt) => {
        sys.error("Returned confidence interval for fake CI type.")
      }
    }
  }

  test("Corrects invalid B") {
    val bad_B = -12 // Invalid and must be greater than 1
    val fixed_boot = new Bootstrapper(data, mean, bad_B, seed)
    assert(fixed_boot.B == default_B)
  }

}
