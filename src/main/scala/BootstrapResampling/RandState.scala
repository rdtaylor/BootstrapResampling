package BootstrapResampling

import scala.util.Random


protected[BootstrapResampling] class RandState(seed: Int) {

  def this() = this(default_seed)

  private[this] var new_seed = seed
  private[this] var r = new Random(seed)

  def nextInt(limit: Int): Int = {
    val result = r.nextInt(limit)
    reseed()
    result
  }

  def reseed(): Unit = {
    new_seed = (new_seed * 0x5DEECE66DL + 0xBL).toInt
    r = new Random(new_seed)
  }

}
