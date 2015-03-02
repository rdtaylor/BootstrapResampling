package object BootstrapResampling {
  type N = Double
  type SingleSample = Vector[N]
  type Samples = Vector[SingleSample]
  type ConfInt = (N, N)

  def toSample[T](l: scala.collection.Seq[T]): Vector[T] = l.toVector
  def toSample[T](l: scala.collection.parallel.ParSeq[T]): Vector[T] =
    l.toVector

  protected[BootstrapResampling] val default_B = 1000
  protected[BootstrapResampling] val default_seed = 1
}
