package lambdanet.correctness

/**
  * Given initial temperature and epoch, returns
  */
trait Schedule extends Function1[Int, Double] {
  def t0: Double

  def description: String
}

case class LogSchedule(t0: Double) extends Schedule {
  val description: String =
    "Logarithmic schedule (t(k) = t0 * log(2) / log(k+1))"

  override def apply(epoch: Int): Double =
    t0 * math.log(2) / math.log(epoch + 1)
}
