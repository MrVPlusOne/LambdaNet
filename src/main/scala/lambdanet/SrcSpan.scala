package lambdanet

case class SrcSpan(
                    start: (Int, Int),
                    until: (Int, Int),
                    srcFile: ProjectPath,
                  ) {
  def showShort(oneBasedLineNumber: Boolean = true): String = {
    val zeroIdx = if (oneBasedLineNumber) 1 else 0
    val start1 = (start._1 + zeroIdx, start._2 + zeroIdx)
    val until1 = (until._1 + zeroIdx, until._2 + zeroIdx)
    s"$start1-$until1"
  }
}
