package skw.teatime

case class Pseudotime(p: Double) {
  def ~>(other: Pseudotime): Boolean =
    p < other.p

  def ~>(other: PseudoTemporalEnvironment): Boolean =
    this < other.start

  def <(other: Pseudotime): Boolean =
    p < other.p

  def <=(other: Pseudotime): Boolean =
    p <= other.p

  def >(other: Pseudotime): Boolean =
    p > other.p

  def >=(other: Pseudotime): Boolean =
    p >= other.p

  def +(other: Pseudotime): Pseudotime =
    Pseudotime(p + other.p)

  def /(divisor: Double): Pseudotime =
    Pseudotime(p / divisor)
}
