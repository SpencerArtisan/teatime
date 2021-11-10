package skw.teatime

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class PseudotimeSpec extends AnyFlatSpec with should.Matchers {
  it should "implement less than" in {
    val p1 = Pseudotime(1)
    val p2 = Pseudotime(2)
    p1 < p2 shouldBe true
    p1 < p1 shouldBe false
    p2 < p1 shouldBe false
  }

  it should "implement less than or equal to" in {
    val p1 = Pseudotime(1)
    val p2 = Pseudotime(2)
    p1 <= p2 shouldBe true
    p1 <= p1 shouldBe true
    p2 <= p1 shouldBe false
  }

  it should "implement ordering" in {
    val p1 = Pseudotime(1)
    val p2 = Pseudotime(2)
    p1 ~> p2 shouldBe true
    p1 ~> p1 shouldBe false
    p2 ~> p1 shouldBe false
  }
}
