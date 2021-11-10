package skw.teatime

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import skw.teatime.PseudoTemporalEnvironment
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PseudoTemporalEnvironmentSpec extends AnyFlatSpec with should.Matchers with ScalaCheckDrivenPropertyChecks {
  it should "implement ordering" in {
    val p1 = Pseudotime(1)
    val p2 = Pseudotime(2)
    val p3 = Pseudotime(3)
    val p4 = Pseudotime(4)
    PseudoTemporalEnvironment(p1, p2) ~> PseudoTemporalEnvironment(p3, p4) shouldBe true
    PseudoTemporalEnvironment(p1, p2) ~> PseudoTemporalEnvironment(p2, p3) shouldBe false
    PseudoTemporalEnvironment(p3, p4) ~> PseudoTemporalEnvironment(p1, p2) shouldBe false
  }

  it should "generate a subrange" in {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (start, end) =>
      whenever(end > start) {
        val pte = PseudoTemporalEnvironment(Pseudotime(start), Pseudotime(end))
        val subPte = pte.transaction()
        subPte.end > subPte.start shouldBe true
        subPte.start > pte.start shouldBe true
        subPte.start < pte.end shouldBe true
        subPte.end > pte.start shouldBe true
        subPte.end < pte.end shouldBe true
      }
    }
  }

  it should "generate non overlappping subranges" in {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (start, end) =>
      whenever(end > start) {
        val pte = PseudoTemporalEnvironment(Pseudotime(start), Pseudotime(end))
        val subPte1 = pte.transaction()
        val subPte2 = pte.transaction()

        subPte1 ~> subPte2 || subPte2 ~> subPte1 shouldBe true
      }
    }
  }

  it should "generate identical current times in successive calls" in {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (start, end) =>
      whenever(end > start) {
        val pte = PseudoTemporalEnvironment(Pseudotime(start), Pseudotime(end))
        val p1 = pte.current()
        val p2 = pte.current()
        val p3 = pte.current()

        p1 shouldBe p2
        p1 shouldBe p3
        p2 shouldBe p3
      }
    }
  }  
  
  it should "generate next times after the current time" in {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (start, end) =>
      whenever(end > start) {
        val pte = PseudoTemporalEnvironment(Pseudotime(start), Pseudotime(end))
        val p1 = pte.current()
        val p2 = pte.next()

        p1 ~> p2 shouldBe true
      }
    }
  }

  it should "generate subranges after the current time" in {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (start, end) =>
      whenever(end > start) {
        val pte = PseudoTemporalEnvironment(Pseudotime(start), Pseudotime(end))
        val p1 = pte.current()
        val subPte = pte.transaction()

        p1 ~> subPte shouldBe true
      }
    }
  }

  it should "generate increasing next times in successive calls" in {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (start, end) =>
      whenever(end > start) {
        val pte = PseudoTemporalEnvironment(Pseudotime(start), Pseudotime(end))
        val p1 = pte.next()
        val p2 = pte.next()
        val p3 = pte.next()

        p1 ~> p2 shouldBe true
        p2 ~> p3 shouldBe true
        p1 ~> p3 shouldBe true
      }
    }
  }

  it should "generate subranges after the next time" in {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (start, end) =>
      whenever(end > start) {
        val pte = PseudoTemporalEnvironment(Pseudotime(start), Pseudotime(end))
        val p1 = pte.next()
        val subPte = pte.transaction()

        p1 ~> subPte shouldBe true
      }
    }
  }
}
