package skw.teatime

import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import skw.teatime.TObjectManager
import skw.teatime.Pseudotime
import skw.teatime.memory._

object TObjectManagerSpec extends DefaultRunnableSpec {
  val manager = new TObjectManager[Int]()
  val p = Pseudotime(1)
  val env = Has[SharedMemory.Service](SharedMemory.Service.live)

  def spec =
    suite("TObjectManagerSpec")(
      testM("Create teatime object") {
        (for {
          or <- manager.create(p, 42)
          a <- manager.lookup(or, p)
        } yield assert(a)(equalTo(42)))
      }
    ).provide(env)
}
