package skw.teatime

import skw.teatime.memory._
import skw.teatime.time.{PTEnv, current}
import zio._
import zio.test.Assertion._
import zio.test._

object TeatimeManagerSpec extends DefaultRunnableSpec {
  val manager = new TeatimeManager[Int]()
  val p = Pseudotime(1)
  val env = Has.allOf[SharedMemory.Service, PTEnv.Service](SharedMemory.Service.live, PTEnv.Service.live)

  def spec =
    suite("Teatime ManagerSpec")(
      testM("Create teatime object") {
        (for {
          or <- manager.create(42)
          a <- manager.lookup(or)
        } yield assert(a)(equalTo(42)))
      },
      testM("Mutate teatime object") {
        (for {
          or <- manager.create(42)
          _ <- manager.mutate(or, _ + 1)
          p <- current()
          a <- manager.lookup(or)
        } yield assert(a)(equalTo(43)))
      }
    ).provide(env)
}
