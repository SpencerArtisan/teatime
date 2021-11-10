package skw.teatime

import zio.console._
import zio.ZIO
import zio._
import skw.teatime.memory._
import skw.teatime.Version.VersionError

object TeaTime extends zio.App {

  def run(args: List[String]) = {
    val live =
      Has.allOf[Console.Service, SharedMemory.Service](
        Console.Service.live,
        SharedMemory.Service.live
      )
    myAppLogic.provide(live).exitCode
  }

  val myAppLogic: ZIO[Console with SharedMemory, VersionError, Unit] = {
    val manager = new TObjectManager[Int]()
    val p1 = Pseudotime(1)

    for {
      or <- manager.create(p1, 42)
      _ <- putStrLn(">>> TObjectRef: " + or)
      value <- manager.lookup(or, p1)
      _ <- putStrLn(">>> Value: " + value)
    } yield ()
  }
}
