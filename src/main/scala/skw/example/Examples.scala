package skw.example

import zio.console._
import zio.ZIO
import zio._
import zio.duration._
import zio.random._
import java.util.concurrent.TimeUnit

object Hello extends zio.App {
  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic: ZIO[Console, Nothing, Unit] =
    putStr("Hello")// *> putStr(" world")
}

object CustomEnvironment extends zio.App {
  trait MyEnv {
    def shout(): ZIO[Any, Nothing, Unit]
  }

  object TestEnv extends MyEnv {
    def shout(): ZIO[Any, Nothing, Unit] = {
      println("HELLO!!!")
      ZIO.unit
    }
  }

  def run(args: List[String]) =
    myAppLogic.provide(TestEnv).exitCode

  val myAppLogic: ZIO[MyEnv, Nothing, Unit] =
    for { 
      env <- ZIO.environment[MyEnv]
      _ <- env.shout()
    } yield ()
}

object DecentStackTrace extends zio.App {
  def run(args: List[String]) =
    (myAppLogic.exitCode).catchAllCause(cause => putStr(cause.prettyPrint).exitCode)

  val myAppLogic =
    putStr("Hello") *> ZIO.fail("bad thing") *> putStr("Never happens")
}

object ParallelEffectsWithFiber extends zio.App {
  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic =
    for {
      fiber <- (putStr(".") *> ZIO.sleep(1.second)).forever.fork
      _ <- ZIO.sleep(5.seconds) *> putStr("FINISHED") *> fiber.interrupt
    } yield ()
}

object ParallelComputation extends zio.App {
  def run(args: List[String]) =
    myAppLogic.exitCode

  final case class PiState(insideCount: Long, count: Long)

  def estimatePi(insideCount: Long, count: Long): Double = 
    (insideCount.toDouble / count.toDouble) * 4.0

  def insideCircle(x: Double, y: Double): Boolean = 
    Math.sqrt(x * x + y * y) <= 1.0

  val randomPoint: ZIO[Random, Nothing, (Double, Double)] = nextDouble zip nextDouble  

  def updateOnce(ref: Ref[PiState]): ZIO[Random, Nothing, Unit] = 
    for {
      tuple <- randomPoint
      (x, y) = tuple
      insideDelta = if (insideCircle(x, y)) 1 else 0
      _ <- ref.update(state => PiState(state.insideCount + insideDelta, state.count + 1))
    } yield ()

  val myAppLogic =
    for {
      ref <- Ref.make(PiState(0, 0))
      worker = updateOnce(ref).forever
      workers = List.fill(4)(worker)
      compositeFiber <- ZIO.forkAll_(workers)
      _ <- ZIO.sleep(1.second)
      pi <- ref.get
      _ <- putStr("PI: " + estimatePi(pi.insideCount, pi.count))
    } yield ()

}