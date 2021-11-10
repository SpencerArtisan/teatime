package skw.teatime

import zio.ZIO
import zio._
import skw.teatime.Version._

object time {
  type PTEnv = Has[PTEnv.Service]

  object PTEnv extends Serializable {
    trait Service extends Serializable {
      def current(): UIO[Pseudotime]
      def next(): UIO[Pseudotime]
    }

    object Service {
      val live: Service = new Service {
        private val pte = PseudoTemporalEnvironment(Pseudotime(1), Pseudotime(2))

        def current(): UIO[Pseudotime] = {
          ZIO.succeed(pte.current())
        }

        def next(): UIO[Pseudotime] = {
          ZIO.succeed(pte.next())
        }
      }
    }

    val any: ZLayer[PTEnv, Nothing, PTEnv] =
      ZLayer.requires[PTEnv]

    val live: Layer[Nothing, PTEnv] =
      ZLayer.succeed(Service.live)
  }

  def current(): URIO[PTEnv, Pseudotime] =
    ZIO.accessM(_.get.current())

  def next(): URIO[PTEnv, Pseudotime] =
    ZIO.accessM(_.get.next())
}
