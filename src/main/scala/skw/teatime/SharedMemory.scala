package skw.teatime

import zio.ZIO
import zio._
import skw.teatime.Version._

object memory {
  type SharedMemory = Has[SharedMemory.Service]

  object SharedMemory extends Serializable {
    trait Service extends Serializable {
      def createObjectHistory[A](or: TObjectRef[A], p: Pseudotime): ZIO[Any, Nothing, Unit]
      def findObjectHistory[A](or: TObjectRef[A]): ZIO[Any, Nothing, History[A]]
    }

    object Service {
      val live: Service = new Service {
        private val objects: scala.collection.mutable.Map[TObjectRef[_], History[_]] =
          scala.collection.mutable.Map.empty

        def createObjectHistory[A](or: TObjectRef[A], p: Pseudotime): ZIO[Any, Nothing, Unit] = {
          val history = new History[A](p)
          objects.put(or, history)
          ZIO.unit
        }

        def findObjectHistory[A](or: TObjectRef[A]): ZIO[Any, Nothing, History[A]] = {
          val history = objects(or).asInstanceOf[History[A]]
          ZIO.succeed(history)
        }
      }
    }

    val any: ZLayer[SharedMemory, Nothing, SharedMemory] =
      ZLayer.requires[SharedMemory]

    val live: Layer[Nothing, SharedMemory] =
      ZLayer.succeed(Service.live)
  }

  def createObjectHistory[A](or: TObjectRef[A], p: Pseudotime): URIO[SharedMemory, Unit] =
    ZIO.accessM(_.get.createObjectHistory(or, p))

  def findObjectHistory[A](or: TObjectRef[A]): URIO[SharedMemory, History[A]] =
    ZIO.accessM(_.get findObjectHistory or)
}
