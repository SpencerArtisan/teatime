package skw.teatime

import zio._
import zio.ZIO
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import skw.teatime.TObjectRef
import skw.teatime.Pseudotime
import skw.teatime.memory._
import skw.teatime.TObjectRef.TObjectError._

object TObjectRefSpec extends DefaultRunnableSpec {
  val p1 = Pseudotime(1)
  val p2 = Pseudotime(2)
  val p3 = Pseudotime(3)
  val env = Has[SharedMemory.Service](SharedMemory.Service.live)

  def history[A](or: TObjectRef[A]): ZIO[SharedMemory, Nothing, Version.History[A]] =
    ZIO.accessM(_.get.findObjectHistory(or))

  def spec =
    suite("TObjectRefSpec")(
      testM("Creates an object ref without any versions") {
        for {
          or <- TObjectRef.create[Int](p1)
          h <- history(or)
        } yield assert(h.versionCount)(equalTo(0))
      },
      testM("Creates an object with the specified creation time") {
        for {
          or <- TObjectRef.create[Int](p1)
          h <- history(or)
        } yield assert(h.createdAt)(equalTo(p1))
      },
      testM("Can create two different objects") {
        for {
          or1 <- TObjectRef.create[Int](p1)
          or2 <- TObjectRef.create[Int](p1)
        } yield assert(or1)(not(equalTo(or2)))
      },
      testM("Can delete an object without versions") {
        for {
          or <- TObjectRef.create[Int](p1)
          _ <- or.delete[Int](p2)
          h <- history(or)
        } yield assert(h.deletedAt)(equalTo(Some(p2)))
      },
      testM("Can delete an object after the last version") {
        for {
          or <- TObjectRef.create[Int](p1)
          vr <- VersionRef.freeze[Int](or, p2)
          _ <- vr.define(42)
          _ <- or.delete[Int](p3)
          h <- history(or)
        } yield assert(h.deletedAt)(equalTo(Some(p3)))
      },
      testM("Fails when an object is double deleted") {
        for {
          or <- TObjectRef.create[Int](p1)
          _ <- or.delete[Int](p2)
          res <- or.delete[Int](p3).run
        } yield assert(res)(fails(equalTo(BadDelete(p3))))
      },
      testM("Fails when the delete is before the create") {
        for {
          or <- TObjectRef.create[Int](p2)
          res <- or.delete[Int](p1).run
        } yield assert(res)(fails(equalTo(BadDelete(p1))))
      },
      testM("Fails when the delete is at the same time as the create") {
        for {
          or <- TObjectRef.create[Int](p1)
          res <- or.delete[Int](p1).run
        } yield assert(res)(fails(equalTo(BadDelete(p1))))
      },
      testM("Fails when the delete is at the same time as a version") {
        for {
          or <- TObjectRef.create[Int](p1)
          vr <- VersionRef.freeze[Int](or, p2)
          _ <- vr.define(42)
          res <- or.delete[Int](p2).run
        } yield assert(res)(fails(equalTo(BadDelete(p2))))
      },
      testM("Fails when the delete is at a time before a version") {
        for {
          or <- TObjectRef.create[Int](p1)
          vr <- VersionRef.freeze[Int](or, p3)
          _ <- vr.define(42)
          res <- or.delete[Int](p2).run
        } yield assert(res)(fails(equalTo(BadDelete(p2))))
      },
    ).provide(env)
}
