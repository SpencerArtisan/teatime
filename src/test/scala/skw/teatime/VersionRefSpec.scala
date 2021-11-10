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
import skw.teatime.Version.VersionError._

object VersionRefSpec extends DefaultRunnableSpec {
  val p1 = Pseudotime(1)
  val p2 = Pseudotime(2)
  val p3 = Pseudotime(3)
  val env = Has[SharedMemory.Service](SharedMemory.Service.live)

  def history[A](or: TObjectRef[A]): ZIO[SharedMemory, Nothing, Version.History[A]] =
    ZIO.accessM(_.get.findObjectHistory(or))

  def spec =
    suite("VersionRefSpec")(
      testM("Can create two different version refs for an object at the same time") {
        (for {
          or <- TObjectRef.create[Int](p1)
          vr1 <- VersionRef.freeze[Int](or, p1)
          vr2 <- VersionRef.freeze[Int](or, p1)
        } yield assert(vr1)(not(equalTo(vr2))))
      },
      testM("Can define a version with a given value") {
        (for {
          or <- TObjectRef.create[Int](p1)
          vr <- VersionRef.freeze[Int](or, p1)
          _ <- vr.define(42)
          h <- history(or)
          v <- h.versionAt(p1)
        } yield assert(v)(equalTo(Version(p1, 42))))
      },
      testM("Can define multiple versions with different values") {
        (for {
          or <- TObjectRef.create[Int](p1)
          vr1 <- VersionRef.freeze[Int](or, p1)
          _ <- vr1.define(42)
          vr2 <- VersionRef.freeze[Int](or, p2)
          _ <- vr2.define(43)
          h <- history(or)
          v1 <- h.versionAt(p1)
          v2 <- h.versionAt(p2)
        } yield assert(v1)(equalTo(Version(p1, 42))) && assert(v2)(equalTo(Version(p2, 43))))
      },
      testM("Can lookup a version from a version reference") {
        (for {
          or <- TObjectRef.create[Int](p1)
          vr <- VersionRef.freeze[Int](or, p1)
          _ <- vr.define(42)
          retrieved <- vr.lookup()
        } yield assert(retrieved)(equalTo(Version(p1, 42))))
      },
      testM("Fails when trying to define a version at the same time as another version") {
        (for {
          or <- TObjectRef.create[Int](p1)
          vr <- VersionRef.freeze[Int](or, p1)
          _ <- vr.define(42)
          res <- vr.define(42).run
        } yield assert(res)(fails(equalTo(RedefinitionError(p1)))))
      },
      testM("Fails when trying to define a version before the object creation time") {
        (for {
          or <- TObjectRef.create[Int](p2)
          vr <- VersionRef.freeze[Int](or, p1)
          res <- vr.define(42).run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p1)))))
      },
      testM("Fails when trying to lookup a version before the object creation time") {
        (for {
          or <- TObjectRef.create[Int](p2)
          vr <- VersionRef.freeze[Int](or, p1)
          res <- vr.lookup().run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p1)))))
      },
      testM("Fails when trying to lookup a version in an object without versions") {
        (for {
          or <- TObjectRef.create[Int](p2)
          vr <- VersionRef.freeze[Int](or, p2)
          res <- vr.lookup().run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p2)))))
      },
      testM("Fails when trying to lookup a version which is not defined on the object") {
        (for {
          or <- TObjectRef.create[Int](p2)
          vr1 <- VersionRef.freeze[Int](or, p3)
          _ <- vr1.define(42)
          vr2 <- VersionRef.freeze[Int](or, p2)
          res <- vr2.lookup().run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p2)))))
      },
    ).provide(env)
}
