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

object VersionSpec extends DefaultRunnableSpec {
  val p1 = Pseudotime(1)
  val p2 = Pseudotime(2)
  val p3 = Pseudotime(3)

  def spec =
    suite("VersionSpec")(
      testM("Can define a version with a given value") {
        val v = Version(p1, 42)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v)
          retrieved <- h.versionAt(p1)
        } yield assert(retrieved)(equalTo(v))
      },
      testM("Can define multiple versions with different values") {
        val v1 = Version(p1, 42)
        val v2 = Version(p2, 43)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v1)
          _ <- h.add(v2)
          retrieved1 <- h.versionAt(p1)
          retrieved2 <- h.versionAt(p2)
        } yield assert(retrieved1)(equalTo(v1)) && assert(retrieved2)(equalTo(v2))
      },
      testM("Can retrieve a version before a given time") {
        val v = Version(p1, 42)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v)
          retrieved <- h.versionAt(p2)
        } yield assert(retrieved)(equalTo(v))
      },
      testM("Can retrieve the closest earlier version") {
        val v1 = Version(p1, 42)
        val v2 = Version(p2, 43)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v1)
          _ <- h.add(v2)
          retrieved <- h.versionAt(p3)
        } yield assert(retrieved)(equalTo(v2))
      },
      testM("Fails when trying to define a version at the same time as another version") {
        val v1 = Version(p1, 42)
        val v2 = Version(p1, 43)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v1)
          res <- h.add(v2).run
        } yield assert(res)(fails(equalTo(RedefinitionError(p1))))
      },
      testM("Fails when trying to define a version on a deleted object") {
        val v = Version(p2, 42)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.delete(p1)
          res <- h.add(v).run
        } yield assert(res)(fails(equalTo(ObjectDeletedError(p2))))
      },
      testM("Fails when trying to define a version before the object creation time") {
        val v = Version(p1, 42)
        val h = new Version.History[Int](p2)
        for {
          res <- h.add(v).run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p1))))
      },
      testM("Fails when trying to lookup a version before the object creation time") {
        val h = new Version.History[Int](p2)
        for {
          res <- h.versionAt(p1).run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p1))))
      },
      testM("Fails when trying to lookup a version in an object without versions") {
        val h = new Version.History[Int](p1)
        for {
          res <- h.versionAt(p2).run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p2))))
      },
      testM("Fails when trying to lookup a version at the deletion time") {
        val v = Version(p1, 42)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v)
          _ <- h.delete(p2)
          res <- h.versionAt(p2).run
        } yield assert(res)(fails(equalTo(ObjectDeletedError(p2))))
      },     
       testM("Fails when trying to lookup a version after the deletion time") {
        val v = Version(p1, 42)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v)
          _ <- h.delete(p2)
          res <- h.versionAt(p3).run
        } yield assert(res)(fails(equalTo(ObjectDeletedError(p3))))
      },
      testM("Fails when trying to lookup a version which is before all versions defined on the object") {
        val v = Version(p2, 42)
        val h = new Version.History[Int](p1)
        for {
          _ <- h.add(v)
          res <- h.versionAt(p1).run
        } yield assert(res)(fails(equalTo(NonExistentStateError(p1))))
      }
    )
}
