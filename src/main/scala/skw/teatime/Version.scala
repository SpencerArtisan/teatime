package skw.teatime

import zio.ZIO
import skw.teatime.Version.VersionError._
import scala.collection.mutable

case class Version[A](p: Pseudotime, a: A)

object Version {
  trait VersionError

  object VersionError {
    case class NonExistentStateError(p: Pseudotime) extends VersionError
    case class RedefinitionError(p: Pseudotime) extends VersionError
    case class ObjectDeletedError(p: Pseudotime) extends VersionError
  }

  class History[A](val createdAt: Pseudotime) {
    var deletedAt: Option[Pseudotime] = None
    private val versions: mutable.ListBuffer[Version[A]] = mutable.ListBuffer.empty

    def delete(p: Pseudotime): ZIO[Any, Nothing, Unit] =
      ZIO.effectTotal(this.deletedAt = Some(p))

    def versionCount: Int = versions.size

    def lastVersion: Option[Version[A]] = versions.sortBy(_.p.p).lastOption

    def isDeletedBy(p: Pseudotime): Boolean = 
      deletedAt.exists(_ <= p)

    def versionAt(p: Pseudotime): ZIO[Any, VersionError, Version[A]] =
      if (isDeletedBy(p))
        ZIO.fail(ObjectDeletedError(p))
      else
        versions
          .takeWhile(_.p <= p)
          .lastOption
          .map(v => ZIO.succeed(v))
          .getOrElse(ZIO.fail(NonExistentStateError(p)))

    def add(version: Version[A]): ZIO[Any, VersionError, Unit] =
      if (versions.exists(_.p == version.p))
        ZIO.fail(RedefinitionError(version.p))
      else if (version.p < createdAt)
        ZIO.fail(NonExistentStateError(version.p))
      else if (isDeletedBy(version.p))
        ZIO.fail(ObjectDeletedError(version.p))
      else
        ZIO.succeed(versions.addOne(version))
  }
}
