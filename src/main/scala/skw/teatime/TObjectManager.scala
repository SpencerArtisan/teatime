package skw.teatime

import zio.ZIO
import skw.teatime.memory._
import Version._
import Version.VersionError.NonExistentStateError

class TObjectManager[A] {
  def create(p: Pseudotime, a: A): ZIO[SharedMemory, VersionError, TObjectRef[A]] =
    for {
      or <- TObjectRef.create[A](p)
      vr <- VersionRef.freeze(or, p)
      _ <- vr.define(a)
    } yield or

  def lookup(or: TObjectRef[A], p: Pseudotime): ZIO[SharedMemory, VersionError, A] =
    for {
      history <- findObjectHistory(or)
      version <- history.versionAt(p)
    } yield version.a
}
