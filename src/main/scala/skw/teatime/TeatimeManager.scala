package skw.teatime

import skw.teatime.Version._
import skw.teatime.memory._
import skw.teatime.time.{PTEnv, current, next}
import zio.ZIO

class TeatimeManager[A] {
  def create(a: A): ZIO[SharedMemory with PTEnv, VersionError, TObjectRef[A]] =
    for {
      p <- current()
      or <- TObjectRef.create[A](p)
      vr <- VersionRef.freeze(or, p)
      _ <- vr.define(a)
    } yield or

  def mutate(or: TObjectRef[A], mutator: A => A): ZIO[SharedMemory with PTEnv, VersionError, TObjectRef[A]] =
    for {
      a1 <- lookup(or)
      p <- next()
      vr <- VersionRef.freeze(or, p)
      a2 = mutator(a1)
      _ <- vr.define(a2)
    } yield or

  def lookup(or: TObjectRef[A]): ZIO[SharedMemory with PTEnv, VersionError, A] =
    for {
      p <- current()
      history <- findObjectHistory(or)
      version <- history.versionAt(p)
    } yield version.a
}
