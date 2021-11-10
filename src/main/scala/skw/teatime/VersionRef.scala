package skw.teatime

import zio._
import skw.teatime.memory._
import skw.teatime.Version._
import skw.teatime.Version.VersionError._

// A two part name composed of an object reference and a selector called a pseudo-time
// To get from a version ref to a value...
// 1. First select the or object history from memory (object history is a function from pseudo-times to versions)
// 2. Use p to extract the correct version from the history
class VersionRef[A](or: TObjectRef[A], p: Pseudotime) {
  import VersionRef._

  // This operation creates the version specified by the version reference.
  // It is used in the desugaring of an update operation on an object.
  // The second parameter is a value that will be the value of the version
  // referred to by the version reference, if no error is signalled.
  // The nonexistent_state error indicates that an attempt to assign a
  // version that never will exist (no version of the object exists for
  // that pseudo-time). The redefinition error indicates that there was
  // already a valid version associated with the specified version reference.
  // The version_refSdefine operation can be applied at most once to a
  // version reference.
  def define(a: A): ZIO[SharedMemory, VersionError, Unit] = {
    val version = Version[A](p, a)
    for {
      history <- findObjectHistory[A](or)
      _ <- history.add(version)
    } yield ()
  }

  // This operation gets the version associated with a particular 
  // version reference. It is used in the desugaring of an operation
  // that reads the value of an object. 
  // The nonexistent_state error indicates that the version reference
  // specifies a state that will never have existed.
  def lookup(): ZIO[SharedMemory, VersionError, Version[A]] =
    for {
      history <- findObjectHistory[A](or)
      version <- history.versionAt(p)
    } yield version
}

object VersionRef {
  // Generates a version ref from an object ref and psuedo-time
  def freeze[A](or: TObjectRef[A], p: Pseudotime): ZIO[Any, Nothing, VersionRef[A]] =
    ZIO.succeed(new VersionRef[A](or, p))
}
