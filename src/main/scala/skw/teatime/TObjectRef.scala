package skw.teatime

import zio.ZIO
import skw.teatime.memory._
import TObjectRef.TObjectError._

// Cannot be used to access objects, only used as params to function.
// Instead a version reference is required to read and write a value from memory.
class TObjectRef[A] {
  // This operation deletes the object specified by the first parameter, 
  // by setting the deletion pseudo-time to the second parameter. 
  // The signal bad_delete indicates the delete operation was not performed 
  // because the specified pseudo-time was inconsistent with the history 
  // of the object. This could be because the pseudo-time preceded the 
  // creation pseudo-time or because a version corresponding to that 
  // pseudo-time exists, or because the delete pseudo-time has already 
  // been set to another value. As in the version_refSdefine operation, 
  // only one such operation may ever be applied to the particular object 
  // reference.
  def delete[A](p: Pseudotime): ZIO[SharedMemory, BadDelete, Unit] =
    for {
      history <- findObjectHistory(this)
      _ <- (history.createdAt, history.deletedAt, history.lastVersion) match {
          case (_, Some(_), _) =>
             ZIO.fail(BadDelete(p))
          case (createdAt, None, _) if p <= createdAt =>
             ZIO.fail(BadDelete(p))
          case (_, None, Some(lastVersion)) if p <= lastVersion.p =>
             ZIO.fail(BadDelete(p))
          case (createdAt, None, _) =>
            history.delete(p)
      }
    } yield ()
}

object TObjectRef {
  trait TObjectError

  object TObjectError {
    case class BadDelete(p: Pseudotime) extends TObjectError
  }

  // This operation creates an object reference whose tcreate
  // is specified by the parameter.
  def create[A](p: Pseudotime): ZIO[SharedMemory, Nothing, TObjectRef[A]] = {
    val or = new TObjectRef[A]()
    for {
      history <- createObjectHistory(or, p)
    } yield or
  }
}