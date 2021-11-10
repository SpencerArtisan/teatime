package skw.teatime

import zio.console._
import zio.ZIO
import zio._
import zio.duration._
import zio.random._
import java.util.concurrent.TimeUnit

object TeaTimeSimple extends zio.App {
  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic = {
    val manager = new TObjectManager[Int]()
    val time1 = Pseudotime(1)
    for {
      or <- manager.create(time1, 42)
      _ <- putStrLn(">>> TObjectRef: " + or)
      value <- manager.lookup(or, time1)
      _ <- putStrLn(">>> Value: " + value)
    } yield ()
  }

  class TObjectManager[A] {

    def create(p: Pseudotime, a: A): ZIO[Any, Nothing, TObjectRef[A]] = {
      val or = TObjectRef.create[A](p)
      val vr = VersionRef(or, p)
      vr.define(a)
      ZIO.succeed(or)
    }

    def lookup(or: TObjectRef[A], p: Pseudotime): ZIO[Any, Nothing, A] = {
      val history = SharedMemory.findObjectHistory(or)
      val version = history.versions(p)
      ZIO.succeed(version.a)
    }
  }

  object SharedMemory {
    val objects: scala.collection.mutable.Map[TObjectRef[_], ObjectHistory[_]] =
      scala.collection.mutable.Map.empty

    def createObjectHistory[A](or: TObjectRef[A]): Unit = {
      val history = new ObjectHistory[A]()
      objects.put(or, history)
    }

    def findObjectHistory[A](or: TObjectRef[A]): ObjectHistory[A] = {
      objects(or).asInstanceOf[ObjectHistory[A]]
    }
  }

  case class Pseudotime(p: Long) {}

  case class Version[A](p: Pseudotime, a: A)

// Cannot be used to access objects, only used as params to function.
// Instead a version reference is required to read and write a value from memory.
  case class TObjectRef[A]()

  object TObjectRef {
    // Creates an object ref whose t-create is speicified by the given value
    def create[A](p: Pseudotime): TObjectRef[A] = {
      val or = TObjectRef[A]()
      SharedMemory.createObjectHistory(or)
      or
    }

    // Deletes an object whose t-delete is speicified by the given value.
    // May only be called once for a particular object reference
    def delete[A](or: TObjectRef[A], p: Pseudotime) =
      ???
  }

// A two part name composed of an object reference and a selector called a pseudo-time
// To get from a version ref to a value...
// 1. First select the or object history from memory (object history is a function from pseudo-times to versions)
// 2. Use p to extract the correct version from the history
  case class VersionRef[A](or: TObjectRef[A], p: Pseudotime) {
    // Creates a version. Can be applied at most once.
    // Each value change assoicated with an object requires that this method must have been called.
    def define(a: A): Unit = {
      val history = SharedMemory.findObjectHistory[A](or)
      val version = Version[A](p, a)
      history.add(version)
    }

    // Gets the version associated with a particular version reference
    def lookup(): Version[A] = {
      val history = SharedMemory.findObjectHistory[A](or)
      history.versions(p)
    }
  }

  object VersionRef {
    // Generates a version ref from an object ref and psuedo-time
    def freeze[A](or: TObjectRef[A], p: Pseudotime): VersionRef[A] =
      VersionRef[A](or, p)
  }

  class ObjectHistory[A]() {
    val versions: scala.collection.mutable.Map[Pseudotime, Version[A]] =
      scala.collection.mutable.Map.empty

    def add(version: Version[A]): Unit = {
      versions.put(version.p, version)
    }
  }
}
