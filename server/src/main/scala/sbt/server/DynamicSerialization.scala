package sbt
package server

import scala.pickling.{ FastTypeTag, SPickler, Unpickler }
import sbt.protocol._

/** Helper class lookups for serialization/deserialization. */
private object Classes {
  val StringClass = classOf[String]
  val FileClass = classOf[java.io.File]
  val BooleanClass = classOf[Boolean]
  val ShortClass = classOf[Short]
  val IntClass = classOf[Int]
  val LongClass = classOf[Long]
  val FloatClass = classOf[Float]
  val DoubleClass = classOf[Double]
  val OptionClass = classOf[Option[_]]
  val SeqClass = classOf[Seq[_]]
  val AttributedClass = classOf[sbt.Attributed[_]]
  val URIClass = classOf[java.net.URI]
  val ThrowableClass = classOf[Throwable]

  // TODO - Figure out how to handle attributed, and
  // other sbt special classes....

  abstract class SubClass(cls: Class[_]) {
    def unapply(ocls: Class[_]): Boolean =
      cls.isAssignableFrom(ocls)
  }

  object OptionSubClass extends SubClass(OptionClass)
  object SeqSubClass extends SubClass(SeqClass)
  object AttributedSubClass extends SubClass(AttributedClass)
  object ThrowableSubClass extends SubClass(ThrowableClass)
}

case class Format[A](tag: FastTypeTag[A], pickler: SPickler[A], unpickler: Unpickler[A])
object Format {
  implicit def genFormat[A](implicit tag: FastTypeTag[A], pickler: SPickler[A], unpickler: Unpickler[A]): Format[A] =
    Format(tag, pickler, unpickler)
}

/**
 * An interface representing a mechanism to register and
 *  retrieve serialization for specific types at runtime.
 *
 *  This suffers from all the same limitations as scala.Manifest for
 *  handling types.
 *
 *  A DynamicSerialization is immutable.
 */
sealed trait DynamicSerialization {
  /** Look up a serialization using its type manifest */
  def lookup[T](implicit mf: Manifest[T]): Option[Format[T]]
  /** Look up by runtime class (potentially broken if the class has type parameters) */
  def lookup[T](klass: Class[T]): Option[Format[T]]
  /** Add a serializer, returning the new modified DynamicSerialization. */
  def register[T](serializer: Format[T])(implicit mf: Manifest[T]): DynamicSerialization

  // Here we need to reflectively look up the serialization of things...
  final def buildValue[A](a: A)(implicit mf: Manifest[A]): BuildValue =
    lookup(mf) map { serializer =>
      implicit val tag = serializer.tag
      implicit val pickler = serializer.pickler
      BuildValue(SerializedValue(a), a.toString)
    } getOrElse BuildValue(SerializedValue.Null, a.toString)

  final def buildValueUsingRuntimeClass[A](a: A): BuildValue = {
    lookup(a.getClass) map { serializer =>
      implicit val tag: FastTypeTag[A] = serializer.tag.asInstanceOf[FastTypeTag[A]]
      implicit val pickler: SPickler[A] = serializer.pickler.asInstanceOf[SPickler[A]]
      BuildValue(SerializedValue(a), a.toString)
    } getOrElse (BuildValue(SerializedValue.Null, a.toString))
  }
}

object DynamicSerialization {
  val defaultSerializations: DynamicSerialization =
    NonTrivialSerializers.registerSerializers(ConcreteDynamicSerialization(Map.empty, Map.empty))
}

private final case class ConcreteDynamicSerialization(registered: Map[Manifest[_], Format[_]], byClass: Map[Class[_], Format[_]]) extends DynamicSerialization {
  override def register[T](serializer: Format[T])(implicit mf: Manifest[T]): DynamicSerialization =
    // Here we erase the original type when storing
    ConcreteDynamicSerialization(registered + (mf -> serializer), byClass + (mf.erasure -> serializer))

  override def lookup[T](implicit mf: Manifest[T]): Option[Format[T]] =
    // When looking up, given the interface, it's safe to return to
    // the original type.
    (registered get mf).asInstanceOf[Option[Format[T]]] orElse
      ConcreteDynamicSerialization.memoizedDefaultSerializer(mf)

  override def lookup[T](klass: Class[T]): Option[Format[T]] =
    (byClass get klass).asInstanceOf[Option[Format[T]]] orElse
      ConcreteDynamicSerialization.memoizedDefaultSerializer(klass)
}

private object ConcreteDynamicSerialization {
  import sbt.GenericSerializers._

  private val defaultSerializationMemosByManifest =
    scala.collection.concurrent.TrieMap[Manifest[_], Format[_]]()
  private val defaultSerializationMemosByClass =
    scala.collection.concurrent.TrieMap[Class[_], Format[_]]()

  def memoizedDefaultSerializer[T](mf: Manifest[T]): Option[Format[T]] =
    defaultSerializer(mf) match {
      case Some(s) =>
        defaultSerializationMemosByManifest.put(mf, s)
        Some(s)
      case None => None
    }

  def memoizedDefaultSerializer[T](klass: Class[T]): Option[Format[T]] =
    defaultSerializer[T](klass) match {
      case Some(s) =>
        defaultSerializationMemosByClass.put(klass, s)
        Some(s)
      case None => None
    }

  private def defaultSerializer[T](klass: Class[T]): Option[Format[T]] = {
    import scala.pickling._, sbt.pickling.json._
    defaultSerializationMemosByClass.get(klass).map(_.asInstanceOf[Format[T]]) orElse {
      (klass match {
        case Classes.StringClass => Some(implicitly[Format[String]])
        case Classes.FileClass => Some(implicitly[Format[java.io.File]])
        case Classes.BooleanClass => Some(implicitly[Format[Boolean]])
        case Classes.ShortClass => Some(implicitly[Format[Short]])
        case Classes.IntClass => Some(implicitly[Format[Int]])
        case Classes.LongClass => Some(implicitly[Format[Long]])
        case Classes.FloatClass => Some(implicitly[Format[Float]])
        case Classes.DoubleClass => Some(implicitly[Format[Double]])
        case Classes.URIClass => Some(implicitly[Format[java.net.URI]])
        // case Classes.ThrowableSubClass() => Some(Format[java.lang.Throwable](throwableReads, throwableWrites))
        case _ =>
          None
      }).asInstanceOf[Option[Format[T]]]
    }
  }
  private def defaultSerializer[T](mf: Manifest[T]): Option[Format[T]] = None
  // private def defaultSerializer[T](mf: Manifest[T]): Option[Format[T]] = defaultSerializationMemosByManifest.get(mf).map(_.asInstanceOf[Format[T]]) orElse
  //   defaultSerializer[T](mf.erasure.asInstanceOf[Class[T]]) orElse {
  //     (mf.erasure match {
  //       case Classes.OptionSubClass() =>
  //         for {
  //           child <- memoizedDefaultSerializer(mf.typeArguments(0))
  //         } yield {
  //           Format.optionWithNull(child)
  //         }
  //       // TODO - polymorphism?
  //       case Classes.SeqSubClass() =>
  //         // Now we need to find the first type arguments structure:
  //         import collection.generic.CanBuildFrom
  //         for {
  //           child <- memoizedDefaultSerializer(mf.typeArguments(0))
  //         } yield {
  //           val reads = Reads.traversableReads[Seq, Any](collection.breakOut, child.asInstanceOf[Reads[Any]])
  //           val writes = Writes.traversableWrites(child.asInstanceOf[Writes[Any]])
  //           Format(reads, writes)
  //         }
  //       case Classes.AttributedSubClass() =>
  //         for {
  //           child <- memoizedDefaultSerializer(mf.typeArguments(0))
  //         } yield Format(attributedReads(child), attributedWrites(child))
  //       case _ =>
  //         None
  //     }).asInstanceOf[Option[Format[T]]]
  //   }
}

private object NonTrivialSerializers {
  private sealed trait RegisteredFormat {
    type T
    def manifest: Manifest[T]
    def format: Format[T]
  }
  private def toRegisteredFormat[U](implicit f: Format[U], mf: Manifest[U]): RegisteredFormat = new RegisteredFormat {
    type T = U
    override val format = f
    override val manifest = mf
  }
  // this overload is used when we want to be able to lookup a subtype (since DynamicSerialization
  // is only smart enough to handle exact types)

  // private def toRegisteredFormat[U, W >: U](implicit reads: Reads[U], writes: Writes[W], mf: Manifest[U]): RegisteredFormat = new RegisteredFormat {
  //   type T = U
  //   override val format = Format[U](reads, writes)
  //   override val manifest = mf
  // }
  def registerSerializers(base: DynamicSerialization): DynamicSerialization = {
    import scala.pickling._, sbt.pickling.json._

    // TODO I think we only lookup by exact type, so registering an abstract type
    // here such as Type or xsbti.Problem is not useful. I think.
    // TODO we are registering some types here that aren't likely or conceivable
    // task results; we only need to register types T that appear in taskKey[T].
    // We don't have to register all the types of the fields in result types.
    val formats = Seq(
      toRegisteredFormat[ByteArray],
      toRegisteredFormat[ProjectReference],
      toRegisteredFormat[AttributeKey],
      toRegisteredFormat[SbtScope],
      toRegisteredFormat[ScopedKey],
      toRegisteredFormat[MinimalBuildStructure] // toRegisteredFormat[Analysis],
      // toRegisteredFormat[Stamps],
      // toRegisteredFormat[SourceInfo],
      // toRegisteredFormat[SourceInfos],
      // toRegisteredFormat[xsbti.Problem],
      // toRegisteredFormat[Problem, xsbti.Problem],
      // toRegisteredFormat[APIs],
      // toRegisteredFormat[ThePackage],
      // toRegisteredFormat[TypeParameter],
      // toRegisteredFormat[Path],
      // toRegisteredFormat[Modifiers],
      // toRegisteredFormat[AnnotationArgument],
      // toRegisteredFormat[Annotation],
      // toRegisteredFormat[Definition],
      // toRegisteredFormat[SourceAPI],
      // toRegisteredFormat[Source],
      // toRegisteredFormat[RelationsSource],
      // toRegisteredFormat[Relations],
      // toRegisteredFormat[OutputSetting],
      // toRegisteredFormat[Compilation],
      // toRegisteredFormat[Compilations],
      // toRegisteredFormat[Stamp],
      // toRegisteredFormat[Qualifier],
      // toRegisteredFormat[Access],
      // toRegisteredFormat[PathComponent],
      // toRegisteredFormat[Type],
      // toRegisteredFormat[SimpleType, Type],
      // toRegisteredFormat[CompileFailedException],
      // toRegisteredFormat[ModuleId]
      )
    formats.foldLeft(base) { (sofar, next) =>
      sofar.register(next.format)(next.manifest)
    }
  }
}
