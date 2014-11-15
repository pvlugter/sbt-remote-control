package sbt.protocol

import scala.pickling.{ FastTypeTag, SPickler, Unpickler }
import org.json4s.{ JValue, JString, JNull }
import scala.util.Try

/**
 * We serialize to and from this opaque type. The idea is to
 * hide exactly what we can serialize from/to and hide
 * which library we use to do it.
 */
sealed trait SerializedValue {
  def parse[A: FastTypeTag: Unpickler]: Try[A]
}

object SerializedValue {
  def apply[A: FastTypeTag: SPickler](a: A): SerializedValue =
    JsonValue[A](a)
  def Null: SerializedValue = JsonValue(JNull)
}

private[sbt] sealed trait SbtPrivateSerializedValue extends SerializedValue {
  def toJson: JsonValue
}

/** A value we have serialized as JSON */
private[sbt] final case class JsonValue(json: JValue) extends SbtPrivateSerializedValue {
  override def parse[A: FastTypeTag: Unpickler]: Try[A] = {
    import sbt.pickling.json._
    import org.json4s.native.JsonMethods._
    Try(JSONPickle(compact(render(json))).unpickle[A])
  }
  override def toJson = this
}

private[sbt] object JsonValue {
  def apply[A: FastTypeTag: SPickler](a: A): JsonValue = {
    import scala.pickling._, sbt.pickling.json._
    new JsonValue(JsonUtil.parseJson(a.pickle.value))
  }
}

// With LazyValue I get: 
// [error] sbt-remote-control/pickler/src/test/scala/sbt/pickling/SpecsUtil.scala:8: recursive value unpickler needs type
// [error]     s.unpickle[Message]
// [error]               ^
/**
 * A value we have the info available to serialize, but we haven't
 *  picked a format yet. Allows us to defer format selection.
 */
// private[sbt] final case class LazyValue[V: FastTypeTag: SPickler](value: V) extends SbtPrivateSerializedValue {
//   // this could theoretically avoid the round-trip in some cases, but
//   // pretty annoying to figure out what those cases are so forget
//   // it. Not expecting to actually call this really anyway because
//   // we use LazyValue on the "send" side.
//   override def parse[A: FastTypeTag: Unpickler]: Try[A] =
//     toJson.parse[A]

//   override def toJson: JsonValue = JsonValue(value)
// }
