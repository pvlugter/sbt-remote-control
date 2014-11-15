package sbt.protocol

import play.api.libs.json._
import scala.util.{ Try, Success, Failure }
import scala.pickling.{ FastTypeTag, SPickler, Unpickler }

/**
 *  Represents a serialized value with a stringValue fallback.
 */
final case class BuildValue(serialized: SerializedValue, stringValue: String) {
  def value[A: FastTypeTag: Unpickler]: Try[A] = serialized.parse[A]
  override def equals(o: Any): Boolean =
    o match {
      case x: BuildValue => x.serialized == serialized
      case _ => false
    }
  override def hashCode: Int = serialized.hashCode
}

object BuildValue {
  def apply[A: FastTypeTag: SPickler](value: A): BuildValue = {
    BuildValue(serialized = SerializedValue(value), stringValue = value.toString)
  }
}

/**
 * Represents the outcome of a task. The outcome can be a value or an exception.
 */
sealed trait TaskResult {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean
  final def result[A: FastTypeTag: Unpickler]: Try[A] = {
    implicit val tag = implicitly[FastTypeTag[Throwable]]
    implicit val unpickler: Unpickler[Throwable] = ???
    resultWithCustomThrowable[A, Throwable]
  }
  def resultWithCustomThrowable[A: FastTypeTag: Unpickler, B <: Throwable: FastTypeTag: Unpickler]: Try[A]
}
/** This represents that the task was run successfully. */
final case class TaskSuccess(value: BuildValue) extends TaskResult {
  override def isSuccess = true
  override def resultWithCustomThrowable[A: FastTypeTag: Unpickler, B <: Throwable: FastTypeTag: Unpickler]: Try[A] =
    value.value[A]
}

final case class TaskFailure(cause: BuildValue) extends TaskResult {
  override def isSuccess = false
  override def resultWithCustomThrowable[A: FastTypeTag: Unpickler, B <: Throwable: FastTypeTag: Unpickler]: Try[A] = {
    val t = cause.serialized.parse[B].getOrElse(new Exception(cause.stringValue))
    Failure(t)
  }
}

object TaskResult {
  // implicit val reads: Reads[TaskResult] =
  //   new Reads[TaskResult] {
  //     override def reads(m: JsValue): JsResult[TaskResult] = {
  //       (m \ "success") match {
  //         case JsBoolean(true) =>
  //           Json.fromJson[BuildValue](m).map(TaskSuccess.apply)
  //         case JsBoolean(false) =>
  //           Json.fromJson[BuildValue](m \ "cause").map(TaskFailure.apply)
  //         case _ =>
  //           JsError("Unable to deserialize task result.")
  //       }
  //     }
  //   }
  // implicit val writes: Writes[TaskResult] =
  //   new Writes[TaskResult] {
  //     override def writes(t: TaskResult): JsValue =
  //       t match {
  //         case TaskFailure(cause) =>
  //           JsObject(Seq("success" -> JsBoolean(false), "cause" -> Json.toJson(cause)))
  //         case TaskSuccess(value) =>
  //           val base = Json.obj("success" -> true)
  //           val valueJson = Json.toJson(value) match {
  //             case o: JsObject => o
  //             case other => throw new RuntimeException("serialized a BuildValue to non-JsObject")
  //           }
  //           base ++ valueJson
  //       }
  //   }
}
