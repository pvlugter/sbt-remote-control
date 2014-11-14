package sbt.pickling

import java.io.File
import java.net.URI
import scala.pickling.internal.AppliedType

trait CanToString[A] {
  def toString(a: A): String
  def fromString(s: String): A
}

object CanToString {
  implicit val fileCanToString: CanToString[File] = CanToString(
    _.toURI.toASCIIString, {
      s: String => new File(new URI(s))
    })
  implicit val uriCanToString: CanToString[URI] = CanToString(
    _.toASCIIString, {
      s: String => new URI(s)
    })
  implicit val appliedTypeCanToString: CanToString[AppliedType] = CanToString(
    _.toString, {
      s: String => AppliedType.parse(s)._1
    })
  def apply[A](ts: A => String, fs: String => A): CanToString[A] = new CanToString[A] {
    def toString(a: A): String = ts(a)
    def fromString(s: String): A = fs(s)
  }
}
