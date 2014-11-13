package sbt.protocol

import java.io.File
import java.net.URL
import org.json4s._

private[sbt] object JsonUtil {
  def parseJson(s: String): JValue = {
    jawn.support.json4s.Parser.parseFromString(s).get
  }
}
