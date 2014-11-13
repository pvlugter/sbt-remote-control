package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import scala.pickling._, sbt.pickling.json._
import SpecsUtil._
import JUnitUtil._
import sbt.protocol
import protocol.SerializedValue

class SerializedValuePicklerTest {
  @Test
  def testRoundtrip: Unit = {
    roundTrip(protocol.SerializedValue(1): protocol.SerializedValue)
  }
}
