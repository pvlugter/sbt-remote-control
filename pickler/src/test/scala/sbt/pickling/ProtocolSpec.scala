package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import scala.pickling._, sbt.pickling.json._
import SpecsUtil._
import JUnitUtil._
import sbt.protocol
import sbt.protocol.Message
import scala.pickling.internal.AppliedType

class ProtocolTest {
  val key = protocol.AttributeKey("name", AppliedType.parse("java.lang.String")._1)
  val build = new java.net.URI("file:///test/project")
  val projectRef = protocol.ProjectReference(build, "test")
  val scope = protocol.SbtScope(project = Some(projectRef))
  val scopedKey = protocol.ScopedKey(key, scope)
  val buildStructure = protocol.MinimalBuildStructure(
    builds = Vector(build),
    projects = Vector(protocol.MinimalProjectStructure(scope.project.get, Vector("com.foo.Plugin"))))

  @Test
  def testMessages: Unit = {
    // messages
    roundTripMessage(protocol.KillServerRequest())
    roundTripMessage(protocol.ReadLineRequest(42, "HI", true))
    roundTripMessage(protocol.ReadLineResponse(Some("line")))
    roundTripMessage(protocol.ConfirmRequest(43, "msg"))
    roundTripMessage(protocol.ReadLineResponse(Some("line")))
    roundTripMessage(protocol.ReceivedResponse())
    roundTripMessage(protocol.CommandCompletionsRequest("He", 2))
    roundTripMessage(protocol.CommandCompletionsResponse(Vector(protocol.Completion("llo", "Hello", true))))
    roundTripMessage(protocol.ListenToEvents())
    roundTripMessage(protocol.ListenToBuildChange())
    roundTripMessage(protocol.ExecutionRequest("test command string"))
    roundTripMessage(protocol.ListenToValue(scopedKey))
    roundTripMessage(protocol.CancelExecutionRequest(1))
    roundTripMessage(protocol.ErrorResponse("ZOMG"))
    roundTripMessage(protocol.CancelExecutionResponse(false))
  }

  @Test
  def testEvents: Unit = {
    // events
    roundTripMessage(protocol.TaskStarted(47, 1, Some(scopedKey)))
    roundTripMessage(protocol.TaskFinished(48, 1, Some(scopedKey), true))
    roundTripMessage(protocol.TaskStarted(47, 1, None))
    roundTripMessage(protocol.TaskFinished(48, 1, None, true))
    roundTripMessage(protocol.BuildStructureChanged(buildStructure))

    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue("HI"))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f))))
  }

  @Test
  def testLogEvents: Unit = {
    roundTripMessage(protocol.TaskLogEvent(1, protocol.LogStdOut("Hello, world")))
    roundTripMessage(protocol.CoreLogEvent(protocol.LogStdOut("Hello, world")))
    roundTripMessage(protocol.TaskLogEvent(2, protocol.LogMessage(protocol.LogMessage.INFO, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(3, protocol.LogMessage(protocol.LogMessage.ERROR, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(4, protocol.LogMessage(protocol.LogMessage.WARN, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(5, protocol.LogMessage(protocol.LogMessage.DEBUG, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(6, protocol.LogStdErr("TEST")))
    roundTripMessage(protocol.TaskLogEvent(7, protocol.LogStdOut("TEST2")))    
  }

  @Test
  def testTaskEvents: Unit = {
    roundTripMessage(protocol.TaskEvent(8, PlayStartedEvent(port = 10)))
    roundTripMessage(protocol.BackgroundJobStarted(9, protocol.BackgroundJobInfo(id = 67, humanReadableName = "foojob", spawningTask = scopedKey)))
    roundTripMessage(protocol.BackgroundJobFinished(9, 67))
    roundTripMessage(protocol.BackgroundJobEvent(67, PlayStartedEvent(port = 10)))
  }
}
