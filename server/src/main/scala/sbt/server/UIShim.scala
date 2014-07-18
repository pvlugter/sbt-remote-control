package sbt
package server

import play.api.libs.json.Writes
import sbt.protocol.TaskEvent
import sbt.protocol.DynamicSerialization

private[server] class ServerUIContext(state: ServerState, taskIdFinder: TaskIdFinder, eventSink: JsonSink[TaskEvent]) extends AbstractUIContext {

  private def withClient[A](state: ServerState)(f: (ExecutionId, LiveClient) => A): Option[A] = {
    state.lastCommand match {
      // work.allRequesters.head picks a client at random to do the confirmation protocol.
      case Some(LastCommand(work)) => Some(f(work.id, work.allRequesters.head))
      case _ => None
    }
  }
  // TODO - this is probably bad
  private def waitForever[A](f: concurrent.Future[A]): A =
    concurrent.Await.result(f, concurrent.duration.Duration.Inf)

  // TODO - Figure out how to block on input from server
  def readLine(prompt: String, mask: Boolean): Option[String] =
    // TODO - Timeouts error handling....
    withClient(state) { (executionId, client) =>
      waitForever(client.readLine(executionId, prompt, mask))
    }.getOrElse(throw new java.io.IOException("No clients listening to readLine request."))
  def confirm(msg: String): Boolean =
    withClient(state) { (executionId, client) =>
      System.out.println(s"Asking client($client) to confirm on request: $executionId, $msg")
      waitForever(client.confirm(executionId, msg))
      // TODO - Maybe we just always return some default value here.
    }.getOrElse(throw new java.io.IOException("No clients listening to confirm request."))

  def sendEvent[T: Writes](event: T): Unit =
    eventSink.send(TaskEvent(taskId, event))

  private def taskId: Long = {
    // TODO currently this depends on thread locals; we need to
    // set things up similar to how streams work now where we make
    // a per-task UIContext which knows that task's ID. This may
    // involve changes to the sbt core.
    taskIdFinder.bestGuessTaskId(taskIfKnown = None)
  }
}

object UIShims {

  private def uiContextSetting(taskIdFinder: TaskIdFinder, eventSink: JsonSink[TaskEvent]): Setting[_] =
    UIContext.uiContext in Global := {
      val state = sbt.Keys.state.value
      // TODO - Maybe we don't need to register these everytime, but only
      // `onLoad` of a build?
      val formats = UIContext.registeredFormats.value
      formats foreach { x =>
        DynamicSerialization.register(x.format)(x.manifest)
      }
      new ServerUIContext(ServerState.extract(state), taskIdFinder, eventSink)
    }
  def makeShims(state: State, taskIdFinder: TaskIdFinder, eventSink: JsonSink[TaskEvent]): Seq[Setting[_]] =
    Seq(
      UIContext.registeredFormats in Global <<= (UIContext.registeredFormats in Global) ?? Nil,
      uiContextSetting(taskIdFinder, eventSink))
}
