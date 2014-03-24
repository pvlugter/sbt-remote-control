package com.typesafe.sbtrc
package client

import sbt.client._
import java.io.File
import scala.concurrent.ExecutionContext
import sbt.protocol._

class SimpleConnector(configName: String, humanReadableName: String, directory: File, locator: SbtServerLocator) extends SbtConnector {
  private var currentClient: Option[SbtClient] = None
  private var listeners: List[Listener] = Nil
  private var reconnecting: Boolean = true

  // Helper to give listeners an identity and execute methods on the given
  // context.
  class Listener(handler: SbtClient => Unit, ctx: ExecutionContext) {
    private val id = java.util.UUID.randomUUID.toString
    def emit(client: SbtClient): Unit =
      ctx.prepare.execute(new Runnable() {
        override def run(): Unit = {
          handler(client)
        }
      })
    override def hashCode = id.hashCode
    override def equals(o: Any) =
      o match {
        case other: Listener => id == other.id
        case _ => false
      }
  }

  def onConnect(handler: SbtClient => Unit)(implicit ex: ExecutionContext): Subscription = {
    val listener = new Listener(handler, ex)
    SimpleConnector.this.synchronized(listeners = listener :: listeners)
    object sub extends Subscription {
      def cancel(): Unit = {
        SimpleConnector.this.synchronized(listeners = listeners.filterNot(_ == listener))
      }
    }
    handleNewSubscriber(listener)
    sub
  }
  private[this] def handleNewSubscriber(listener: Listener): Unit = synchronized {
    currentClient match {
      case Some(client) => listener emit client
      case None => connectToSbt()
    }
  }

  private[this] def connectToSbt(): Unit = synchronized {
    val uri = locator.locate(directory)
    // TODO - We need  way to be notified of failures so we can reconnect here...
    val socket = new java.net.Socket(uri.getHost, uri.getPort)
    val rawClient = new ipc.Client(socket)
    val uuid = java.util.UUID.randomUUID()
    val registerSerial = rawClient.sendJson(RegisterClientRequest(uuid.toString, configName, humanReadableName))
    Envelope(rawClient.receive()) match {
      case Envelope(_, replyTo, ErrorResponse(message)) if replyTo == registerSerial =>
        throw new RuntimeException(s"Failed to register client with sbt: ${message}")
      case Envelope(_, replyTo, reply: ReceivedResponse) if replyTo == registerSerial =>
      case wtf => {
        rawClient.close()
        throw new RuntimeException(s"unexpected initial message from server was not a register client reply: ${wtf}")
      }
    }
    val sbtClient = new SimpleSbtClient(uuid, configName, humanReadableName, rawClient, () => onClose())
    currentClient = Some(sbtClient)
    // notify all existing folks of the new client.
    def loop(remaining: List[Listener]): Unit =
      remaining match {
        case Nil => ()
        case head :: tail =>
          head emit sbtClient
          loop(tail)
      }
    loop(listeners)
  }
  // A callback from the server handling thread.
  private def onClose(): Unit = synchronized {
    // TODO - This should only happen if we think the connection to the
    // sbt server is closed.
    if (reconnecting) connectToSbt()
  }

  def close(): Unit = {
    synchronized { reconnecting = false }
    currentClient foreach (_.close())
    synchronized {
      currentClient = None
      // TODO - Is this the right way to go?
      listeners = Nil
    }
  }
}