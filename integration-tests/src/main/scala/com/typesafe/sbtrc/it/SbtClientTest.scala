package com.typesafe.sbtrc
package it

import sbt.client._
import sbt.client.impl.Testing
import java.io.File
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import concurrent.Promise
import scala.annotation.tailrec

trait SbtClientTest extends IntegrationTest {
  // TODO - load from config - this timeout is long because travis is slow
  def defaultTimeout = concurrent.duration.Duration(180, java.util.concurrent.TimeUnit.SECONDS)

  /** helper to add error messages when waiting for results and timeouts occur. */
  def waitWithError[T](awaitable: scala.concurrent.Awaitable[T], msg: String): T = {
    try scala.concurrent.Await.result(awaitable, defaultTimeout)
    catch {
      case e: java.util.concurrent.TimeoutException => sys.error(msg)
    }
  }

  sealed trait QueueResults[+T, +U]
  case class Continue[+T, +U](next: U) extends QueueResults[T, U]
  case class Done[+T, +U](result: T) extends QueueResults[T, U]

  /** helper to add error messages when waiting for results and timeouts occur. */
  def waitOnQueue[I, T, U](queue: LinkedBlockingQueue[I], initial: U, msg: String, timeout: concurrent.duration.Duration = defaultTimeout)(body: (I, U) => QueueResults[T, U]): T = {
    def next(): I = (try queue.poll(timeout.length, timeout.unit) catch {
      case e: java.lang.InterruptedException => sys.error(msg)
    }) match {
      case null => sys.error(msg)
      case x => x
    }

    @tailrec
    def step(state: U, value: I): T = {
      body(value, state) match {
        case Continue(u) => step(u, next())
        case Done(t) => t
      }
    }

    step(initial, next())
  }

  /**
   * Allows running tests against sbt.  Will block until sbt server is loaded against
   * a given directory...
   *
   */
  final def withSbt(projectDirectory: java.io.File)(f: SbtClient => Unit): Unit = {
    val connector = Testing.connector(configuration, projectDirectory)

    val numConnects = new java.util.concurrent.atomic.AtomicInteger(0)
    val clientCloseLatch = new CountDownLatch(1)
    // TODO - Executor for this thread....
    object runOneThingExecutor extends concurrent.ExecutionContext {
      private var task = Promise[Runnable]
      def execute(runnable: Runnable): Unit = synchronized {
        // we typically get two runnables; the first one is "newHandler"
        // below and the second is "errorHandler" when the connector is
        // closed. We just drop "errorHandler" on the floor.
        if (task.isCompleted)
          System.out.println(s"Not executing runnable because we only run one thing: ${runnable}")
        else
          task.success(runnable)
      }
      // TODO - Test failure...
      def reportFailure(t: Throwable): Unit = task.failure(t)

      def runWhenReady(): Unit = {
        val result = concurrent.Await.result(task.future, defaultTimeout)
        result.run
      }
    }

    def dumpLogFile(logfile: File): Unit = {
      if (logfile.exists) {
        // This is disabled by default because it tends to make the actual error scroll way off the screen
        // and is irrelevant much of the time.
        System.out.println(s"log file is ${logfile}, you can uncomment some code in SbtClientTest.scala to dump this if you need to")
        //System.out.println(s"log file ${logfile}:")
        //scala.io.Source.fromFile(logfile).getLines().foreach { System.out.println(_) }
        //System.out.println(s"end of ${logfile}")
      } else {
        System.err.println(s"No log file ${logfile} found")
      }
    }

    val newHandler: SbtChannel => Unit = { channel =>
      numConnects.getAndIncrement

      val client = SbtClient(channel)

      val logfile = new File(projectDirectory, s".sbtserver/connections/${client.configName}-${client.uuid}.log").getAbsoluteFile
      if (!logfile.exists)
        System.err.println(s"Warning: no log file for client ${logfile}")

      // TODO - better error reporting than everything.
      (client handleEvents {
        msg =>
          System.out.println(msg)
          msg match {
            case e: sbt.protocol.ClosedEvent =>
              // Check that adding a new handler gets us ANOTHER ClosedEvent immediately.
              // Woo-hoo rube goldberg. The point here is that if you add a handler
              // after close, you should still get ClosedEvent.
              (client handleEvents {
                case e: sbt.protocol.ClosedEvent =>
                  clientCloseLatch.countDown()
                case _ =>
              })(concurrent.ExecutionContext.global)
            case _ =>
          }
      })(concurrent.ExecutionContext.global)
      try f(client)
      catch {
        case e: Throwable =>
          // unfortunately, if the server restarts it deletes all logs so this will be gone
          dumpLogFile(logfile)
          throw e
      } finally {
        if (!client.isClosed) {
          client.requestSelfDestruct()
          while (!client.isClosed) {
            Thread.sleep(200)
            System.out.println("Waiting for client to close")
          }
        }
      }
    }
    // due to the run one thing executor, this is only called
    // if newHandler is NEVER called.
    val errorHandler: (Boolean, String) => Unit = { (reconnecting, error) =>
      // don't retry forever just close. But print those errors.
      System.out.println(s"sbt connection closed, reconnecting=${reconnecting} error=${error}")
      if (reconnecting)
        connector.close()
    }

    // TODO - We may want to connect to the sbt server and dump debugging information/logs.
    val subscription = (connector.openChannel(newHandler, errorHandler))(runOneThingExecutor)
    // Block current thread until we can run the test.
    try {
      runOneThingExecutor.runWhenReady()
      if (numConnects.get <= 0)
        throw new Exception("Never connected to sbt server!")
    } catch {
      case e: Throwable =>
        System.err.println(s"Test failed: ${e.getClass.getName}: ${e.getMessage}")
        // unfortunately, if the server restarts it deletes all logs so this will
        // probably be the wrong log file (for new instead of old server)
        dumpLogFile(new File(projectDirectory, ".sbtserver/master.log").getAbsoluteFile)
        throw e
    } finally connector.close()

    clientCloseLatch.await(15, TimeUnit.SECONDS)
  }

  lazy val utils = new TestUtil(new java.io.File("scratch"))
}
