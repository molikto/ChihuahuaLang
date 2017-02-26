
package org.snailya.mygame

import akka.actor.ActorSystem
import akka.event.Logging.LogLevel
import akka.event.{Logging, LoggingAdapter}
import akka.http.javadsl.server.Directives
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives._
import akka.stream.{ActorMaterializer, Materializer}
import de.heikoseeberger.akkahttpupickle.UpickleSupport

import scala.concurrent.ExecutionContext
import scala.io.StdIn
import scala.util._

object Server {

  val ServerName = "001"
  val storage = new DemoStorage(ServerName)

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  def main(args: Array[String]) {


    // TODO use codegen for this
    import UpickleSupport._
    import upickle.default._

    def completeWithError(e: Exception): Route = {
      e.printStackTrace()
      complete(StatusCodes.BadRequest)
    }


    // TODO maintenance alert
    // TODO client version checking
    // TODO authentication
    // TODO should we do null validation?
    // TODO when parsing, throw exception on NULL
    val route = path(PATH_GAME) {
      post {
        // sometimes we use assertions to ensure game logic.
        // this is somehow acceptable
        // all "exceptions" that should not happens on client side should be treated
        // with minimal effort
        // because normally a good client should not do anything wrong

        // when to use exception and when not?
        // considering resend valid requuests as non-exception, and return without changing state
        // non-valid content is considered exception
        headerValueByName(HEADER_DEBUG_UID) { uid =>


          entity(as[Request]) { foo =>

            val time = System.currentTimeMillis()
            completeWithError(new Exception())
          }
        }
      }
    }



    val bindingFuture = Http().bindAndHandle(requestMethodAndResponseStatusAsInfo(Logging.InfoLevel, route), "0.0.0.0", 8080)

    delog(s"Server online...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate())
  }


  def requestMethodAndResponseStatusAsInfo(level: LogLevel, route: Route)
    (implicit m: Materializer, ex: ExecutionContext) = {

    def akkaResponseTimeLoggingFunction(loggingAdapter: LoggingAdapter, requestTimestamp: Long)(req: HttpRequest)(res: Any): Unit = {

      val entry = res match {
        case Complete(resp) =>
          val responseTimestamp: Long = System.currentTimeMillis()
          val elapsedTime: Long = responseTimestamp - requestTimestamp
          val loggingString = "Logged Request:" + req.method + ":" + req.uri + ":" + resp.status + ":" + elapsedTime
          LogEntry(loggingString, level)
        case anythingElse =>
          LogEntry(s"$anythingElse", level)
      }
      entry.logTo(loggingAdapter)
    }
    DebuggingDirectives.logRequestResult(LoggingMagnet(log => {
      val requestTimestamp = System.currentTimeMillis()
      akkaResponseTimeLoggingFunction(log, requestTimestamp)
    }))(route)

  }


}
