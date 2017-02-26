package org.snailya.mygame

import com.badlogic.gdx.Application.ApplicationType
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Net._
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.net._

import scala.util.{Failure, Success, Try}

/**
  * Created by molikto on 07/01/2017.
  */
trait MyGameNetworking {

  /**
    *
    */



  case class StatusCodeException(code: Int) extends Exception


  implicit var uid: String = System.currentTimeMillis() + ""

  def debugNewUid() = uid = System.currentTimeMillis() + ""

  //val LOCAL_URL = Routing("192.168.32.191:8080") //work
  //val LOCAL_URL = Routing("192.168.0.107:8080") // old home
  val LOCAL_URL = Routing("192.168.1.101:8080") // home


  val URL = if (Debug) if (Gdx.app.getType == ApplicationType.Desktop) Routing("127.0.0.1:8080") else LOCAL_URL else Routing("")


  def relativeTimeMillis = System.nanoTime() / 1000000
  def systemTimeMillis = System.currentTimeMillis()

  var lastRequestTime = relativeTimeMillis - 100000L
  var sendingRequest = false


  def send(r: Request, l: Try[Response] => Unit): Unit  = {
    delog("sending request " + r)
    import upickle.default._
    val requestBuilder = new HttpRequestBuilder()
    val httpRequest = requestBuilder.newRequest()
      .method(HttpMethods.POST)
      .url(URL.GAME)
      .header(HttpRequestHeader.ContentType, "application/json")
      .header(HEADER_DEBUG_UID, uid)
      .content(write(r))
      .build()
    // single communication now
    assert(!sendingRequest)
    sendingRequest = true
    lastRequestTime = relativeTimeMillis
    def post(r: Try[Response]) = {
      MyGame.postRun(() => {
        sendingRequest = false
        delog("posting request result " + r)
        l(r)
      })
    }
    val finalListener = new HttpResponseListener {

      override def handleHttpResponse(httpResponse: HttpResponse): Unit = {
        val code = httpResponse.getStatus.getStatusCode
        if (code == 200) {
          try {
            post(Success(read[Response](httpResponse.getResultAsString)))
          } catch {
            case e: Throwable => post(Failure(e))
          }
        } else {
          post(Failure(StatusCodeException(code)))
        }
      }


      override def cancelled(): Unit = {
        post(Failure(new Exception("cancelled")))
      }

      override def failed(t: Throwable): Unit = {
        post(Failure(t))
      }
    }

    val listener = new HttpResponseListener {

      var retry = 0

      override def handleHttpResponse(httpResponse: HttpResponse): Unit = finalListener.handleHttpResponse(httpResponse)

      override def cancelled(): Unit = finalListener.cancelled()

      override def failed(t: Throwable): Unit = {
        if (retry < 5) {
          retry += 1
          Gdx.net.sendHttpRequest(httpRequest, this)
        } else {
          finalListener.failed(t)
        }
      }
    }
    Gdx.net.sendHttpRequest(httpRequest, listener)
  }



}
