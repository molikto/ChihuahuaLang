package org.snailya.mygame

/**
  * Created by molikto on 31/01/2017.
  */
trait UtilsHttp {

  val PATH_GAME = "game"

  final case class Routing(base: String) {
    val PROTOCOL_DASH = "http://" + base + "/"
    val GAME = PROTOCOL_DASH + PATH_GAME
  }

  val HEADER_DEBUG_UID = "Debug-Uid"
}
