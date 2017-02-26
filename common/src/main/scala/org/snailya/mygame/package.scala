package org.snailya

import java.util.concurrent.atomic.AtomicInteger


import scala.collection.mutable
import scala.util.Random

/**
  * data objects shared in client & server, package object so we don't need to import
  */
package object mygame extends UtilsCommon with UtilsHttp {

  case class Request()
  case class Response()
}
