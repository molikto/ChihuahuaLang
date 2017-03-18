package org.snailya.mygame

import java.util.concurrent.atomic.AtomicInteger

import scala.util.Random

/**
  * common debug things shared in server and client
  */
trait UtilsCommon {

  def abs(a: Int) = if (a > 0) a else -a

  def dis(x: Float, y: Float, x2: Float, y2: Float): Float = {
    val dx = x - x2
    val dy = y - y2
    Math.sqrt(dx * dx + dy * dy).toFloat
  }

  def swapCoordinate[T](seq: Seq[Seq[T]]): Seq[Seq[T]] = {
    for (x <- 0 until seq.head.length) yield for (y <- 0 until seq.length) yield seq(y)(x)
  }

  def delog(s: => Object) = {
    if (Debug) {
      try {
        println(s)
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }

  def delogThread() = delog(Thread.currentThread().getName)

  val random = new Random(System.currentTimeMillis())

  def randomItem[T](i: Seq[T]) = i(random.nextInt(i.size))

  def nextSign() = if (random.nextBoolean()) 1 else -1

  // color conversion
  def cc(i: Int) = i * 1F / 0xFF


  var Debug = true

  def static_assert(a: Boolean) = {
    if (Debug) assert(a)
  }

}
