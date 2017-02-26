package org.snailya.mygame

import scala.collection.mutable

/**
  * Created by molikto on 04/02/2017.
  */
trait UtilsNaming {

  def generateUniqueRandomNames(existing: Set[String], newLength: Int): Seq[String] = {
    val cache = mutable.ArrayBuffer.empty[String]
    while (cache.size < newLength) {
      val cand = random.nextInt(1000).toString
      if (!existing.contains(cand)) {
        cache.append(cand)
      }
    }
    cache
  }


  def pickNewNameFromNamingPool(existing: Set[String], candidate: Set[String]): String = (candidate -- existing).iterator.next()

}
