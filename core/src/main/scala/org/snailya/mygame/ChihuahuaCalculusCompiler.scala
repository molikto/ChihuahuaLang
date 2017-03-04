package org.snailya.mygame

/**
  * Created by molikto on 03/03/2017.
  */
trait ChihuahuaCalculusCompiler extends ChihuahuaCalculusAst {


  def compile(T: Term): Either[String, Seq[Error]] = Left("")
}
