package org.snailya.mygame

/**
  * Created by molikto on 27/02/2017.
  */
object UntypedLambdaCalculus {

  abstract class Ast extends AstBase
  case class Hole() extends Ast

  trait Frontend extends LanguageFrontendDynamics[Ast, Hole] {
    override val Lang = _

    override def compile(l: Ast) = ???

    override def NewHole() = ???
  }
}
