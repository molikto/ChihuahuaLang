package org.snailya.mygame

/**
  * Created by molikto on 21/02/2017.
  */

import MyGame._
import MyGame.game._
import com.badlogic.gdx.{Gdx, InputProcessor}
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.GlyphLayout
import com.badlogic.gdx.scenes.scene2d.ui.TextField
import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldStyle

import scala.collection.mutable

object UntypedArithmeticCompiler {
  abstract class Ast
  case object True extends Ast
  case object False extends Ast
  case class If(pred: Ast, left: Ast, right: Ast) extends Ast
  case class Pred(a: Ast) extends Ast
  case class Succ(a: Ast) extends Ast
  case class IsZero(a: Ast) extends Ast
  case class Number(s: BigInt) extends Ast
  case class Hole() extends Ast
}

trait UntypedArithmeticFrontend extends LanguageFrontendDynamics[UntypedArithmeticCompiler.Ast, UntypedArithmeticCompiler.Hole] {
  val uac = UntypedArithmeticCompiler

  def NewHole() = uac.Hole()

  val Term = SyntaxSort("term", null)
  val True = SyntaxFormConstant("true", uac.True)
  val False = SyntaxFormConstant("false", uac.False)
  val IfThenElse = SyntaxForm(ConstantCommand("if"), Seq(Term, Term, Term),
    ToLayout(3, (seq) => {
      WVertical(
        WSequence(WCommand(), WConstant(" "), seq(0)),
        WSequence(WIndent, seq(1)),
        WConstant("else"),
        WSequence(WIndent, seq(2))
      )
    }),
    (name, childs) => uac.If(childs(0), childs(1), childs(2))
  )
  val Succ = SyntaxFormApplicative1("succ", Term, (_, a) => uac.Succ(a(0)))
  val Pred = SyntaxFormApplicative1("pred", Term, (_, a) => uac.Pred(a(0)))
  val IsZero = SyntaxFormApplicative1("iszero", Term, (_, a) => uac.IsZero(a(0)))
  def isPositiveNumber(s: String) = {
    s.length > 0 && s.forall(a => "0123456789".contains(a))
  }
  def isNumber(s: String): Boolean = {
    if (s.startsWith("-")) isPositiveNumber(s.substring(1))
    else isPositiveNumber(s)
  }
  val Number = SyntaxForm(AcceptanceCommand(isNumber), Seq(), layouts.Inline1, (c, _) => uac.Number(BigInt(c)))
  Term.forms = Seq(True, False, IfThenElse, Number, Succ, Pred, IsZero)
  override val Lang = Language(Seq(Term), Term.forms)
}

class ScreenMain extends ScreenBase with UntypedArithmeticFrontend {
  override def render(delta: Float) = super.renderFrontend(delta)
}
