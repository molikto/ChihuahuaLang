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
}

trait UntypedArithmeticFrontend extends LanguageFrontendDynamics[UntypedArithmeticCompiler.Ast] {
  val Term = SyntaxSort("term", null)
  val True = SyntaxForm1("true")
  val False = SyntaxForm1("false")
  val IfThenElse = SyntaxForm(ConstantCommand("if"), Seq(Term, Term, Term),
    ToLayout(3, (seq) => {
      WVertical(
        WSequence(WCommand(), WConstant(" "), seq(0)),
        WSequence(WIndent, seq(1)),
        WConstant("else"),
        WSequence(WIndent, seq(2))
      )
    })
  )
  val Succ = SyntaxForm2("succ", Term)
  val Pred = SyntaxForm2("pred", Term)
  val IsZero = SyntaxForm2("iszero", Term)
  def isPositiveNumber(s: String) = {
    s.length > 0 && s.forall(a => "0123456789".contains(a))
  }
  def isNumber(s: String): Boolean = {
    if (s.startsWith("-")) isPositiveNumber(s.substring(1))
    else isPositiveNumber(s)
  }
  val Number = SyntaxForm(AcceptanceCommand(isNumber), Seq(), layouts.Inline1)
  Term.forms = Seq(True, False, IfThenElse, Number, Succ, Pred, IsZero)
  override val Lang = Language(Seq(Term), Term.forms)
}

class ScreenMain extends ScreenBase with UntypedArithmeticFrontend {

  override def render(delta: Float) = super.renderFrontend(delta)

}
