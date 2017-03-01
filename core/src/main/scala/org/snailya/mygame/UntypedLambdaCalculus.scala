package org.snailya.mygame

/**
  * Created by molikto on 27/02/2017.
  */
object UntypedLambdaCalculus {

  abstract class Ast extends AstBaseWithPositionData
  case class Application(left: Ast, right: Ast) extends Ast
  abstract class Referential extends Ast
  case class Lambda1(left: Referential, right: Ast) extends Ast
  case class Reference(name: String) extends Referential
  case class Hole() extends Referential
  case class Definition(name: Referential, right: Ast) extends Ast
  case class Begin(defs: Ast*) extends Ast

  trait Frontend extends LanguageFrontendDynamics[Ast, Hole] {

    val ULC = UntypedLambdaCalculus

    val Term = SyntaxSort("term", null)

    val Binding = SyntaxSort("binding", null)

    val Application = SyntaxForm(
      ConstantCommand("("),
      Seq(Term, Term),
      ToLayout(
        2,
        seq => WSequence(WCommand(), seq(0), WConstant(" "), seq(1), WConstant(")"))
      ),
      (c, seq) => ULC.Application(seq(0), seq(1))
    )

    val Lambda = SyntaxForm(
      ConstantCommand("\\"),
      Seq(Binding, Term),
      ToLayout(2, seq => WSequence(WCommand("λ"), WConstant(" "), seq(0), WConstant(" ⇒ "), seq(1))),
      (c, seq) => ULC.Lambda1(seq(0).asInstanceOf[Referential], seq(1)) // ALERT: this is NOT working correctly, it will type check but will crash here
    )

    val Reference = SyntaxForm(
      AcceptanceCommand(s => true),
      Seq.empty,
      ToLayout(0, seq => WCommand()),
      (c, seq) => ULC.Reference(c)
    )

    val Definition = SyntaxForm(
      ConstantCommand("def"),
      Seq(Binding, Term),
      ToLayout(2, seq => WSequence(WCommand(), WConstant(" "), seq(0), WConstant(" = "), seq(1))),
      (c, seq) => ULC.Definition(seq(0).asInstanceOf[Referential], seq(1))
    )

    val Begin = SyntaxForm(
      ConstantCommand("{"),
      Seq(Term),
      ToLayout(-1, seq => WVertical(
        WCommand(),
        WSequence(WIndent, WVertical(seq: _*)),
        WConstant("}")
      )),
      (c, seq) => ULC.Begin(seq: _*)
    )

    Term.forms = Seq(Application, Lambda, Definition, Begin, Reference)

    Binding.forms = Seq(Reference)

    override val Lang = Language(Seq(Term, Binding), Term.forms)

    override def compile(l: Ast) = Left("")

    override def newHole() = ULC.Hole()
  }
}
