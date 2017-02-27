package org.snailya.mygame

/**
  * Created by molikto on 27/02/2017.
  */
object UntypedLambdaCalculus {

  abstract class Ast extends AstBase
  case class Application(left: Ast, right: Ast) extends Ast
  case class Lambda(left: Ast, right: Ast) extends Ast
  case class Reference(name: String) extends Ast
  case class Hole() extends Ast

  trait Frontend extends LanguageFrontendDynamics[Ast, Hole] {

    val ULC = UntypedLambdaCalculus

    val Term = SyntaxSort("term", null)

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
      Seq(Term, Term),
      ToLayout(2, seq => WSequence(WCommand(), seq(0), WConstant(" => "), seq(1))),
      (c, seq) => ULC.Lambda(seq(0), seq(1))
    )

    val Reference = SyntaxForm(
      AcceptanceCommand(s => true),
      Seq.empty,
      ToLayout(0, seq => WCommand()),
      (c, seq) => ULC.Reference(c)
    )

    Term.forms = Seq(Application, Lambda, Reference)

    override val Lang = Language(Seq(Term), Term.forms)

    override def compile(l: Ast) = Left("")

    override def NewHole() = ULC.Hole()
  }
}