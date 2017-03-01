package org.snailya.mygame

import org.snailya.mygame.UntypedLambdaCalculus.Lambda1

/**
  * Created by molikto on 01/03/2017.
  */
object ChihuahuaCalculus extends ChihuahuaCalculusAst {

  trait Frontend extends LanguageFrontendDynamics[Ast, Hole] {

    val CC = ChihuahuaCalculus

    val Term = SyntaxSort("term", null)

    val Binding = SyntaxSort("binding", null)

    val Type = SyntaxSort("type", null)

    val TypeBinding = SyntaxSort("type binding", null)


//    val Application = SyntaxForm(
//      ConstantCommand("("),
//      Seq(Term, Term),
//      ToLayout(
//        2,
//        seq => WSequence(WCommand(), seq(0), WConstant(" "), seq(1), WConstant(")"))
//      ),
//      (c, seq) => CC.Application(seq(0), seq(1))
//    )
//
//    val Lambda = SyntaxForm(
//      ConstantCommand("\\"),
//      Seq(Binding, Term),
//      ToLayout(2, seq => WSequence(WCommand("λ"), WConstant(" "), seq(0), WConstant(" ⇒ "), seq(1))),
//      (c, seq) => CC.Lambda(seq(0).asInstanceOf[ReferenceOrHole], seq(1))
//    )

    val Reference = SyntaxForm(
      AcceptanceCommand(s => true),
      Seq.empty,
      ToLayout(0, seq => WCommand()),
      (c, seq) => CC.Binding(c)
    )

    /* val Definition = SyntaxForm(
      ConstantCommand("def"),
      Seq(Binding, Term),
      ToLayout(2, seq => WSequence(WCommand(), WConstant(" "), seq(0), WConstant(" = "), seq(1))),
      (c, seq) => CC.Definition(seq(0).asInstanceOf[ReferenceOrHole], seq(1))
    )

    val Begin = SyntaxForm(
      ConstantCommand("{"),
      Seq(Term),
      ToLayout(-1, seq => WVertical(
        WCommand(),
        WSequence(WIndent, WVertical(seq: _*)),
        WConstant("}")
      )),
      (c, seq) => CC.Begin(seq: _*)
    )
    */

    Term.forms = Seq(Reference)

    Binding.forms = Seq(Reference)

    override val Lang = Language(Seq(Term, Binding), Term.forms)

    override def compile(l: Ast) = Left("")

    override def newHole() = CC.Hole()
  }
}
