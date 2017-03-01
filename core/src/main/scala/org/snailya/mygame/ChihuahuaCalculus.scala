package org.snailya.mygame


import scala.util.Try

/**
  * Created by molikto on 01/03/2017.
  */
object ChihuahuaCalculus extends ChihuahuaCalculusAst {

  trait Frontend extends LanguageFrontendDynamics[Ast, Hole] {

    val CC = ChihuahuaCalculus

    val TermSort = SyntaxSort("term", null)

    val BindingSort = SyntaxSort("binding", null)

    val TypeSort = SyntaxSort("type", null)

    val TypeBindingSort = SyntaxSort("type binding", null)

    val BindingOptionalTypeSort = SyntaxSort("binding optional type", null)

    val Sorts = Seq(
      TermSort,
      BindingSort,
      TypeSort,
      TypeBindingSort,
      BindingOptionalTypeSort
    )

//    val BindingOptionalType = SyntaxForm(
//      ConstantCommand("::"),
//      Seq(BindingSort),
//    )
//    val Lambda = SyntaxForm(
//      ConstantCommand("\\"),
//      Seq(BindingOptionalTypeSort),
//      ToWidget(-1, seq =>
//        WSequence(
//          Seq(WCommand("λ"), WConstant("(")) ++ seq.dropRight(1): _*),
//      (c, seq) => CC.Lambda(seq(0), seq(1))
//    )


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

    val Reference = SyntaxForm(
      AcceptanceCommand(s => true),
      Seq.empty,
      seq => WCommand(),
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

    val PrimIntConstant = SyntaxForm(
      AcceptanceCommand(s => Try {
        Integer.parseInt(s)
      }.isSuccess),
      Seq.empty,
      _ => WCommand(),
      (c, seq) => CC.PrimIntConstant(Integer.parseInt(c))
    )

    //    Term.forms = Seq(Reference)
    //    Binding.forms = Seq(Reference)

    val Forms = Seq(Reference, PrimIntConstant)

    override val Lang = Language(Sorts, Forms)

    override def compile(l: Ast) = Left("")

    override def newHole() = CC.Hole()
  }

}
