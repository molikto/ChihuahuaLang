package org.snailya.mygame


import scala.util.Try

/**
  * Created by molikto on 01/03/2017.
  */
object ChihuahuaCalculus extends ChihuahuaCalculusAst {

  trait Frontend extends LanguageFrontendDynamics[Ast, AstHole] {

    val CC = ChihuahuaCalculus

    val TermSort = SyntaxSort("term", null)

    def ensureTermSort(t: Ast): (Term, Seq[Error]) = t match {
      case b: Term => (b, Seq.empty)
      case h: AstHole => (Hole(), Seq.empty)
      case a: Ast => (Hole(), mismatchError(a, TermSort))
    }

    val BindingSort = SyntaxSort("binding", null)

    def ensureBindingSort(t: Ast): (Binding, Seq[Error]) = t match {
      case b: BindingName => (b, Seq.empty)
      case h: AstHole => (BindingHole(), Seq.empty)
      case a: Ast => (BindingHole(), mismatchError(a, BindingSort))
    }

    val TypeSort = SyntaxSort("type", null)

    def ensureTypeSort(t: Ast): (Type, Seq[Error]) = t match {
      case b: Type => (b, Seq.empty)
      case h: AstHole => (TypeHole(), Seq.empty)
      case a: Ast => (TypeHole(), mismatchError(a, TypeSort))
    }

    val TypeBindingSort = SyntaxSort("type binding", null)

    val BindingOptionalTypeSort = SyntaxSort("binding optional type", null)

    def ensureBindingOptionalTypeSort(t: Ast): (BindingOptionalType, Seq[Error]) = t match {
      case b: BindingOptionalType => (b, Seq.empty)
      case h: AstHole => (CC.BindingOptionalType(BindingHole(), None), Seq.empty)
      case a: Ast => (CC.BindingOptionalType(BindingHole(), None), mismatchError(a, BindingOptionalTypeSort))
    }

    val Sorts = Seq(
      TermSort,
      BindingSort,
      TypeSort,
      TypeBindingSort,
      BindingOptionalTypeSort
    )


    val BindingOptionalType = SyntaxForm(
      ConstantCommand("::", autoCreate = true), // not actually used most of time...
      Seq(
        ChildRelationship(BindingSort, 1, 1),
        ChildRelationship(TypeSort, 0, 1)
      ),
      seq => WVertical(seq.take(1) ++ Seq(WCommand()) ++ seq.drop(1): _*),
      (_, seq) =>
        if (seq.size <= 2) {
          val (b, e1) = ensureBindingSort(seq.head)
          if (seq.size == 2) {
            val (t, e2) = ensureTypeSort(seq(1))
            (CC.BindingOptionalType(b, Some(t)), e1 ++ e2)
          } else {
            (CC.BindingOptionalType(b, None), e1)
          }
        } else {
          throw new Exception()
        }
    )

    val Lambda = SyntaxForm(
      ConstantCommand("\\"),
      Seq(
        ChildRelationship(BindingOptionalTypeSort, 0, MAX_BRANCH),
        ChildRelationship(TermSort, 1, 1)
      ),
      seq =>
        WSequence(
          Seq(WCommand("λ"), WConstant("(")) ++ seq.dropRight(1) ++ Seq(WConstant(")  ⇒ "), seq.last): _*),
      (_, seq) => {
        val bs = seq.dropRight(1).map(ensureBindingOptionalTypeSort)
        val bst = bs.map(_._1)
        val bse = bs.flatMap(_._2)
        val (tm, te) = ensureTermSort(seq.last)
        (CC.Lambda(bst, tm), bse ++ te)
      }
    )


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


    val Binding = SyntaxForm(
      AcceptanceCommand(s => true),
      Seq.empty,
      seq => WCommand(),
      (c, seq) => emptyError(CC.BindingName(c))
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
      (c, seq) => emptyError(CC.PrimIntConstant(Integer.parseInt(c)))
    )

    //    Term.forms = Seq(Reference)
    //    Binding.forms = Seq(Reference)

//    val Sorts = Seq(
//      TermSort,
//      BindingSort,
//      TypeSort,
//      TypeBindingSort,
//      BindingOptionalTypeSort
//    )

    TermSort.forms = Seq(
      Lambda,
      PrimIntConstant,
      Binding
    )

    BindingOptionalTypeSort.forms = Seq(
      BindingOptionalType
    )

    BindingSort.forms = Seq(
      Binding
    )

    TypeSort.forms = Seq()

    val Forms = Seq(
      BindingOptionalType, Lambda,
      // dynamic
      PrimIntConstant,
      // bottom
      Binding)

    override val Lang = Language(Sorts, Forms)

    override def compile(l: Ast) = Left("")

    override def newHole() = CC.AstHole()

  }

}
