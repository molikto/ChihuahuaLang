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

    val BindingAndTypeSort = SyntaxSort("binding optional type", null)

    def ensureBindingOptionalTypeSort(t: Ast): (BindingOptionalType, Seq[Error]) = t match {
      case b: BindingOptionalType => (b, Seq.empty)
      case h: AstHole => (CC.BindingOptionalType(BindingHole(), None), Seq.empty)
      case a: Ast => (CC.BindingOptionalType(BindingHole(), None), mismatchError(a, BindingAndTypeSort))
    }

    val Sorts = Seq(
      TermSort,
      BindingSort,
      TypeSort,
      TypeBindingSort,
      BindingAndTypeSort
    )


    override val commandDelimiter = Seq(':', ',', '(')

    val BindingCommand = AcceptanceCommand(s => Some(Acceptance(false)))

    val BindingAndType = SyntaxForm(
      BindingCommand,
      Seq(
        ChildRelationship(TypeSort, 0, 1, 0, createCommand = Some(':'))
      ),
      seq => if (seq.isEmpty) WCommand() else WSequence(Seq(WCommand(), WConstant(": ")) ++ seq: _*),
      (c, seq) =>
        if (seq.isEmpty) {
          (CC.BindingOptionalType(CC.BindingName(c), None), Seq.empty)
        } else if (seq.size == 1) {
          val (t, e2) = ensureTypeSort(seq.head)
          (CC.BindingOptionalType(CC.BindingName(c), Some(t)), e2)
        } else {
          throw new Exception()
        }
    )

    val Binding = SyntaxForm(
      BindingCommand,
      Seq.empty,
      seq => WCommand(),
      (c, seq) => emptyError(CC.BindingName(c))
    )

    val TypeBinding = SyntaxForm(
      BindingCommand,
      Seq.empty,
      seq => WCommand(),
      (c, seq) => emptyError(CC.TypeBindingName(c))
    )

    val Lambda = SyntaxForm(
      AcceptanceCommand(s => if (s == "\\") Some(Acceptance(true)) else if (s == "lam") Some(Acceptance(false)) else None),
      Seq(
        ChildRelationship(BindingAndTypeSort, 0, MAX_BRANCH, 1, sepCommand = Some(',')),
        ChildRelationshipFixed(TermSort, 1)
      ),
      seq =>
        WSequence(
          Seq(WCommand("λ"), WConstant("(")) ++
            sep(seq.dropRight(1), () => WConstant(", ")) ++
            Seq(WConstant(") ⇒ "), seq.last): _*),
      (_, seq) => {
        val bs = seq.dropRight(1).map(ensureBindingOptionalTypeSort)
        val bst = bs.map(_._1)
        val bse = bs.flatMap(_._2)
        val (tm, te) = ensureTermSort(seq.last)
        (CC.Lambda(bst, tm), bse ++ te)
      }
    )


    val Application = SyntaxForm(
      RefuseAllCommand, // we don't have a command for application, instead, we always use the rotation command bellow
      Seq(
        ChildRelationshipFixed(TermSort, 1, rotationCommand = Some('(')),
        ChildRelationship(TermSort, 0, MAX_BRANCH, 1, sepCommand = Some(','))
      ),
      seq => WSequence(Seq(seq.head, WCommand("(")) ++ sep(seq.tail, () => WConstant(", ")) ++ Seq(WConstant(")")) : _*),
      (_, seq) => {
        val ps = seq.map(ensureTermSort)
        val ts = ps.map(_._1)
        val es = ps.flatMap(_._2)
        (CC.Application(ts.head, ts.tail), es)
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
      AcceptanceCommand(s => if (Try {
        Integer.parseInt(s)
      }.isSuccess) Some(Acceptance(false)) else None),
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
      Application,
      Lambda,
      PrimIntConstant,
      Binding
    )

    BindingAndTypeSort.forms = Seq(
      BindingAndType
    )

    BindingSort.forms = Seq(
      Binding
    )

    TypeSort.forms = Seq(TypeBinding)

    TypeBindingSort.forms = Seq(TypeBinding)

    val Forms = Seq(
      Application,
      Lambda,
      // dynamic
      PrimIntConstant,
      // bottom
      Binding,
      BindingAndType)

    override val Lang = Language(Sorts, Forms)

    override def compile(l: Ast) = Left("")

    override def newHole() = CC.AstHole()

  }

}
