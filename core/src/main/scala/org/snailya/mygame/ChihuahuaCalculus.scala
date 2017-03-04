package org.snailya.mygame


import scala.util.Try

/**
  * Created by molikto on 01/03/2017.
  */
object ChihuahuaCalculus extends ChihuahuaCalculusCompiler {

  trait Frontend extends LanguageFrontendDynamics[Ast, AstHole] {


    val CC = ChihuahuaCalculus

    val TermSort = SyntaxSort("term", null)

    def ensureTermSort(t: Ast): (Term, Seq[Error]) = t match {
      case b: Term => (b, Seq.empty)
      case h: AstHole => (Hole(), Seq.empty)
      case a: Ast => (Hole(), mismatchError(a, TermSort))
    }

    val TermOrDefsSort = SyntaxSort("terms or defs", null)


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

    def ensureTypeBindingSort(t: Ast): (TypeBinding, Seq[Error]) = t match {
      case b: TypeBindingName => (b, Seq.empty)
      case h: AstHole => (TypeBindingHole(), Seq.empty)
      case a: Ast => (TypeBindingHole(), mismatchError(a, BindingSort))
    }

    val BindingAndTypeSort = SyntaxSort("binding with type", null)

    def ensureBindingOptionalTypeSort(t: Ast): (BindingOptionalType, Seq[Error]) = t match {
      case b: BindingOptionalType => (b, Seq.empty)
      case h: AstHole => (CC.BindingOptionalType(BindingHole(), None), Seq.empty)
      case a: Ast => (CC.BindingOptionalType(BindingHole(), None), mismatchError(a, BindingAndTypeSort))
    }


    val RecordItemSort = SyntaxSort("record item", null)

    val TypeRecordItemSort = SyntaxSort("record type item", null)

    val CaseItemSort = SyntaxSort("case item", null)

    val TypeVariantItemSort = SyntaxSort("variant type item", null)

    val Sorts = Seq(
      TermSort,
      TermOrDefsSort,
      BindingSort,
      TypeSort,
      TypeBindingSort,
      BindingAndTypeSort,
      RecordItemSort,
      TypeRecordItemSort,
      CaseItemSort,
      TypeVariantItemSort
    )


    override val commandDelimiter = Seq(':', ',', '(', '@', '{', '=', ';')

    val BindingCommand = AcceptanceCommand(s => {
      if (s.nonEmpty && !"0123456789".contains(s.head)) {
        Some(Acceptance(false))
      } else {
        None
      }
    })

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
      seq => WSequence(Seq(seq.head, WCommand()) ++ sep(seq.tail, () => WConstant(", ")) ++ Seq(WConstant(")")): _*),
      (_, seq) => {
        val ps = seq.map(ensureTermSort)
        val ts = ps.map(_._1)
        val es = ps.flatMap(_._2)
        (CC.Application(ts.head, ts.tail), es)
      }
    )

    val Ascription = SyntaxForm(
      RefuseAllCommand, // we don't have a command for application, instead, we always use the rotation command bellow
      Seq(
        ChildRelationshipFixed(TermSort, 1, rotationCommand = Some('@')),
        ChildRelationshipFixed(TypeSort, 1)
      ),
      seq => WSequence(Seq(seq.head, WConstant(" "), WCommand(), WConstant(" "), seq(1)): _*),
      (_, seq) => {
        val t1 = ensureTermSort(seq.head)
        val t2 = ensureTypeSort(seq(1))
        (CC.Ascription(t1._1, t2._1), t1._2 ++ t2._2)
      }
    )

    val TypeFunction = SyntaxForm(
      ConstantCommand("pi"),
      Seq(
        ChildRelationship(TypeSort, 0, MAX_BRANCH, 1, sepCommand = Some(',')),
        ChildRelationshipFixed(TypeSort, 1)
      ),
      seq =>
        WSequence(
          optDelimit(WConstant("("),
            sep(seq.dropRight(1), () => WConstant(", ")),
            WConstant(")")) ++
            Seq(WConstant(" "), WCommand("⇒"), WConstant(" "), seq.last): _*),
      (_, seq) => {
        val bs = seq.map(ensureTypeSort)
        val bst = bs.map(_._1)
        val bse = bs.flatMap(_._2)
        (CC.TypeFunction(bst.dropRight(1), bst.last), bse)
      }
    )

    val TermDef = SyntaxForm(
      ConstantCommand("def"),
      Seq(
        ChildRelationshipFixed(BindingSort, 1),
        ChildRelationshipFixed(TermSort, 1)
      ),
      seq => WSequence(WCommand(), WConstant(" "), seq(0), WConstant(" = "), seq(1)),
      (_, seq) => {
        val bs = ensureBindingSort(seq(0))
        val ts = ensureTermSort(seq(1))
        (CC.TermDef(bs._1, ts._1), bs._2 ++ ts._2)
      }
    )

    val TypeDef = SyntaxForm(
      ConstantCommand("typedef"),
      Seq(
        ChildRelationshipFixed(TypeBindingSort, 1),
        ChildRelationshipFixed(TypeSort, 1)
      ),
      seq => WSequence(WCommand(), WConstant(" "), seq(0), WConstant(" = "), seq(1)),
      (_, seq) => {
        val bs = ensureTypeBindingSort(seq(0))
        val ts = ensureTypeSort(seq(1))
        (CC.TypeDef(bs._1, ts._1), bs._2 ++ ts._2)
      }
    )

    def ensureDefSort(t: Ast): (Either[TermDef, TypeDef], Seq[Error]) = t match {
      case b: Term => (Left(CC.TermDef(CC.BindingIgnore(), b)), Seq.empty)
      case a: TermDef => (Left(a), Seq.empty)
      case a: TypeDef => (Right(a), Seq.empty)
      case a: Ast => (Left(CC.TermDef(CC.BindingIgnore(), CC.Hole())), mismatchError(a, TermOrDefsSort))
    }

    val Block = SyntaxForm(
      ConstantCommand("{", acc = Acceptance(true)),
      Seq(
        ChildRelationship(TermOrDefsSort, 0, MAX_BRANCH, 1, sepCommand = Some(';'))
      ),
      seq => WVertical(WCommand(), WSequence(WIndent(), WVertical(seq: _*)), WConstant("}")),
      (c, seqp) => {
        val seq = seqp.filter(!_.isInstanceOf[AstHole])
        if (seq.isEmpty) {
          (CC.Let(Seq.empty, Prelude.TermUnit), Seq.empty)
        } else {
          val appended = if (!seq.last.isInstanceOf[Term]) {
            seq ++ Seq(Prelude.TermUnit)
          } else {
            seq
          }
          val ps = appended.dropRight(1).map(ensureDefSort)
          val k = ensureTermSort(appended.last)
          (CC.Let(ps.map(_._1), k._1), ps.flatMap(_._2) ++ k._2)
        }
      },
      isBlock = true
    )


    val RecordItem = SyntaxForm(
      BindingCommand,
      Seq(ChildRelationshipFixed(TermSort, 1, createCommand = Some('='))),
      seq => WSequence(WCommand(), WConstant(" = "), seq.head),
      (c, seqp) => {
        val ps = ensureTermSort(seqp.head)
        (CC.RecordItem(c, ps._1), ps._2)
      }
    )

    // TODO tuples needs look ahead...

    val Record = SyntaxForm(
      ConstantCommand("[", acc = Acceptance(true)),
      Seq(ChildRelationship(RecordItemSort, 0, MAX_BRANCH, 1, sepCommand = Some(','))),
      seq => WSequence(WCommand() +: sep(seq, () => WConstant(", ")) :+ WConstant("]"): _*),
      (c, seq) => {
        val nHole = seq.filter(!_.isInstanceOf[AstHole])
        val rs = nHole.filter(_.isInstanceOf[CC.RecordItem]).map(_.asInstanceOf[CC.RecordItem])
        val es = nHole.filter(!_.isInstanceOf[CC.RecordItem]).flatMap(a => mismatchError(a, RecordItemSort))
        (CC.Record(rs), es)
      }
    )

    val TypeRecordItem = SyntaxForm(
      BindingCommand,
      Seq(ChildRelationshipFixed(TypeSort, 1, createCommand = Some(':'))),
      seq => WSequence(WCommand(), WConstant(" : "), seq.head),
      (c, seqp) => {
        val ps = ensureTypeSort(seqp.head)
        (CC.NameAndType(c, ps._1), ps._2)
      }
    )

    val TypeRecord = SyntaxForm(
      ConstantCommand("[", acc = Acceptance(true)),
      Seq(ChildRelationship(TypeRecordItemSort, 0, MAX_BRANCH, 1, sepCommand = Some(','))),
      seq => WSequence(WCommand() +: sep(seq, () => WConstant(", ")) :+ WConstant("]"): _*),
      (c, seq) => {
        val nHole = seq.filter(!_.isInstanceOf[AstHole])
        val rs = nHole.filter(_.isInstanceOf[CC.NameAndType]).map(_.asInstanceOf[CC.NameAndType])
        val es = nHole.filter(!_.isInstanceOf[CC.NameAndType]).flatMap(a => mismatchError(a, TypeRecordItemSort))
        (CC.TypeRecord(rs), es)
      }
    )

    // we currantly use a rotation command.. if we want to implement ones without #, we need to declear variables...
    val Tagging = SyntaxForm(
      AcceptanceCommand(s =>
        if (s.nonEmpty && s.endsWith("#") && BindingCommand.accept(s.dropRight(1)).nonEmpty)
          Some(Acceptance(true, containsCurrent = false))
        else None
      ),
      Seq(ChildRelationshipFixed(TermSort, 1)),
      seq => WSequence(WCommand(), WConstant("#"), seq.head),
      (c, seq) => {
        val ps = ensureTermSort(seq.head)
        (CC.Tagging(c, ps._1), ps._2)
      }
    )

    val TypeVariantItem = SyntaxForm(
      BindingCommand,
      Seq(ChildRelationshipFixed(TypeSort, 1, transformCommand = Some('#'))),
      seq => WSequence(WCommand(), WConstant(" # "), seq.head),
      (c, seqp) => {
        val ps = ensureTypeSort(seqp.head)
        (CC.NameAndType(c, ps._1), ps._2)
      }
    )

    // TODO support default type unit...

    val TypeVariant = SyntaxForm(
      ConstantCommand("variant"),
      Seq(ChildRelationship(TypeVariantItemSort, 0, MAX_BRANCH, 1, sepCommand = Some(','))),
      seq => WSequence(WCommand() +: WConstant("{") +: sep(seq, () => WConstant(", ")) :+ WConstant("}"): _*),
      (c, seq) => {
        val nHole = seq.filter(!_.isInstanceOf[AstHole])
        val rs = nHole.filter(_.isInstanceOf[CC.NameAndType]).map(_.asInstanceOf[CC.NameAndType])
        val es = nHole.filter(!_.isInstanceOf[CC.NameAndType]).flatMap(a => mismatchError(a, TypeVariantItemSort))
        (CC.TypeVariant(rs), es)
      }
    )

    val CaseItem = SyntaxForm(
      BindingCommand,
      Seq(
        ChildRelationshipFixed(BindingSort, 1),
        ChildRelationshipFixed(TermSort, 1)
      ),
      seq => WSequence(WCommand(), WConstant("#"), seq.head, WConstant(" ⇒ "), seq(1)),
      (c, seq) => {
        val t1 = ensureBindingSort(seq.head)
        val t2 = ensureTermSort(seq(1))
        (CC.CaseItem(c, t1._1, t2._1), t1._2 ++ t2._2)
      }
    )

    val Case = SyntaxForm(
      ConstantCommand("case"),
      Seq(
        ChildRelationshipFixed(TermSort, 1),
        ChildRelationship(CaseItemSort, 0, MAX_BRANCH, 1)),
      seq => WVertical(WSequence(WCommand(), WConstant(" "), seq.head) +: seq.tail :_*),
      (c, seq) => {
        val t1 = ensureTermSort(seq.head)
        val t2 = seq.tail.filter(a => !a.isInstanceOf[Hole])
        val t3 = t2.filter(_.isInstanceOf[CaseItem]).map(_.asInstanceOf[CaseItem])
        val t4 = t2.filter(!_.isInstanceOf[CaseItem]).flatMap(a => mismatchError(a, CaseItemSort))
        (CC.Case(t1._1, t3), t1._2 ++ t4)
      }
    )

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
      Case,
      Tagging,
      Record,
      Block,
      Application,
      Ascription,
      Lambda,
      PrimIntConstant,
      Binding
    )

    TermOrDefsSort.forms = Seq(TermDef, TypeDef) ++ TermSort.forms

    BindingAndTypeSort.forms = Seq(
      BindingAndType
    )

    BindingSort.forms = Seq(
      Binding
    )

    TypeSort.forms = Seq(TypeVariant, TypeFunction, TypeBinding, TypeRecord)

    TypeBindingSort.forms = Seq(TypeBinding)

    RecordItemSort.forms = Seq(RecordItem)

    TypeRecordItemSort.forms = Seq(TypeRecordItem)

    TypeVariantItemSort.forms = Seq(TypeVariantItem)

    CaseItemSort.forms = Seq(CaseItem)

    val Forms = Seq(
      Case,
      TypeVariant,
      Tagging,
      Record,
      TypeRecord,
      Block,
      TermDef,
      TypeDef,
      Application,
      Ascription,
      Lambda,
      TypeFunction,
      // dynamic
      PrimIntConstant,
      // bottom
      RecordItem,
      TypeRecordItem,
      Binding,
      BindingAndType,
      TypeVariantItem
    )

    override val Lang = Language(Sorts, Forms, Some(TermSort))

    override def compile(l: Ast): Either[String, Seq[Error]] = {
      val t = ensureTermSort(l)
      if (t._2.nonEmpty) {
        Right(t._2)
      } else {
        CC.compile(t._1)
      }
    }

    override def newHole() = CC.AstHole()

  }

}
