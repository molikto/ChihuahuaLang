package org.snailya.mygame


object UntypedArithmetic {

  abstract class Ast extends AstBase {
  }
  case object True extends Ast
  case object False extends Ast
  case class If(pred: Ast, left: Ast, right: Ast) extends Ast
  case class Pred(a: Ast) extends Ast
  case class Succ(a: Ast) extends Ast
  case class IsZero(a: Ast) extends Ast
  case class Number(s: BigInt) extends Ast
  case class Hole() extends Ast

  trait Frontend extends LanguageFrontendDynamics[UntypedArithmetic.Ast, UntypedArithmetic.Hole] {
    val UAA = UntypedArithmetic

    def newHole() = UAA.Hole()

    abstract class Type
    case object TypeBoolean extends Type
    case object TypeNumber extends Type
    case object TypeAny extends Type

    abstract class Value
    case class ValueBoolean(b: Boolean) extends Value
    case class ValueNumber(s: BigInt) extends Value

    def ensureType(a: UAA.Ast, t: Type): Seq[Error] = {
      val ires = infer(a)
      val e = if (ires._1 == TypeAny || ires._1 == t) Seq.empty[Error] else Seq(Error(a.data, "expecting type " + t + ", but got " + ires._1))
      ires._2 ++ e
    }

    def ensureNumber(a: UAA.Ast) = ensureType(a, TypeNumber)

    def ensureBoolean(a: UAA.Ast) = ensureType(a, TypeBoolean)

    def min(a: Type, b: Type) = if (a == b) a else TypeAny

    def infer(t: UAA.Ast): (Type, Seq[Error]) = {
      t match {
        case UAA.False | UAA.True => (TypeBoolean, Seq.empty)
        case _: UAA.Number => (TypeNumber, Seq.empty)
        case UAA.Pred(a) => (TypeNumber, ensureNumber(a))
        case UAA.Succ(a) => (TypeNumber, ensureNumber(a))
        case UAA.Hole() => (TypeAny, Seq.empty)
        case UAA.IsZero(a) => (TypeBoolean, ensureNumber(a))
        case i@UAA.If(a, b, c) =>
          val ra = ensureBoolean(a)
          val rb = infer(b)
          val rc = infer(c)
          val te = if (rb._1 == TypeAny || rc._1 == TypeAny) {
            (TypeAny, Seq.empty[Error])
          } else if (rb._1 != rc._1) {
            (TypeAny, Seq(Error(i.data, "two branch type mismatch")))
          } else {
            (rb._1, Seq.empty[Error])
          }
          (te._1, ra ++ rb._2 ++ rc._2 ++ te._2)
      }
    }

    override def compile(t: UntypedArithmetic.Ast): Either[String, Seq[Error]] = {
      val a = infer(t)
      if (a._2.isEmpty) Left("no errors") else Right(a._2)
    }

    val Term = SyntaxSort("term", null)
    val True = SyntaxFormConstant("true", UAA.True)
    val False = SyntaxFormConstant("false", UAA.False)
    val IfThenElse = SyntaxForm(ConstantCommand("if"), Seq(Term, Term, Term),
      ToLayout(3, (seq) => {
        WVertical(
          WSequence(WCommand(), WConstant(" "), seq(0)),
          WSequence(WIndent, seq(1)),
          WConstant("else"),
          WSequence(WIndent, seq(2))
        )
      }),
      (name, childs) => UAA.If(childs(0), childs(1), childs(2))
    )
    val Succ = SyntaxFormApplicative1("succ", Term, (_, a) => UAA.Succ(a(0)))
    val Pred = SyntaxFormApplicative1("pred", Term, (_, a) => UAA.Pred(a(0)))
    val IsZero = SyntaxFormApplicative1("iszero", Term, (_, a) => UAA.IsZero(a(0)))
    def isPositiveNumber(s: String) = {
      s.length > 0 && s.forall(a => "0123456789".contains(a))
    }
    def isNumber(s: String): Boolean = {
      if (s.startsWith("-")) isPositiveNumber(s.substring(1))
      else isPositiveNumber(s)
    }
    val Number = SyntaxForm(AcceptanceCommand(isNumber), Seq(), layouts.Inline1, (c, _) => UAA.Number(BigInt(c)))
    Term.forms = Seq(True, False, IfThenElse, Number, Succ, Pred, IsZero)
    override val Lang = Language(Seq(Term), Term.forms)
  }
}

