package org.snailya.mygame

import org.snailya.mygame.UntypedLambdaCalculus.Lambda1

/**
  * Created by molikto on 01/03/2017.
  */
object ChihuahuaCalculus {


  abstract class Ast extends AstBase


  abstract class Term extends Ast
  abstract class Type extends Ast

  case class Reference(name: String) extends Term

  case class TypeReference(name: String) extends Type

  case class Fix(t: Term) extends Term

  case class Ascription(t: Term, ty: Type) extends Term

  /**
    * lambda
    */
  case class Lambda(parameters: Seq[(Reference, Option[Type])], body: Term) extends Term
  case class Application(left: Term, right: Seq[Term]) extends Term

  case class TypeFunction(left: Seq[Type], to: Type) extends Type

  /**
    * record
    */
  case class Record(ts: Seq[(String, Term)]) extends Term
  case class Projection(l: Term, r: String) extends Term

  case class TypeRecord(ts: Seq[(String, Type)]) extends Type

  /**
    * variant
    */
  case class Tagging(name: String, term: Term) extends Term
  case class Case(t: Term, cs: Seq[(String, Reference, Term)]) extends Term

  case class TypeVariant(vs: Seq[(String, Type)]) extends Type

  /**
    * ref
    */
  case class Ref(x: Term) extends Term
  case class Store(x: Term, y: Term) extends Term
  case class Read(x: Term) extends Term

  case class RefType(x: Type) extends Type
  case class SinkType(x: Type) extends Type
  case class SourceType(x: Type) extends Type

  /**
    * let
    */
  case class TermDef(name: Reference, Term: Term)
  case class TypeDef(name: TypeReference, t: Type)
  case class Let(bindings: Seq[Either[TermDef, TypeDef]], body: Term) extends Term

  case class Hole() extends Ast

  trait Frontend extends LanguageFrontendDynamics[Ast, Hole] {

    val CC = ChihuahuaCalculus

    val Term = SyntaxSort("term", null)

    val Binding = SyntaxSort("binding", null)

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
      (c, seq) => CC.Reference(c)
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

    override def NewHole() = CC.Hole()
  }
}
