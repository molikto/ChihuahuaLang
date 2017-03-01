package org.snailya.mygame

/**
  * Created by molikto on 01/03/2017.
  */
trait ChihuahuaCalculusAst {

  abstract class Ast extends AstBaseWithPositionData

  abstract class Term extends Ast
  abstract class Type extends Ast

  case class Hole() extends Ast

  case class Binding(name: String) extends Term

  case class TypeBinding(name: String) extends Type

  case class Fix(t: Term) extends Term

  case class Ascription(t: Term, ty: Type) extends Term

  /**
    * lambda
    */
  case class Lambda(parameters: Seq[(Binding, Option[Type])], body: Term) extends Term
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
  case class Case(t: Term, cs: Seq[(String, Binding, Term)]) extends Term

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
  case class TermDef(name: Binding, Term: Term)
  case class TypeDef(name: TypeBinding, t: Type)
  case class Let(bindings: Seq[Either[TermDef, TypeDef]], body: Term) extends Term



}
