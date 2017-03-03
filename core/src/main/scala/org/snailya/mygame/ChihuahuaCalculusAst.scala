package org.snailya.mygame

/**
  * Created by molikto on 01/03/2017.
  */
trait ChihuahuaCalculusAst {

  abstract class Ast extends AstBaseWithPositionData
  case class AstHole() extends Ast

  abstract class Term extends Ast
  case class Hole() extends Term // when it construct a hole, it will be

  abstract class Type extends Ast
  case class TypeHole() extends Type


  object Prelude {
    val TermUnit = BindingName("unit")
  }

  abstract class Binding() extends Term
  case class BindingName(name: String) extends Binding
  case class BindingHole() extends Binding
  case class BindingIgnore() extends Binding

  abstract class TypeBinding() extends Type
  case class TypeBindingName(name: String) extends TypeBinding
  case class TypeBindingHole() extends TypeBinding

  case class Fix(t: Term) extends Term

  case class Ascription(t: Term, ty: Type) extends Term

  /**
    * lambda
    */
  case class BindingOptionalType(binding: Binding, ty: Option[Type]) extends Ast
  case class Lambda(parameters: Seq[BindingOptionalType],  body: Term) extends Term
  case class Application(left: Term, right: Seq[Term]) extends Term

  case class TypeFunction(left: Seq[Type], to: Type) extends Type

  /**
    * record
    */
  case class RecordItem(b: String, t: Term) extends Ast
  case class Record(ts: Seq[RecordItem]) extends Term
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
  case class TermDef(name: Binding, Term: Term) extends Ast
  case class TypeDef(name: TypeBinding, t: Type) extends Ast
  case class Let(bindings: Seq[Either[TermDef, TypeDef]], body: Term) extends Term


  /**
    * primitive
    */
  case class PrimIntConstant(i: Int) extends Term

}
