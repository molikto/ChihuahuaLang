package org.snailya.mygame

import scala.collection.mutable

/**
  * Created by molikto on 03/03/2017.
  */
trait ChihuahuaCalculusCompiler extends ChihuahuaCalculusAst {


  type NType = Type

  abstract class CtxItem()
  final case class CtxType(name: String, ty: NType) extends CtxItem
  final case class CtxTerm(name: String, ty: NType) extends CtxItem
  var ctx: Seq[Seq[CtxItem]] = List.empty

  def levelUp() = ctx = List.empty +: ctx
  def levelDown() = ctx = ctx.tail

  def appendPrimitiveType(name: String) = {
    val t = PrimitiveType(name)
    appendType(TypeBindingName(name), t)
    t
  }


  levelUp()
  object ptype {
    val Int = appendPrimitiveType("Int")
    val Unit = appendPrimitiveType("Unit")
    val Bottom = appendPrimitiveType("Bottom")
  }


  object pterm {
    val unit = appendPrimitiveTerm("unit", ptype.Unit)
  }

  def appendPrimitiveTerm(name: String, ty: NType) = {
    appendTerm(BindingName(name), ty)
  }

  def appendTerm(name: Binding, ty: NType) = {
    name match {
      case BindingName(n) =>
        val a = CtxTerm(n, ty)
        ctx = (a +: ctx.head) +: ctx.tail
      case _ =>
    }
  }


  def appendType(name: TypeBinding, ty: NType) = {
    name match {
      case TypeBindingName(n) =>
        val a = CtxType(n, ty)
        ctx = (a +: ctx.head) +: ctx.tail
      case _ =>
    }
  }

  def lookupTerm(name: Binding): (NType, Seq[Error]) = {
    name match {
      case BindingName(n) =>
        for (a <- ctx) {
          for (b <- a) {
            b match {
              case CtxTerm(nn, ty) =>
                if (n == nn) return (ty, Seq.empty)
              case _ =>
            }
          }
        }
        (TypeHole(), Seq(Error(name.data, "cannot find term binding " + n)))
      case BindingHole() =>
        (TypeHole(), Seq.empty)
    }
  }

  def lookupType(name: TypeBinding): (NType, Seq[Error]) = {
    name match {
      case TypeBindingName(n) =>
        for (a <- ctx) {
          for (b <- a) {
            b match {
              case CtxType(nn, ty) =>
                if (nn == n) return (ty, Seq.empty)
              case _ =>
            }
          }
        }
        (TypeHole(), Seq(Error(name.data, "cannot find type binding " + n)))
      case TypeBindingHole() =>
        (TypeHole(), Seq.empty)
    }
  }

  /**
    * normalize a syntax type to ntype
    * @param t
    * @return
    */
  def normalize(t: Type): (NType, Seq[Error]) = {
    t match {
      case p: PrimitiveType => (p, Seq.empty)
      case r: TypeBinding => lookupType(r)
      case h: TypeHole => (h, Seq.empty)
      case a =>
        delog("normalize not imp error " + a.toString)
        (TypeHole(), Seq.empty)
    }
  }

  /**
    * infer the type of term t, return the normalized type, and errors
    * @param t
    * @return the pair type
    */
  def infer(t: Term): (NType, Seq[Error]) = {
    t match {
      case Let(bindings, term) =>
        // TODO uniqueness check for bindings
        levelUp()
        val e1 = bindings.flatMap(b => {
          b match {
            case Left(a) =>
              val res = infer(a.term)
              appendTerm(a.name, res._1)
              res._2
            case Right(a) =>
              val res = normalize(a.t)
              appendType(a.name, res._1)
              res._2
          }
        })
        val t = infer(term)
        levelDown()
        (t._1, t._2 ++ e1)
      case PrimIntConstant(a) =>
        (ptype.Int, Seq.empty)
      case Ascription(a, b) =>
        val k = normalize(b)
        val e2 = check(a, k._1)
        (k._1, k._2 ++ e2)
      case b: Binding =>
        lookupTerm(b)
      case Hole() =>
        (TypeHole(), Seq.empty)
      case a =>
        delog("infer not implemented error " + a.toString)
        (TypeHole(), Seq.empty)
    }
  }

  def subtype(left: NType, right: NType): Seq[Error] = {
    if (left == right || left == TypeHole() || right == TypeHole()) {
      Seq.empty
    } else {
      Seq(Error(null, left + " is not a subtype of " + right))
    }
  }

  /**
    * check a term has a normlized type
    * @param t
    * @param ty
    * @return
    */
  def check(t: Term, ty: NType): Seq[Error] = {
    (t, ty) match {
      case (PrimIntConstant(a), ptype.Int) =>
        Seq.empty
      case (k : Binding, a) =>
        val b = lookupTerm(k)
        val c = subtype(b._1, a)
        b._2 ++ c
      case (Hole(), _) =>
        Seq.empty
      case (_, TypeHole()) =>
        Seq.empty
      case _ =>
        val a= infer(t)
        subtype(a._1, ty) ++ a._2
    }
  }

  def compile(t: Term): Either[String, Seq[Error]] = {
    pterm.unit
    val a = infer(t)
    if (a._2.isEmpty) {
      Left("")
    } else {
      Right(a._2)
    }
  }
}
