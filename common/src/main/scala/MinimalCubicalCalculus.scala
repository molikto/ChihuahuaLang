/**
  * Created by molikto on 05/04/2017.
  */
object MinimalCubicalCalculus extends scala.App {


  object syntax {
    sealed abstract class DMA
    case class DMAMeet(ls: Seq[DMA]) extends DMA
    case class DMAJoin(ls: Seq[DMA]) extends DMA
    case class DMAInvolution(i: DMA) extends DMA
    case class DMARef(i: Int) extends DMA
    val DMA0 = DMAJoin(Seq.empty)
    val DMA1 = DMAMeet(Seq.empty)

    sealed abstract class Term
    case class Pi(from: Term, abs: Term) extends Term
    // inferred
    case class Lambda(abs: Term) extends Term
    case class App(left: Term, right: Term) extends Term
    // PathP in cubicaltt, the term is abstracted
    case class Path(abs: Term, left: Term, right: Term) extends Term
    // inferred
    case class PathAbs(abs: Term) extends Term
    case class PathApp(term: Term, i: DMA) extends Term
    case class Universe() extends Term
  }

  object semantics {

    sealed abstract class DMA
    sealed abstract class DMAStuck extends DMA
    case class DMAMeet(stuck: Seq[DMAStuck]) extends DMAStuck
    case class DMAJoin(stuck: Seq[DMAStuck]) extends DMAStuck
    case class DMAInvolution(stuck: DMAStuck) extends DMAStuck
    case class DMAGeneric(i: Int) extends DMAStuck
    case object DMA1 extends DMA
    case object DMA0 extends DMA
    private var _dmaGen = 0
    def DMAGen() = DMAGeneric({_dmaGen += 1; _dmaGen})

    // TODO normalization
    def meet(ds: Seq[DMA]) =
      if (ds.contains(DMA0)) DMA0 else {
        val f = ds.filter(_ != DMA1).map(_.asInstanceOf[DMAStuck]); if (f.isEmpty) DMA1 else DMAMeet(f) }
    def join(ds: Seq[DMA]) =
      if (ds.contains(DMA1)) DMA1 else {
        val f = ds.filter(_ != DMA0).map(_.asInstanceOf[DMAStuck]); if (f.isEmpty) DMA0 else DMAJoin(f) }
    def involution(f: DMA) = f match {
      case DMA1 => DMA0
      case DMA0 => DMA1
      case DMAInvolution(k) => k
      case a: DMAStuck => DMAInvolution(a)
    }


    sealed abstract class Value {
      def app(v: Value): Value = throw new Exception()
      def papp(d: DMA): Value = throw new Exception()
    }

    case class Universe() extends Value

    case class Lambda(f: Value => Value) extends Value {
      override def app(v: Value) = f(v)
    }
    case class Pi(left: Value, ty: Value => Value) extends Value

    // this is cubicaltt's PathP, where the type is also abstract
    case class Path(ty: DMA => Value, left: Value, right: Value) extends Value
    case class PathAbs(f: DMA => Value) extends Value {
      override def papp(d: DMA) = f(d)
    }

    sealed abstract class Stuck extends Value {
      override def app(v: Value) = App(this, v)
      override def papp(d: DMA) = PathApp(this, d)
    }
    case class LocalRef(i: Int) extends Stuck
    case class App(left: Stuck, right: Value) extends Stuck
    case class PathApp(left: Stuck, d: DMA) extends Stuck

    case class Generic(i: Int) extends Stuck

    var _gen = 0
    def Gen() = Generic({_gen += 1; _gen})

  }
  import semantics.Generic
  import semantics.DMAGeneric
  import semantics.Value
  import semantics.DMAGen
  import semantics.Gen
  import MinimalCubicalCalculus.syntax._


  sealed abstract class Ctx {

    def expand(ty: Value): Ctx = expand(Gen(), ty)
    def expand(g: Generic, ty: Value): Ctx = CtxExpand(g, ty, this)
    def abstrakt(g: DMAGeneric) = CtxAbstract(g, this)
    def abstrakt() = abstrakt(DMAGen())

    def eval(term: Term): Value = ???
    def eval(d: DMA): semantics.DMA = ???

    def readback(v: Value): Term = ???

    def infer(term: Term): Value = {
      term match {
        case Lambda(abs) => throw new Exception("Not accepted now")
        case Pi(from, abs) =>
          expand(checkIsTypeAndEval(from)).checkIsType(abs)
          semantics.Universe()
        case App(left, right) =>
          infer(left) match {
            case semantics.Pi(l, inside) => inside(checkAndEval(right, l))
            case _ => throw new Exception("Not accepted now")
          }
        case Path(abs, a, b) =>
          abstrakt().checkIsType(abs)
          check(a, eval(PathApp(PathAbs(abs), DMA0)))
          check(b, eval(PathApp(PathAbs(abs), DMA1)))
          semantics.Universe()
        case a@PathAbs(abs) =>
          val at = abstrakt()
          eval(Path(
            readback(at.infer(abs)),
            PathApp(a, DMA0),
            PathApp(a, DMA1)))
        case PathApp(term, d) =>
          infer(term) match {
            case semantics.PathAbs(f) => f(eval(d))
            case _ => throw new Exception("Not accepted now")
          }
      }
    }

    def checkAndEval(t: Term, v: Value) = { check(t, v); eval(t) }
    def checkIsTypeAndEval(t: Term) = { checkIsType(t); eval(t) }
    // no need to go inside check for now
    def checkIsType(t: Term): Unit = { val u = infer(t); assert(u == semantics.Universe()) }


    def check(term: Term, ty: Value): Unit = {
      (term, ty) match {
        case (Lambda(abs), semantics.Pi(left, f)) =>
          val gen = Gen()
          expand(gen, left).check(abs, f(gen))
        case (a@PathAbs(abs), semantics.Path(f, l, r)) =>
          val gen = DMAGen()
          abstrakt(gen).check(abs, f(gen))
          assert(eval(PathApp(a, DMA0)) == l)
          assert(eval(PathApp(a, DMA1)) == r)
        case (a, b) =>
          assert(infer(a) == b)
      }
    }
  }
  case object EmptyCtx extends Ctx
  case class CtxExpand(g: Generic, ty: Value, c: Ctx) extends Ctx
  case class CtxAbstract(g: DMAGeneric, c: Ctx) extends Ctx
  case class CtxRestrict(c: Ctx) extends Ctx


}
