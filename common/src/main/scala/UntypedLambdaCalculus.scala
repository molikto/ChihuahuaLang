import scala.util.Try

/**
  *
  * untyped lambda calculus with de Bruijn index
  *
  * this is translated form book *Types and Programming Languages*, Chapter 7
  */
object UntypedLambdaCalculus {

  // syntax with de Bruijn index
  sealed abstract class Term
  case class Var(i: Int) extends Term
  case class Abs(term: Term) extends Term
  case class App(left: Term, right: Term) extends Term

  // change what the open variables refers to in term0
  def shift(distance: Int, term0: Term) = {
    def rec(cut: Int, term: Term): Term = term match {
      case v@Var(i) => if (i >= cut) Var(i + distance) else v
      case Abs(t) => Abs(rec(cut + 1, t))
      case App(l, r) => App(rec(cut, l), rec(cut, r))
    }
    rec(0, term0)
  }

  // substitute of a term s for number j in a term t
  def substitution(j: Int, s: Term, t0: Term): Term = {
    def rec(depth: Int, t: Term): Term = t match {
      case v@Var(i) => if (i == depth + j) shift(depth, s) else v
      case Abs(term) => Abs(rec(depth + 1, term))
      case App(l, r) => App(rec(depth, l), rec(depth, r))
    }
    rec(0, t0)
  }

  // first, move the open variables in s up by 1, and substitute it into the body
  def substitutionTop(s: Term, t: Term) = shift(-1, substitution(0, shift(1, s), t))

  def smallStep(t: Term): Term = {
    def loop(t: Term): Term = t match {
      case App(Abs(t), right: Abs) =>
        substitutionTop(right, t)
      case App(Abs(t), right) =>
        App(Abs(t), loop(right))
      case App(t1, t2) =>
        App(loop(t1), t2)
      case _ => throw new Exception("")
    }
    var tt = t
    Try { while (true) { tt = loop (tt) } }
    tt
  }

  def bigStep(t: Term): Term = t match {
      case a@App(l, r) => bigStep(l) match {
        case Abs(t) =>
          bigStep(substitutionTop(bigStep(r), t))
        case a => App(a, r)
      }
      case a => a
    }
}

object tests {
  import UntypedLambdaCalculus._

  def test(eval: Term => Term) = {
    val v0 = Var(0)
    val v1 = Var(1)
    val v2 = Var(2)
    val tru = Abs(Abs(v1)) // \x\y.x
    val fls = Abs(Abs(v0)) // \x\y.y
    val and = Abs(Abs(App(App(v1, v0), fls))) // \a\b. ((a b) fls)
    val and_tru_tru = App(App(and, tru), tru) // (and tru tru) = ((tru tru) fls) = ((\y. tru) fls) = tru
    val and_tru_fls = App(App(and, tru), fls) // ...
    val and_fls_tru = App(App(and, fls), tru) // ...
    val and_fls_fls = App(App(and, fls), fls) // ...
    assert(eval(and_tru_tru) == tru)
    assert(eval(and_tru_fls) == fls)
    assert(eval(and_fls_tru) == fls)
    assert(eval(and_fls_fls) == fls)
    // this is only extensively equal to not
    val not1 = Abs(Abs(Abs(App(App(v2, v0), v1)))) // \a\b\c. ((a c) b)
    val not = Abs(App(App(v0, fls), tru)) // \a.((a fls) tru)
    val not_tru = App(not, tru)
    val not_fls = App(not, fls)
    assert(eval(not_tru) == fls)
    assert(eval(not_fls) == tru)
    // this is only extensively equal to or
    val or1 = Abs(Abs(App(not, App(App(and, v1), v0)))) // or = \a\b.not ((and a) b)
    val or = Abs(Abs(App(App(v1, tru), v0))) // or = \a\b.((a tru) b)
    val or_tru_tru = App(App(or, tru), tru) // (and tru tru) = ((tru tru) fls) = ((\y. tru) fls) = tru
    val or_tru_fls = App(App(or, tru), fls) // ...
    val or_fls_tru = App(App(or, fls), tru) // ...
    val or_fls_fls = App(App(or, fls), fls) // ...
    assert(eval(or_tru_tru) == tru)
    assert(eval(or_tru_fls) == tru)
    assert(eval(or_fls_tru) == tru)
    assert(eval(or_fls_fls) == fls)
    val c0 = Abs(Abs(v0))
    val c1 = Abs(Abs(App(v1, v0)))
    val c2 = Abs(Abs(App(v1, App(v1, v0))))
    val c3 = Abs(Abs(App(v1, App(v1, App(v1, v0)))))
    val c4 = Abs(Abs(App(v1, App(v1, App(v1, App(v1, v0))))))
    val suc = Abs(Abs(Abs(App(v1, App(App(v2, v1), v0)))))
    eval(App(suc, c0)) // Abs(Abs(App(Var(1),App(App(Abs(Abs(Var(0))),Var(1)),Var(0)))))
    // Abs(Abs(App(Var(1),App(App(Abs(Abs(Var(0))),Var(1)),Var(0)))))
    eval(App(suc, c1))
    eval(App(suc, c2))
  }
  test(smallStep)
  test(bigStep)
}

//tests
