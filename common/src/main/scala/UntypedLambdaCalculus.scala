import com.twitter.util.Eval

import scala.util.Try

/**
  *
  * untyped lambda calculus with de Bruijn index
  *
  * this is translated form book *Types and Programming Languages*, Chapter 7
  */

abstract class Head() {
  def app(head: Head): Head
}

case class Lam(f: Head => Head) extends Head {
  override def app(head: Head) = f(head)
}

// it is depth, or what's the index if called relative to root(of the compile or readback), or the reverse de Bruijn index
// so for all bounded variable, it is always negative
case class Acc(depth: Int, as: Seq[Head] = List.empty) extends Head {
  override def app(head: Head) = Acc(depth, as :+ head)
}

object UntypedLambdaCalculus extends scala.App {

  // syntax with de Bruijn index
  sealed abstract class Term
  sealed abstract class Value extends Term
  case class Var(i: Int) extends Term
  case class Abs(term: Term) extends Value
  case class App(left: Term, right: Term) extends Term

  case class NativeInt(i: Int) extends Value

  case class NativeAppPlusInt(i: Int) extends Value

  case object NativePlus extends Value

  // see paper *Full reduction at full throttle*, or *A compiled implementation of strong reduction*
  //  sealed abstract class ExtendedTerm extends Term
  //  case class Accumulator(v: Int, vs: Seq[Value]) extends ExtendedTerm with Value

  // change what the open variables refers to in term0
  def shift(distance: Int, term0: Term) = {
    def rec(cut: Int, term: Term): Term = term match {
      case v@Var(i) => if (i >= cut) Var(i + distance) else v
      case Abs(t) => Abs(rec(cut + 1, t))
      case App(l, r) => App(rec(cut, l), rec(cut, r))
      case k => k
    }

    rec(0, term0)
  }

  // substitute of a term s for number j in a term t
  def substitution(j: Int, s: Term, t0: Term): Term = {
    def rec(depth: Int, t: Term): Term = t match {
      case v@Var(i) => if (i == depth + j) shift(depth, s) else v
      case Abs(term) => Abs(rec(depth + 1, term))
      case App(l, r) => App(rec(depth, l), rec(depth, r))
      case k => k
    }

    rec(0, t0)
  }

  // first, move the open variables in s up by 1, and substitute it into the body
  def substitutionTop(s: Term, t: Term) = shift(-1, substitution(0, shift(1, s), t))

  def isValue(t: Term) = t.isInstanceOf[Value]

  /**
    * or, weak head reduction
    */
  def smallStep(t: Term, i: Int = -1): Term = {
    def loop(t: Term): Term = t match {
      case App(Abs(t), right) if isValue(right) => substitutionTop(right, t)
      case App(Abs(t), right) => App(Abs(t), loop(right))
      case App(NativePlus, NativeInt(i1)) => NativeAppPlusInt(i1)
      case App(NativePlus, t) => App(NativePlus, loop(t))
      case App(NativeAppPlusInt(i2), NativeInt(i1)) => NativeInt(i1 + i2)
      case App(a@NativeAppPlusInt(_), t) => App(a, loop(t))
      case App(t1, t2) => App(loop(t1), t2)
      case _ => throw new Exception("")
    }

    var tt = t
    Try {
      var k = 0;
      while (k != i) {
        tt = loop(tt);
        k += 1
      }
    }
    tt
  }

  /**
    * or, weak head reduction
    */
  def bigStep(t: Term): Term = t match {
    case a@App(l, r) => bigStep(l) match {
      case Abs(t) =>
        bigStep(substitutionTop(bigStep(r), t))
      case NativePlus => bigStep(r) match {
        case NativeInt(i) => NativeAppPlusInt(i)
        case a => App(NativePlus, a)
      }
      case k@NativeAppPlusInt(i) => bigStep(r) match {
        case NativeInt(j) => NativeInt(i + j)
        case a => App(k, a)
      }
      case a => App(a, r)
    }
    case a => a
  }

  /**
    * normalization by evaluation
    *
  // depth ::::  [depth = -1] \x {... depth = 0....}
    *
  // z is open
        // it is index 2 in term
        // computes to Acc(2 - 1 - 1) = 0
        // or such that when depth = -1, i.e. in start states, it have index 0
        // \x.\y.z  ...
        // when depth = -1, x is changed to Acc(- 0 - 1 = -1)
        // when depth = 0, y is changed to Acc(-1 -1 = -2)
        // \x.\y.x
    */

  def nbe(t: Term): Term = {
    def emitScala(t: Term, depth: Int = -1): String = t match {
      case Var(i) =>
        if (i > depth) s"Acc(${i - depth - 1})"
        else "v" + (depth - i)
      case Abs(t) =>
        val d = depth + 1
        s"Lam((v$d: Head) => {${emitScala(t, d)}})"
      case App(l, r) =>
        s"(${emitScala(l, depth)}).app(${emitScala(r, depth)})"
    }
    val text = emitScala(t)
    val compiled = new Eval().apply[Head](text)
    def readback(h: Head, depth: Int = -1): Term = h match {
      case Lam(f) =>
        val d = depth + 1
        Abs(readback(f(Acc(-d - 1)), d))
      case Acc(d, seq) =>
        seq.foldLeft[Term](Var(depth + d + 1)) { (l, s) =>
          App(l, readback(s, depth))
        }
    }
    readback(compiled)
  }


  object tests {

    val v0 = Var(0)
    val v1 = Var(1)
    val v2 = Var(2)

    val tru = Abs(Abs(v1))
    // \x\y.x
    val fls = Abs(Abs(v0)) // \x\y.y

    val and = Abs(Abs(App(App(v1, v0), fls)))
    // \a\b. ((a b) fls)
    val and_tru_tru = App(App(and, tru), tru)
    // (and tru tru) = ((tru tru) fls) = ((\y. tru) fls) = tru
    val and_tru_fls = App(App(and, tru), fls)
    // ...
    val and_fls_tru = App(App(and, fls), tru)
    // ...
    val and_fls_fls = App(App(and, fls), fls) // ...

    val not1 = Abs(Abs(Abs(App(App(v2, v0), v1))))
    // \a\b\c. ((a c) b)
    val not = Abs(App(App(v0, fls), tru))
    // \a.((a fls) tru)
    val not_tru = App(not, tru)
    val not_fls = App(not, fls)
    // this is only extensively equal to or
    val or1 = Abs(Abs(App(not, App(App(and, App(not, v1)), App(not, v0)))))
    // or = \a\b.not ((and a) b)
    val or = Abs(Abs(App(App(v1, tru), v0)))
    // or = \a\b.((a tru) b)
    val or_tru_tru = App(App(or, tru), tru)
    // (and tru tru) = ((tru tru) fls) = ((\y. tru) fls) = tru
    val or_tru_fls = App(App(or, tru), fls)
    // ...
    val or_fls_tru = App(App(or, fls), tru)
    // ...
    val or_fls_fls = App(App(or, fls), fls) // ...

    val or1_tru_tru = App(App(or1, tru), tru)
    // (and tru tru) = ((tru tru) fls) = ((\y. tru) fls) = tru
    val or1_tru_fls = App(App(or1, tru), fls)
    // ...
    val or1_fls_tru = App(App(or1, fls), tru)
    // ...
    val or1_fls_fls = App(App(or1, fls), fls) // ...

    val c0 = Abs(Abs(v0))
    val c1 = Abs(Abs(App(v1, v0)))
    val c2 = Abs(Abs(App(v1, App(v1, v0))))
    val c3 = Abs(Abs(App(v1, App(v1, App(v1, v0)))))
    val c4 = Abs(Abs(App(v1, App(v1, App(v1, App(v1, v0))))))
    val suc = Abs(Abs(Abs(App(v1, App(App(v2, v1), v0))))) // scc = λn. λs. λz. s (n s z);

    // a = (λx.x)(λy. (λz.z) y (λt.t))
    val id = Abs(v0)
    val cst1 = App(id, Abs(App(App(id, v0), id)))
    val cst1_nf = Abs(App(v0, id))

    def assert_weak(a: Term, nf: Term) = assert(a != nf && a.isInstanceOf[Value])

    def test(eval: Term => Term) = {
      assert(eval(and_tru_tru) == tru)
      assert(eval(and_tru_fls) == fls)
      assert(eval(and_fls_tru) == fls)
      assert(eval(and_fls_fls) == fls)
      assert(eval(not_tru) == fls)
      assert(eval(not_fls) == tru)
      assert(eval(or_tru_tru) == tru)
      assert(eval(or_tru_fls) == tru)
      assert(eval(or_fls_tru) == tru)
      assert(eval(or_fls_fls) == fls)

      assert(eval(or1_tru_tru) == tru)
      assert(eval(or1_tru_fls) == tru)
      assert(eval(or1_fls_tru) == tru)
      assert(eval(or1_fls_fls) == fls)

      assert_weak(eval(App(suc, c0)), c1)
      assert_weak(eval(App(suc, c1)), c2)
      assert_weak(eval(App(suc, c2)), c3)
      assert_weak(eval(cst1), cst1_nf)
      assert(eval(App(App(NativePlus, App(App(tru, NativeInt(4)), NativeInt(3))), NativeInt(4))) == NativeInt(8))
    }

    val omega = App(Abs(App(v0, v0)), Abs(App(v0, v0)))
    test(a => smallStep(a))
    test(bigStep)

    assert(nbe(cst1) == cst1_nf)
    assert(nbe(App(suc, c0)) == c1)
    assert(nbe(App(suc, c1)) == c2)
    assert(nbe(App(suc, c2)) == c3)
    assert(nbe(App(suc, c3)) == c4)


    Seq(tru, fls, c0, c1, c2, c3, c4, suc).foreach(a => {
      assert(nbe(a) == a)
    })

    assert(smallStep(omega, 1) == omega)
  }

  override def main(args: Array[String]) = UntypedLambdaCalculus.tests

}




