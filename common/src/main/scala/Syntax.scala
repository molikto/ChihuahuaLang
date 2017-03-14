sealed abstract class Syntax
sealed abstract class Term extends Syntax

trait BindingSite

case class Module(ds: Seq[(String, Term)]) extends Syntax

case class GlobalReference(str: String) extends Term

// the big index is de Bruijn index, the small index is index in the binding sequence...
// for example
// def a_to_b_map = \(a: type, b: type, m: a -> b) => m(a)
// will be (type, type, 0.0 -> 0.1) 0.2(0.0)
// so if you view the Term AST as a tree, and each tree of type BindingSite increases the depth
// then to find the binding site from the reference, what you do is go up the tree, each time you
// go to a binding site you minus your reference by -1 or it is the site if you get 0
case class LocalReference(big: Int, small: Int) extends Term {
  assert(big >= 0 && small >= 0)

  override def toString = s"r($big, $small)"
}



case class Fix(t: Seq[Term]) extends Term with BindingSite {
  assert(t.size == 1)
}

case class Ascription(left: Term, right: Term) extends Term


case class Lambda(is: Seq[Option[Term]], body: Term) extends Term with BindingSite
case class Pi(is: Seq[Term], to: Term) extends Term with BindingSite
case class App(left: Term, right: Seq[Term]) extends Term

case class Let(vs: Seq[Term], body: Term) extends Term with BindingSite


case class Record(ms: Seq[String], ts: Seq[Term]) extends Term {
  assert(ms.size == ts.size)
}
case class Sigma(ms: Seq[String], ts: Seq[Term]) extends Term with BindingSite {
  assert(ms.size == ts.size)
}
case class Projection(left: Term, right: String) extends Term


case class Sum(ts: Map[String, Term]) extends Term
case class Construct(name: String, v: Term) extends Term
case class Split(left: Term, right: Map[String, Term]) extends Term with BindingSite // the right is binding. not left


case class Universe() extends Term

