sealed abstract class Syntax
sealed abstract class Term extends Syntax

// TODO not accurate yet
// this is currently a remark to remind me where the binding happens, but it is not
// good enough, each ast has their unique thing...
case class BindingSite() extends scala.annotation.StaticAnnotation


case class Module(ds: Seq[(String, Term)]) extends Syntax

case class GlobalReference(str: String) extends Term

// the big index is de Bruijn index, the small index is index in the binding sequence...
// for example
// def a_to_b_map = \(a: type, b: type, m: a -> b) => m(a)
// will be (type, type, 0.0 -> 0.1) 0.2(0.0)
// so if you view the Term AST as a tree, and each tree of type BindingSite increases the depth
// then to find the binding site from the reference, what you do is go up the tree, each time you
// go to a binding site you minus your reference by -1 or it is the site if you get 0
// all our binding site
case class LocalReference(big: Int, small: Int) extends Term {
  assert(big >= 0 && small >= 0)

  override def toString = s"r($big, $small)"
}



case class Fix(@BindingSite t: Seq[Term]) extends Term {
  assert(t.size == 1)
}

case class Ascription(left: Term, right: Term) extends Term


case class Lambda(@BindingSite is: Seq[Option[Term]], body: Term) extends Term
case class Pi(@BindingSite is: Seq[Term], to: Term) extends Term
case class App(left: Term, right: Seq[Term]) extends Term

case class Let(@BindingSite vs: Seq[Term], body: Term) extends Term


case class Record(ms: Seq[String], ts: Seq[Term]) extends Term {
  assert(ms.size == ts.size)
}
case class Sigma(ms: Seq[String], @BindingSite ts: Seq[Term]) extends Term {
  assert(ms.size == ts.size)
}
case class Projection(left: Term, right: String) extends Term


case class Sum(ts: Map[String, Term]) extends Term
case class Construct(name: String, v: Term) extends Term
case class Split(left: Term, @BindingSite right: Map[String, Term]) extends Term// the right is binding. not left


case class Universe() extends Term

