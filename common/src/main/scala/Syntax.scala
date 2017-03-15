sealed abstract class Syntax
sealed abstract class Term extends Syntax {
  def closed(i: Int): Boolean
}

// TODO not accurate yet
// this is currently a remark to remind me where the binding happens, but it is not
// good enough, each ast has their unique thing...
case class BindingSite() extends scala.annotation.StaticAnnotation


case class Module(ds: Seq[(String, Term)]) extends Syntax

case class GlobalReference(str: String) extends Term {
  // returns if this term all reference is inside i, ie, if you give it 0, and the term is a r(0, 1), it return false
  override def closed(i: Int) = true
}

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

  override def closed(i: Int) = big < i
}



case class Fix(@BindingSite t: Seq[Term]) extends Term {
  assert(t.size == 1)

  override def closed(i: Int) = t.head.closed(i + 1)
}

case class Ascription(term: Term, ty: Term) extends Term {
  override def closed(i: Int) = term.closed(i) && ty.closed(i)
}


case class Lambda(@BindingSite is: Seq[Option[Term]], body: Term) extends Term {
  override def closed(i: Int) = is.forall(_.forall(_.closed(i + 1))) && body.closed(i + 1)
}
case class Pi(@BindingSite is: Seq[Term], to: Term) extends Term {
  override def closed(i: Int) = is.forall(_.closed(i + 1)) && to.closed(i + 1)
}
case class App(left: Term, right: Seq[Term]) extends Term {
  override def closed(i: Int) = left.closed(i) && right.forall(_.closed(i))
}

case class Let(@BindingSite vs: Seq[Term], body: Term) extends Term {
  override def closed(i: Int) = vs.forall(_.closed(i + 1)) && body.closed(i + 1)
}


case class Record(ms: Seq[String], ts: Seq[Term]) extends Term {
  assert(ms.size == ts.size)

  override def closed(i: Int) = ts.forall(_.closed(i))
}

// assume we have a acyclic directed graph, each node is labeled with a string, we can normalize it like this:
// first, list all labels with no dependencies, order them in label order,
// second list all labels with dependencies of the first batch, order them in label order
// ...
// we will always assume that our Sigma type is of this order...
case class Sigma(ms: Seq[String], @BindingSite ts: Seq[Term]) extends Term {
  assert(ms.size == ts.size)
  assert(normalized())
  def normalized(): Boolean = true // TODO

  override def closed(i: Int) = ts.forall(_.closed(i + 1))
}
case class Projection(left: Term, right: String) extends Term {
  override def closed(i: Int) = left.closed(i)
}


case class Sum(ts: Map[String, Term]) extends Term {
  override def closed(i: Int) = ts.values.forall(_.closed(i))
}
case class Construct(name: String, v: Term) extends Term {
  override def closed(i: Int) = v.closed(i)
}
case class Split(left: Term, @BindingSite right: Map[String, Term]) extends Term {
  override def closed(i: Int) = left.closed(i) && right.values.forall(_.closed(i + 1))
}



case class Universe() extends Term {
  override def closed(i: Int) = true
}

