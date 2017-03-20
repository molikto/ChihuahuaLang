sealed abstract class Syntax

sealed abstract class Term extends Syntax {

  /**
    * logic for closed, closed terms can be remembered for their types
    */
  private var _closed = Integer.MIN_VALUE // >= 0 means their is a open term in this thing...

  def closed0(): Int // return a integer, if it is >= 0, then this term is not open

  def closedRemember(): Int = {
    val k = closed0()
    _closed = k
    k
  }

  def closed(): Boolean = {
    if (_closed == Integer.MIN_VALUE) {
      closedRemember()
    }
    _closed < 0
  }
}


case class Module(ds: Seq[(String, Term)]) extends Syntax

case class GlobalReference(str: String) extends Term {
  // global reference is considered closed, because global reference is unique and constant after defined
  override def closed0(): Int = -1
}

// the big index is de Bruijn index, the small index is index in the binding sequence...
// for example
// def a_to_b_map = \(a: type, b: type, m: a -> b) => m(a)
// will be (type, type, 0.0 -> 0.1) 0.2(0.0)
// so if you view the Term AST as a tree, and each tree of type BindingSite increases the depth
// then to find the binding site from the reference, what you do is go up the tree, each time you
// go to a binding site you minus your reference by -1 or it is the site if you get 0
// all our binding site
case class LocalReference(big: Int) extends Term {
  assert(big >= 0)

  var debugGeneric: String = ""

  override def toString = if (debugGeneric.isEmpty) s"r($big)" else s"r($big, $debugGeneric)"

  // a local reference itself is never closed...
  override def closed0(): Int = big
}



case class Fix(t: Term) extends Term {
  override def closed0(): Int = t.closedRemember() - 1
}

case class Ascription(term: Term, ty: Term) extends Term {
  override def closed0(): Int = term.closedRemember() max ty.closedRemember() - 1
}


case class Lambda(is: Option[Term], body: Term) extends Term {
  override def closed0(): Int = is.map(_.closedRemember()).getOrElse(-1) max (body.closedRemember() - 1)
  override def toString = s"lam(${is.getOrElse("")}) => $body"

}
case class Pi(is: Term, to: Term) extends Term { // only the right is UNDER BINDING...
  override def closed0(): Int = is.closedRemember max (to.closedRemember() - 1)
  override def toString = s"pi($is => $to)"
}
case class App(left: Term, right: Term) extends Term {
  override def closed0(): Int = left.closedRemember() max right.closedRemember()
  override def toString = s"app($left, $right)"
}

//case class Let(vs: Seq[Term], body: Term) extends Term {
//  override def closed0(): Int = (vs.map(_.closedRemember()) :+ body.closedRemember()).max - 1
//}


case class Record(ms: Seq[String], ts: Seq[Term]) extends Term {
  assert(ms.size == ts.size)
  override def closed0(): Int = if (ts.isEmpty) -1 else ts.map(_.closedRemember()).max
  override def toString = s"record[${ms.zip(ts).map(a => a._1 + " @ " + a._2).mkString(", ")}]"
}

// assume we have a acyclic directed graph, each node is labeled with a string, we can normalize it like this:
// first, list all labels with no dependencies, order them in label order,
// second list all labels with dependencies of the first batch, order them in label order
// ...
// we will always assume that our Sigma type is of this order...
case class Sigma(ms: Seq[String], ts: Seq[Term]) extends Term { // so the left most variable IS NOT closed
  assert(ms.size == ts.size)
  assert(ms.toSet.size == ms.size)
  assert(normalized())
  def normalized(): Boolean = true // TODO
  override def closed0(): Int = if (ts.isEmpty) -1 else ts.zipWithIndex.map(p => p._1.closedRemember() - p._2).max
  override def toString = s"sigma[${ms.zip(ts).map(a => a._1 + " @ " + a._2).mkString(", ")}]"
}
case class Projection(left: Term, right: String) extends Term {
  override def closed0(): Int = left.closedRemember()
}


case class Sum(ts: Map[String, Term]) extends Term {
  override def closed0(): Int = if (ts.isEmpty) -1 else  ts.values.map(_.closedRemember()).max - 1
  override def toString = s"sum{${ts.map(a => a._1 + " # " + a._2).mkString(", ")}}"
}
case class Construct(name: String, v: Term) extends Term {
  override def closed0(): Int = v.closedRemember()
  override def toString = s"#($name $v)"
}
case class Split(left: Term, right: Map[String, Term]) extends Term {
  override def closed0(): Int = (left.closedRemember() +: right.values.map(_.closedRemember() - 1).toSeq).max
}

case class Equality(left: Term, right: Term) extends Term {
  override def closed0(): Int = left.closedRemember() max right.closedRemember()
}

case class Refl() extends Term {
  override def closed0(): Int = -1
}



case class Universe() extends Term {
  override def closed0(): Int = -1
  override def toString = "u"
}

