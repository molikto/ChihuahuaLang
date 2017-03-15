import com.twitter.util.Eval
import org.snailya.mygame.UtilsCommon
import sem.OpenReference

import scala.collection.mutable



// the semantic world, it is not a trait but a object because it is easier to dynamic
// link the JIT'ed code now
//
// I know Coq's native_compute will compile to OCaml
// https://github.com/coq/coq/blob/d02c9c566c58e566a1453827038f2b49b695c0a5/kernel/nativelib.ml#L78
// https://github.com/coq/coq/blob/trunk/kernel/nativecode.ml#L1597
// but I don't know the sharing of global definitions and runtime etc. is possible
object sem {

  // these are some normal forms, such that you need to apply to go on?
  // these are normal forms?
  sealed abstract class Value {
    def :<:(o: Value): Boolean = {
    }
    def projection(s: String): Value = throw new Exception()
    def app(seq: Seq[Value]): Value = throw new Exception()
    def split(bs: Map[String, Seq[Value] => Value]): Value = throw new Exception()
  }



  def joint(seq: Seq[Value]): Value = {

  }
  def meet(seq: Seq[Value]): Value = {

  }


  case class DebugPoison() extends Value

  // these are where stuck state starts
  sealed abstract class Stuck extends Value {
    override def app(seq: Seq[Value]): Value = App(this, seq)
    override def projection(s: String) = Projection(this, s)
    override def split(bs: Map[String, Seq[Value] => Value]) = Split(this, bs)
  }

  case class Fix(t: Seq[Value] => Value) extends Value {
    override def app(seq: Seq[Value]) = t(Seq(this)).app(seq)
    override def projection(s: String) = t(Seq(this)).projection(s)
    override def split(bs: Map[String, Seq[Value] => Value]) = t(Seq(this)).split(bs)
  }

  // global reference is kind of like fix, they are "wrapped" so that we don't expand all things
  // referenced by it when reading back
  // this also means that global reference is NOT reduced inside lambda
  // so it plays a role in syntax equality
  case class GlobalReference(name: String) extends Value {
    override def app(seq: Seq[Value]) = global(name).svalue.app(seq)
    override def projection(s: String) = global(name).svalue.projection(s)
    override def split(bs: Map[String, Seq[Value] => Value]) = global(name).svalue.split(bs)
  }

  case class OpenReference(depth: Int, small: Int) extends Stuck
  case class Projection(value: Stuck, str: String) extends Stuck
  case class App(atom: Stuck, app: Seq[Value]) extends Stuck
  case class Split(s: Stuck, names:  Map[String, Seq[Value] => Value]) extends Stuck


  case class Lambda(size: Int, fun: Seq[Value] => Value) extends Value {
    override def app(seq: Seq[Value]) = fun(seq)
  }
  case class Construct(name: String, apps: Value) extends Value {
    override def split(bs: Map[String, Seq[Value] => Value]) = bs(name)(Seq(apps))
  }
  case class Record(ms: Seq[String], vs: Seq[Value]) extends Value {
    override def projection(s: String) = vs(ms.indexOf(s))
  }

  case class Universe() extends Value
  // this function is the function such that when applied, return the types of parameters
  // and the type of return type, if you apply it to real types, you get real values back
  // if you apply it to Accumulators, and read back, you get the Pi term in normal form
  case class Sigma(ms: Seq[String], ts: Seq[Value] => Seq[Value]) extends Value
  case class Pi(size: Int, inside: Seq[Value] => (Seq[Value], Value)) extends Value
  case class Sum(ts: Map[String, Value]) extends Value


  case class Global(svalue: Value, stype: Value)
  val defs = mutable.Map.empty[String, Global] // defined global variables, and their normal form and type

  def global(str: String) = defs(str)

  object names {
    // names across borders.... the ty currently is a hack
    val _ns = mutable.Map.empty[String, Int]
    val ns = mutable.Map.empty[Int, String]

    var counter = 0

    def lookup(n: Int): String = ns(n).substring(1)
    def register(s: String, ty: Char = ' '): Int = {
      val key = ty + s
      val old = _ns.get(key)
      if (old.isEmpty) {
        val c = counter + 1
        counter = c
        _ns.put(key, c)
        ns.put(c, key)
        c
      } else old.get
    }

    def emitScala(s: String, ty: Char = ' ') = s"sem.names.lookup(${register(s, ty)})"
  }


}

import sem.Value



trait Normalization extends UtilsCommon {

  def readback(v: Value, depth: Int = -1): Term = {
    val nd = depth + 1
    val ccc = -nd - 1
    v match {
      case sem.GlobalReference(n) =>
        GlobalReference(n)
      case sem.Fix(f) =>
        Fix(Seq(readback(f(Seq(OpenReference(ccc, 0))), nd)))
      case sem.OpenReference(d, s) =>
        LocalReference(d + depth + 1, s)

      case l@sem.Lambda(size, _) =>
        val ps = (0 until size).map(a => OpenReference(ccc, a))
        Lambda((0 until size).map(_ => None), readback(l.app(ps), nd))
      case sem.Construct(name, value) =>
        Construct(name, readback(value, depth))
      case sem.Record(ms, vs) =>
        Record(ms, vs.map(v => readback(v, depth)))

      case sem.App(left, vs) =>
        App(readback(left, depth), vs.map(a => readback(a, depth)))
      case sem.Projection(vv, s) =>
        Projection(readback(vv, depth), s)
      case sem.Split(s, names) =>
        Split(readback(s, depth), names.mapValues(v => readback(v(Seq(OpenReference(ccc, 0))), nd)))

      case sem.Pi(size, inside) =>
        val ps = (0 until size).map(a => OpenReference(ccc, a))
        val ts = inside(ps)
        Pi(ts._1.map(a => readback(a, nd)), readback(ts._2, nd))
      case sem.Sigma(ms, ts) =>
        val ps = ms.indices.map(a => OpenReference(ccc, a))
        Sigma(ms, ts(ps).map(a => readback(a, nd)))
      case sem.Sum(ts) =>
        Sum(ts.mapValues(c => readback(c, depth)))
      case sem.Universe() => Universe()
      case sem.DebugPoison() => throw new Exception("I am poisoned!")
    }
  }

  val twitterEval = new Eval()

  // needs to ensure term is well typed first!
  def eval(term: Term, debugText: Boolean = false): Value = {
    def emitScala(t: Term, depth: Int): String = {
      t match {
        case GlobalReference(str) =>
          s"sem.GlobalReference(${sem.names.emitScala(str)})"
        case LocalReference(b, s) =>
          // the reason we use a global depth for the big index, is because
          // all free variables is inside the term
          // and all our structural recursive read back function ensures us
          // when reconstructing the term, the depth of binding site is stable when we construct them
          // and when the reference is constructed, the depth of the term is table, and so we can get
          // back the index
          if (b > depth) s"sem.OpenReference(${b - depth - 1}, $s)"
          else s"b${depth - b}($s)"
        case Fix(t) =>
          val d = depth + 1
          s"sem.Fix(b$d => ${emitScala(t.head, d)})"
        case Ascription(left, right) =>
          emitScala(left, depth)
        case Lambda(is, body) =>
          val d = depth + 1
          s"sem.Lambda(${is.size}, b$d => ${emitScala(body, d)})"
        case App(left, right) =>
          s"${emitScala(left, depth)}.app(Seq(${right.map(r => emitScala(r, depth)).mkString(", ")}))"
        case Pi(vs, body) =>
          val d = depth + 1
          s"sem.Pi(${vs.size}, b$d => (Seq(${vs.map(r => emitScala(r, d)).mkString(", ")}), ${emitScala(body, d)}))"
        case Universe() => s"sem.Universe()"
        case Let(vs, body) => ???
        case Record(ms, ts) =>
          s"sem.Record(Seq(${ms.map(a => sem.names.emitScala(a, '@')).mkString(", ")}), Seq(${ts.map(a => emitScala(a, depth)).mkString(", ")}))"
        case Sigma(ms, vs) =>
          val d = depth + 1
          s"sem.Sigma(Seq(${ms.map(a => sem.names.emitScala(a, '@')).mkString(", ")}), b$d => Seq(${vs.map(r => emitScala(r, d)).mkString(", ")}))"
        case Projection(left, right) =>
          s"${emitScala(left, depth)}.projection(${sem.names.emitScala(right, '@')})"
        case Sum(ts) =>
          s"sem.Sum(Map(${ts.map(p =>  sem.names.emitScala(p._1, '#') + " -> " + emitScala(p._2, depth)).mkString(", ")}))"
        case Construct(name, t) =>
          s"sem.Construct(${sem.names.emitScala(name, '#')}, ${emitScala(t, depth)})"
        case Split(left, right) =>
          val d = depth + 1
          s"${emitScala(left, depth)}.split(Map(${right.map(p => sem.names.emitScala(p._1, '#') + " -> (b" + d  + " => " + emitScala(p._2, d) + ")").mkString(", ")}))"
      }
    }
    term match {
      case Universe() => sem.Universe()
      case GlobalReference(str) => sem.GlobalReference(str)
      case _ =>
        val text = emitScala(term, -1)
        if (debugText) delog("\t" + text)
        twitterEval.apply[Value](text)
    }
  }


  def nbe(t: Term) = {
    delog("NbE: " + t)
    val e = eval(t, debugText = true)
    val rb = readback(e)
    delog("\t" + rb)
    rb
  }


  def force(v: Value): Value = {
    def loop(v: Value): Value = v match {
      case sem.GlobalReference(str) => sem.global(str).svalue
      case f@sem.Fix(k) => k(Seq(f))
    }
    var t = v
    while (t == v) t = loop(t)
    t
  }
}



trait TypeCheck extends Normalization {


  def global(g: GlobalReference) = sem.global(g.str)

  // local typing context
  // the context is so that the head is index 0
  case class Context(ctx: Seq[Seq[Value]]) {

    def head = ctx.head
    // new index
    def el() = this.copy(ctx = Seq.empty +: ctx)

    def el(s: Seq[Value]) = this.copy(ctx = s +: ctx)

    // new small index
    def es(v: Value) = this.copy(ctx = (ctx.head :+ v) +: ctx.tail)

    def local(l: LocalReference): Value = ctx(l.big)(l.small)
    // return the type of a term in semantics world
    def infer(term: Term, debugCheck: Boolean = true): Value = {
      def checkLambdaArgs(is: Seq[Option[Term]]): Context = {
        assert(is.forall(a => a.nonEmpty))
        is.map(_.get).foldLeft(el()) { (c, p) =>
          c.checkIsType(p)
          c.es(eval(p))
        }
      }
      val res = term match {
        case g: GlobalReference => global(g).stype
        case l: LocalReference => local(l)
        case Fix(t) =>
          t.head match {
            case Ascription(tt, ty) =>
              checkIsType(ty)
              el().es(eval(ty)).infer(tt)
            case Lambda(is, Ascription(tt, ty)) =>
              val c = checkLambdaArgs(is)
              c.checkIsType(ty)
              val vty = eval(ty)
              c.check(tt, eval(ty))
              eval(Pi(c.head.map(a => readback(a)), readback(vty)))
            case _ => throw new Exception("Cannot infer Fix")
          }
        case Ascription(left, right) =>
          checkIsType(right)
          val r = eval(right)
          check(left, r)
          r

        case Lambda(is, body) =>
          val c = checkLambdaArgs(is)
          val v = c.infer(body)
          // the readback might contains sem.OpenReference,
          // but after we eval with a Pi
          // it can happens that the binding will rebind inside the eval
          eval(Pi(c.head.map(a => readback(a)), readback(v)))
        case App(l, rs) =>
          force(infer(l)) match {
            case sem.Pi(size, inside) =>
              var i = 0
              var confirmed = Seq.empty[Value]
              while (i < size) { // check i-th type
                confirmed ++ (i until size).map(_ => sem.DebugPoison())
                val applied = inside(confirmed)
                val expected = applied._1(i) // it should NOT contain any new open variables...
                if (Debug) {
                  readback(expected)
                }
                check(rs(i), expected)
                confirmed = confirmed :+ expected
                i += 1
              }
              inside(confirmed)._2
            case _ => throw new Exception("Cannot infer App")
          }
        case Let(vs, body) => ???

        case Record(ms, ts) =>
          val vs = ts.map(a => infer(a))
          sem.Sigma(ms, _ => vs)
        case Projection(left, right) =>
          force(infer(left)) match {
            case sem.Sigma(ms, ts) => // the thing is this type MUST be concrete
              val index = ms.indexOf(right)
              val res = ts(ms.map(a => eval(Projection(left, a))))
              res(index)
            case _ => throw new Exception("Cannot infer Projection")
          }
        case Construct(name, v) =>
          sem.Sum(Map(name -> infer(v)))
        case Split(left, right) =>
          force(infer(left)) match {
            case sem.Sum(ts) => // right is bigger
              assert((ts.keySet -- right.keySet).isEmpty)
              sem.meet(ts.toSeq.map(a => {
                val at = a._2
                val term = right(a._1)
                el().es(at).infer(term)
              }))
            case _ => throw new Exception("Cannot infer Split")
          }
        case a: Pi =>
          sem.Universe()
        case a: Sum =>
          checkIsType(a)
          sem.Universe()
        case a: Sigma =>
          checkIsType(a)
          sem.Universe()
        case Universe() =>
          sem.Universe()
      }
      delog("Infer. Context:\n\t" + ctx.reverse.map(a => a.map(k => readback(k)).mkString(" __ ")).mkString("\n\t") + "\nTerm:\n\t" + term + "\nType:\n\t" + readback(res))
      if (debugCheck && Debug) {
        check(term, res)
      }
      res
    }


    // this can be considered just a wrapper for infer(term) :<: t
    // only that: it calls force
    // it handles parameter less lambda
    // so ALWAYS call this instead of :<: directly
    // (unless you know what you are doing)
    // and if you look at the code above,
    // you can see that the check is basically used by
    // function application.....
    // and fix..................
    def check(term: Term, ty: Value): Unit = {
      (term, force(ty)) match {
        case (Lambda(is, body), sem.Pi(size, inside)) =>
          assert(size == is.size)
          if (is.forall(_.nonEmpty)) {
            assert(infer(term, debugCheck = false) :<: ty)
          } else if (is.forall(_.isEmpty)) {
            val t = inside(is.indices.map(a => OpenReference(0, a)))
            el(t._1).check(body, t._2)
          } else {
            throw new Exception(".. this is not implemented yet")
          }
        case (e, t) =>
          assert(infer(e) :<: t)
      }
      delog("Check. Context:\n\t" + ctx.reverse.map(a => a.map(k => readback(k)).mkString(" __ ")).mkString("\n\t") + "\nTerm:\n\t" + term + "\nType:\n\t" + readback(ty))
    }

    def checkIsUniverse(t: Term) = t match {
      case Universe() => Unit
      case _ => throw new Exception("Check is universe failed")
    }

    // no need to go inside check for now
    def checkIsType(t: Term): Unit = assert(infer(t) :<: sem.Universe())
  }

  object Context {
    val Empty = Context(Seq.empty)
  }

  def check(f: Module): Unit = {
    f.ds.foreach(d => {
      val ty = Context.Empty.infer(d._2)
      sem.defs += (d._1 -> sem.Global(eval(d._2), ty))
    })
  }



}

object tests extends scala.App with TypeCheck {

  val u = Universe()
  def r(b: Int, r: Int) = LocalReference(b, r) // reference
  def pns(i: Int) = (0 until i).map(_ => None) // empty parameters
  def ps(t: Term*) = t.map(a => Some(a)) // parameters
  def a(t: Term, ts: Term*) = App(t, ts) // app
  def pi(t: Term*) = Pi(t.dropRight(1), t.last)
  def lam(t: Term*) = Lambda(ps(t.dropRight(1): _*), t.last)
  def tps(t: Term) = t match { // trim parameters
    case l: Lambda =>
      l.copy(is = l.is.map(_ => None))
    case f@Fix(a) => a.head match {
      case l: Lambda =>
        Fix(Seq(l.copy(is = l.is.map(_ => None))))
      case _ => f
    }
    case a => a
  }
  def fix(t: Term) = Fix(Seq(t))
  def abort() = if (2 + 1 == 3) throw new Exception("Abort mission!")

  // \(x : type, y: x, z: x) => x
  val t1 = lam(u, r(0, 0), r(0, 0), r(0, 0))
  assert(nbe(t1) == tps(t1))

  // record[]
  val unit = Sigma(Seq.empty, Seq.empty)
  assert(nbe(unit) == unit)

  val unit0 = Record(Seq.empty, Seq.empty)
  assert(nbe(unit0) == unit0)

  // \(a: type, x: a) => x
  val id = lam(u, r(0, 0), r(0, 1))
  assert(nbe(id) == tps(id))
  assert(nbe(a(id, u, unit)) == unit)


  val type_id = pi(u, r(0, 0), r(0, 0))
  assert(nbe(type_id) == type_id)

  assert(readback(Context.Empty.infer(id)) == type_id)


  // \(a: type) => (x: a) => x
  val idc = lam(u, lam(r(1, 0), r(0, 0)))
  assert(nbe(a(id, u, unit)) == nbe(a(a(idc, u), unit)))

  val type_idc = pi(u, pi(r(1, 0), r(1, 0)))
  assert(nbe(type_idc) == type_idc)

  assert(readback(Context.Empty.infer(idc)) == type_idc)

  abort()

  val id_u = Lambda(ps(u), r(0, 0))
  val app_id_u = Lambda(ps(u), a(id, u, r(0, 0)))
  assert(nbe(id_u) == nbe(app_id_u))

  // \(x: type, f: x -> x, a: x) => f (f a)
  val double = Lambda(ps(u, pi(r(1, 0), r(1, 0)), r(0, 0)), a(r(0, 1), a(r(0, 1), r(0, 2))))
  assert(nbe(double) == tps(double))
  //assert(a(double, unit, ))

  // fix self => sum(zero: unit, succ: self)
  val num = fix(Sum(Map("zero" -> unit, "succ" -> r(0, 0))))

  val n0 = Construct("zero", unit0)
  val n1 = Construct("succ", n0)
  val n2 = Construct("succ", n1)
  val n3 = Construct("succ", n2)
  val n4 = Construct("succ", n3)
  val n5 = Construct("succ", n4)
  val n6 = Construct("succ", n5)
  val n7 = Construct("succ", n6)
  val n8 = Construct("succ", n7)
  val n9 = Construct("succ", n8)
  val n10 = Construct("succ", n9)
  val n11 = Construct("succ", n10)
  val n12 = Construct("succ", n11)
  val n13 = Construct("succ", n12)
  val n14 = Construct("succ", n13)
  val n15 = Construct("succ", n14)
  val n16 = Construct("succ", n15)

  val n0t16 = Seq(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)
  val succ = Lambda(ps(num), Construct("succ", r(0, 0)))

  assert((n0t16 ++ Seq(succ)).forall(a => nbe(a) == tps(a)))

  assert(nbe(a(succ, n1)) == n2)
  assert(nbe(a(succ, n2)) == n3)
  assert(nbe(a(succ, n3)) == n4)
  assert(nbe(a(succ, n4)) == n5)
  assert(nbe(a(succ, n5)) == n6)
  assert(nbe(a(succ, a(succ, n1))) == n3)
  assert(nbe(a(succ, a(succ, n2))) == n4)
  assert(nbe(a(succ, a(succ, n3))) == n5)
  assert(nbe(a(succ, a(succ, n4))) == n6)

  val pair = Lambda(ps(u, u), Sigma(Seq("_1", "_2"), Seq(r(1, 0), r(1, 1))))
  val pair_num_num = Sigma(Seq("_1", "_2"), Seq(num, num))
  assert(nbe(pair) == tps(pair))
  def mk_pair(seq: Term*) = Record(Seq("_1", "_2"), seq)
  assert(nbe(a(pair, num, num)) == pair_num_num)

  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self k b) }
  val plus = fix(Lambda(ps(num, num), Split(r(0, 0), Map("zero" -> r(1, 1), "succ" -> Construct("succ", a(r(2, 0), r(0, 0), r(1, 1)))))))
  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self b k) }
  val plus1 = fix(Lambda(ps(num, num), Split(r(0, 0), Map("zero" -> r(1, 1), "succ" -> Construct("succ", a(r(2, 0), r(1, 1), r(0, 0)))))))
  // fix self => (b, a: nat) => split a { case zero => b; case succ k => succ(self k b) }
  val plus2 = fix(Lambda(ps(num, num), Split(r(0, 1), Map("zero" -> r(1, 0), "succ" -> Construct("succ", a(r(2, 0), r(0, 0), r(1, 0)))))))
  // fix self => (b, a: nat) => split a { case zero => b; case succ k => succ(self b k) }
  val plus3 = fix(Lambda(ps(num, num), Split(r(0, 1), Map("zero" -> r(1, 0), "succ" -> Construct("succ", a(r(2, 0), r(1, 0), r(0, 0)))))))
  assert(nbe(plus) == tps(plus))

  for (i <- 0 to 16) {
    for (j <- 0 to 16) {
      if (i + j <= 16) {
        assert(nbe(a(plus, n0t16(i), n0t16(j))) == n0t16(i + j))
        assert(nbe(a(plus1, n0t16(i), n0t16(j))) == n0t16(i + j))
        assert(nbe(a(plus2, n0t16(i), n0t16(j))) == n0t16(i + j))
        assert(nbe(a(plus3, n0t16(i), n0t16(j))) == n0t16(i + j))
      }
    }
  }

  // fix self => (a, b: nat) => split a { case zero => 0; case succ k => (plus b (self k b) }
  val mult = fix(Lambda(ps(num, num), Split(r(0, 0), Map("zero" -> n0, "succ" -> a(plus, r(1, 1), a(r(2, 0), r(0, 0), r(1, 1)))))))
  val mult1 = fix(Lambda(ps(num, num), Split(r(0, 0), Map("zero" -> n0, "succ" -> a(plus, r(1, 1), a(r(2, 0), r(1, 1), r(0, 0)))))))

  for (i <- 0 to 16) {
    for (j <- 0 to 16) {
      if (i * j <= 16) {
        assert(nbe(a(mult, n0t16(i), n0t16(j))) == n0t16(i * j))
        assert(nbe(a(mult1, n0t16(i), n0t16(j))) == n0t16(i * j))
      }
    }
  }

}
