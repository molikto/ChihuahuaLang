import UntypedLambdaCalculus.{Abs, App, Var}
import com.twitter.util.Eval
import sem.OpenReference

import scala.collection.mutable



object sem {

  // these are some normal forms, such that you need to apply to go on?
  // these are normal forms?
  sealed abstract class Value {
    def projection(s: String): Value = throw new Exception()
    def app(seq: Seq[Value]): Value = throw new Exception()
    def split(bs: Map[String, Value => Value]): Value = throw new Exception()
  }

  // these are where stuck state starts
  sealed abstract class Stuck extends Value {
    override def app(seq: Seq[Value]): Value = App(this, seq)
    override def projection(s: String) = Projection(this, s)
    override def split(bs: Map[String, Value => Value]) = Split(this, bs)
  }

  case class Fix(t: Seq[Value] => Value) extends Value {
    override def app(seq: Seq[Value]) = t(Seq(this)).app(seq)
    override def projection(s: String) = t(Seq(this)).projection(s)
    override def split(bs: Map[String, Value => Value]) = t(Seq(this)).split(bs)
  }

  case class OpenReference(depth: Int, small: Int) extends Stuck
  case class Projection(value: Stuck, str: String) extends Stuck
  case class App(atom: Stuck, app: Seq[Value]) extends Stuck
  case class Split(s: Stuck, names:  Map[String, Value => Value]) extends Stuck


  case class Lambda(size: Int, fun: Seq[Value] => Value) extends Value {
    override def app(seq: Seq[Value]) = fun(seq)
  }
  case class Construct(name: String, apps: Value) extends Value {
    override def split(bs: Map[String, Value => Value]) = bs(name)(apps)
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


  case class Global(svalue: Value, stype: Value, term: Term)
  val defs = mutable.Map.empty[String, Global] // defined global variables, and their normal form and type

  def global(str: String) = defs(str).svalue

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
  }

}

trait TypeCheck {

  import sem.Value

  def global(g: GlobalReference): Value = sem.global(g.str)

  /*
  case class Ctx(ctx: Seq[Seq[Value]]) {
    def el() = this.copy(ctx = Seq.empty +: ctx)
    def es(v: Value) = this.copy(ctx = (ctx.head :+ v) +: ctx.tail)

    def local(l: LocalReference): Value = ctx(l.big)(l.small)

    def infer(term: Term): Value = {
      term match {
        case g: GlobalReference => global(g)
        case l:LocalReference => local(l)
        case Fix(t) =>
          t match {
            case Ascription(a, b) =>
            case Lambda(is, Ascription(l, r)) =>
          }
        case Ascription(left, right) =>
          checkIsType(right)
          val r = eval(right)
          check(left, r)
          r
        case Lambda(is, body) =>
          assert(is.forall(a => a.nonEmpty))
          is.map(_.get).foldLeft(el()) { (c, p) =>
            c.checkIsType(p)
            eval(p)
          }
        case Pi(is, body) =>
        case App(l, rs) =>
          readback(infer(l)) match {
            case Pi(is, body) =>
              assert(rs.size == is.size)
              rs.zip(is).foldLeft(el()) { (c, p) =>
                val k = eval(p._2)
                c.check(p._1, k)

              }
          }
        case Record(ms, ts) =>
        case Sigma()
        case Projection()
        case Universe() =>
          sem.Universe()
      }
    }


    def checkIsType(t: Term): Unit = {
      null
    }

    def check(term: Term, ty: Value): Unit = {

    }
  }

  object Ctx {
    val Empty = Ctx(Seq.empty)
  }

  def check(f: Module): Unit = {
    f.ds.foreach(d => {
      val ty = Ctx.Empty.infer(d._2)
      defs += (d._1 -> (eval(d._2), ty))
    })
  }

  */

  def readback(v: Value, depth: Int = -1): Term = {
    val nd = depth + 1
    val ccc = -nd -1
    v match {
      case sem.Fix(f) =>
        Fix(Seq(readback(f(Seq(OpenReference(ccc, 0))), nd)))
      case sem.OpenReference(d, s) =>
        LocalReference(d + depth + 1, s)

      case l@sem.Lambda(size, f) =>
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
        Split(readback(s, depth), names.mapValues(v => readback(v(OpenReference(ccc, 0)),  nd)))

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
    }
  }

  val twitterEval = new Eval()

  // needs to ensure term is well typed first!
  def eval(term: Term): Value = {
    if (term == Universe()) sem.Universe()
    def emitScala(t: Term, depth: Int = -1): String = {
      t match {
        case GlobalReference(str) =>
          s"sem.global(${sem.names.register(str)})"
        case LocalReference(b, s) =>
          // the reason we use a global depth for the big index, is because
          // all free variables is inside the term
          // and all our structural recursive read back function ensures us
          // when reconstructing the term, the depth of binding site is stable when we construct them
          // and when the reference is constructed, the depth of the term is table, and so we can get
          // back the index
          if (b > depth) s"sem.LocalReference(${b - depth - 1}, $s)"
          else s"b${b}($s)"
        case Fix(t) =>
          val d = depth + 1
          s"sem.Fix(b$d => ${emitScala(t.head, d)})"
        case Ascription(left, right) => ???
          emitScala(left, depth)
        case Lambda(is, body) =>
          val d = depth + 1
          s"sem.Lambda(${is.size}, b$d => ${emitScala(body, d)})"
        case App(left, right) =>
          s"(${emitScala(left, depth)}).app(${right.map(r => emitScala(r, depth).mkString(", "))})"
        case Pi(vs, body) =>
          val d = depth + 1
          s"sem.Pi(${vs.size}, b$d => (Seq(${vs.map(r => emitScala(r, d).mkString(", "))}), ${emitScala(body, d)})"
        case Universe() => s"Universe()"
        case Let(vs, body) => ???
        case Record(ms, ts) =>
          s"sem.Record(Seq(${ms.map(a => sem.names.register(a, '@')).mkString(", ")}).map(lookup), Seq(${ts.map(a => emitScala(a, depth)).mkString(", ")}))"
        case Sigma(ms, vs) =>
          val d = depth + 1
          s"sem.Sigma(Seq(${ms.map(a => sem.names.register(a, '@')).mkString(", ")}).map(lookup), b$d => Seq(${vs.map(r => emitScala(r, d).mkString(", "))}))"
        case Projection(left, right) =>
          s"${emitScala(left, depth)}.projection(${sem.names.register(right, '@')})"
        case Sum(ts) =>
          s"sem.Sum(Map(${ts.map(p => "sem.names.lookup(" + sem.names.register(p._1, '#')+ ") -> " + emitScala(p._2, depth)).mkString(", ")}))"
        case Construct(name, t) =>
          s"sem.Construct(sem.names.lookup(${sem.names.register(name, '#')}), ${emitScala(t, depth)})"
        case Split(left, right) =>
          val d = depth + 1
          s"${emitScala(left, depth)}.split(Map(${right.map(p => "sem.names.lookup(" + sem.names.register(p._1, '#')+ ") -> (b" + d  + " => " + emitScala(p._2, depth + 1) + ")").mkString(", ")}))"
      }
    }
    val text = emitScala(term, -1)
    println(text)
    twitterEval.apply[Value](text)
  }
}

object tests extends scala.App with TypeCheck {

  val u = Universe()
  def r(b: Int, r: Int) = LocalReference(b, r)

  // \(x : type, y: x, z: x) => x
  val test1 = Lambda(Seq(u, r(0, 0), r(0, 0)).map(a => Some(a)), r(0, 0))
  // sem.Lambda(b0 => b0(0))
  val test
  eval(test1)
}
