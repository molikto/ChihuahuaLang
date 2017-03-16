import com.twitter.util.Eval
import org.snailya.mygame.UtilsCommon

import scala.collection.mutable
import scala.util.{Failure, Success, Try}



// the semantic world, it is not a trait but a object because it is easier to dynamic
// link the JIT'ed code now
//
// I know Coq's native_compute will compile to OCaml
// https://github.com/coq/coq/blob/d02c9c566c58e566a1453827038f2b49b695c0a5/kernel/nativelib.ml#L78
// https://github.com/coq/coq/blob/trunk/kernel/nativecode.ml#L1597
// but I don't know the sharing of global definitions and runtime etc. is possible
object sem {

  val Bottom = Sum(Map.empty)
  // these are some normal forms, such that you need to apply to go on?
  // these are normal forms?
  var i = 0
  def newUniqueName(): String = {
    i += 1
    i.toString
  }

  sealed abstract class Value {

    def subtypeOf(o: Value): Boolean = {
      if (this == o) {
        return true
      }
      (this, o) match {
        case (_, sem.Bottom) =>
          false
        case (sem.Bottom, _) =>
          true
        case (sem.Universe(), sem.Universe()) =>
          true
        case (sem.Fix(t0), sem.Fix(t1)) =>
          val k = Seq(Generic())
          t0(k) subtypeOf t1(k)
        case (sem.Fix(t), a) =>
          val k = Seq(this)
          t(k) subtypeOf a
        case (a, sem.Fix(t)) =>
          val k = Seq(o)
          a subtypeOf t(k)
        case (GlobalReference(g1), GlobalReference(g2)) =>
          if (g1 == g2) true
          else sem.global(g1).svalue subtypeOf sem.global(g2).svalue
        case (GlobalReference(g1), g2) =>
          sem.global(g1).svalue subtypeOf g2
        case (g1, GlobalReference(g2)) =>
          g1 subtypeOf sem.global(g2).svalue
        case (sem.Sigma(ms0, ts0), sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]
          if (ms1.startsWith(ms0)) { // TODO advanced subtyping for records
          val ids = ms1.indices.map(i => Generic())
            val tts0 = ts0(ids)
            val tts1 = ts1(ids)
            tts0.zip(tts1).forall(p => p._1 subtypeOf p._2)
          } else false
        case (sem.Pi(size, vs), sem.Pi(size1, vs1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
          if (size == size1) {
            val ids = (0 until size).map(i => Generic())
            val tts0 = vs(ids)
            val tts1 = vs1(ids)
            tts0._1.zip(tts1._1).forall(pair => pair._2 subtypeOf pair._1) && (tts0._2 subtypeOf tts1._2)
          } else false
        case (sem.Sum(ts), sem.Sum(ts1)) => // assuming nat <: integer, we have sum[#a nat] <: sum[#a integer, #b type]
          if ((ts.keySet -- ts1.keySet).isEmpty) {
            ts.forall(pair => pair._2 subtypeOf ts1(pair._1))
          } else false
        case _ => false
      }
    }

    // meet
    // c = a /\ b
    // then c <: a and c <: b
    def :/\:(o: Value): Value = {
      if (this == o) {
        this
      } else {
        (this, o) match {
          case (sem.Bottom, _) =>
            sem.Bottom
          case (_, sem.Bottom) =>
            sem.Bottom
          case (sem.Universe(), sem.Universe()) =>
            sem.Universe()
          case (sem.Fix(t0), sem.Fix(t1)) =>
            val k = Seq(Generic())
            t0(k) :/\: t1(k)
          case (sem.Fix(t), a) =>
            val k = Seq(this)
            t(k) :/\: a
          case (a, sem.Fix(t)) =>
            val k = Seq(o)
            a :/\: t(k)
          case (s0@sem.Sigma(ms0, ts0), s1@sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]
            if (s0 subtypeOf s1) { // TODO bad
              s0
            } else if (s1 subtypeOf s0) {
              s1
            } else {
              Bottom
            }
          case (p0@sem.Pi(size, vs), p1@sem.Pi(size1, vs1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
            if (size == size1) {
//              val ids = (0 until size).map(i => OpenReference(0, i))
//              val tts0 = vs(ids)
//              val tts1 = vs1(ids)
//              sem.Pi(size, tts0._1.zip(tts1._1).map(p => p._1 :\/: p._2)
              if (p0 subtypeOf p1) { // TODO bad
                p0
              } else if (p1 subtypeOf p0) {
                p1
              } else {
                Bottom
              }
            } else {
              Bottom
            }
          case (sem.Sum(ts), sem.Sum(ts1)) => // assuming nat <: integer, we have sum[#a nat] <: sum[#a integer, #b type]
            val keys = ts.keySet intersect ts1.keySet
            sem.Sum(keys.map(a => (a, ts(a) :/\: ts1(a))).toMap)
          case _ => Bottom
        }
      }
    }

    // join
    def :\/:(o: Value): Value = {
      if (this == o) {
        this
      } else {
        (this, o) match {
          case (sem.Bottom, a) =>
            a
          case (a, sem.Bottom) =>
            a
          case (sem.Universe(), sem.Universe()) =>
            sem.Universe()
          case (sem.Fix(t0), sem.Fix(t1)) =>
            val k = Seq(Generic())
            t0(k) :\/: t1(k)
          case (sem.Fix(t), a) =>
            val k = Seq(this)
            t(k) :\/: a
          case (a, sem.Fix(t)) =>
            val k = Seq(o)
            a :\/: t(k)
          case (s0@sem.Sigma(ms0, ts0), s1@sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]
            if (s0 subtypeOf s1) { // TODO bad
              s1
            } else if (s1 subtypeOf s0) {
              s0
            } else {
              Bottom
            }
          case (p0@sem.Pi(size, vs), p1@sem.Pi(size1, vs1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
            if (size == size1) {
              //              val ids = (0 until size).map(i => OpenReference(0, i))
              //              val tts0 = vs(ids)
              //              val tts1 = vs1(ids)
              //              sem.Pi(size, tts0._1.zip(tts1._1).map(p => p._1 :\/: p._2)
              if (p0 subtypeOf p1) { // TODO bad
                p1
              } else if (p1 subtypeOf p0) {
                p0
              } else {
                Bottom
              }
            } else {
              Bottom
            }
          case (sem.Sum(ts), sem.Sum(ts1)) => // assuming nat <: integer, we have sum[#a nat] <: sum[#a integer, #b type]
            val keys = ts.keySet union ts1.keySet
            sem.Sum(keys.map(a => (a, ts.getOrElse(a, Bottom) :\/: ts1.getOrElse(a, Bottom))).toMap)
          case _ => Bottom
        }
      }
    }
    def projection(s: String): Value = throw new Exception()
    def app(seq: Seq[Value]): Value = throw new Exception()
    def split(bs: Map[String, Seq[Value] => Value]): Value = throw new Exception()
  }



  def join(seq: Seq[Value]): Value = {
    assert(seq.nonEmpty)
    seq.tail.fold(seq.head) { (v0, v1) => v0 :\/: v1}
  }
  def meet(seq: Seq[Value]): Value = {
    assert(seq.nonEmpty)
    seq.tail.fold(seq.head) { (v0, v1) => v0 :/\: v1}
  }


  abstract class Special extends Value
  abstract class Lazy extends Value

  case class DebugPoison() extends Special


  // these are where stuck state starts
  sealed abstract class Stuck extends Value {
    override def app(seq: Seq[Value]): Value = App(this, seq)
    override def projection(s: String) = Projection(this, s)
    override def split(bs: Map[String, Seq[Value] => Value]) = Split(this, bs)
  }


  case class Fix(t: Seq[Value] => Value) extends Lazy {
    override def app(seq: Seq[Value]) = t(Seq(this)).app(seq)
    override def projection(s: String) = t(Seq(this)).projection(s)
    override def split(bs: Map[String, Seq[Value] => Value]) = t(Seq(this)).split(bs)
  }


  // global reference is kind of like fix, they are "wrapped" so that we don't expand all things
  // referenced by it when reading back
  // this also means that global reference is NOT reduced inside lambda
  // so it plays a role in syntax equality
  case class GlobalReference(name: String) extends Lazy {
    override def app(seq: Seq[Value]) = global(name).svalue.app(seq)
    override def projection(s: String) = global(name).svalue.projection(s)
    override def split(bs: Map[String, Seq[Value] => Value]) = global(name).svalue.split(bs)
  }


  // only used when testing things is equal INSIDE the semantical world, like in subtype relationship
  case class Generic(name: String = newUniqueName()) extends Stuck // this references the things in our env
  case class OpenReference(depth: Int, small: Int) extends Stuck // only used in read back
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
  def addGlobal(str: String, global: Global) = {
    if (defs.contains(str)) {
      throw new Exception("Not allowed, global have same name")
    } else {
      defs += str -> global
    }
  }


  def force(v: Value): Value = {
    def loop(v: Value): Value = v match {
      case sem.GlobalReference(str) => sem.global(str).svalue
      case f@sem.Fix(k) => k(Seq(f))
      case a => a
    }
    var p = v
    var n = loop(p)
    while (n != p) {
      p = n
      n = loop(p)
    }
    n
  }

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

  def readback(v: Value, ctx: Seq[Seq[(String, Value)]]): Term = {
    def rec(v: Value, depth: Int): Term = {
      val nd = depth + 1
      val ccc = -nd - 1
      v match {
        case sem.GlobalReference(n) =>
          GlobalReference(n)
        case sem.Generic(g) =>
          for (i <- ctx.indices) {
            val cs = ctx(i)
            for (j <- cs.indices) {
              val c = cs(j)
              if (c._1 == g) {
                // when depth = -1
                // and we read a generic at i = 0
                // we should give it 0
                // when depth = 0
                // we give it and we read a generic at i = 0, we should give it 1
                val local = LocalReference(depth + 1 + i, j)
                local.debugGeneric = g
                return local
              }
            }
          }
          throw new Exception("")
        case sem.Fix(f) =>
          Fix(Seq(rec(f(Seq(sem.OpenReference(ccc, 0))), nd)))
        case sem.OpenReference(d, s) => // we don't want truely open references, all local reference must be from a Generic
          assert(d < 0)
          LocalReference(d + depth + 1, s)
        case l@sem.Lambda(size, _) =>
          val ps = (0 until size).map(a => sem.OpenReference(ccc, a))
          Lambda((0 until size).map(_ => None), rec(l.app(ps), nd))
        case sem.Construct(name, value) =>
          Construct(name, rec(value, depth))
        case sem.Record(ms, vs) =>
          Record(ms, vs.map(v => rec(v, depth)))

        case sem.App(left, vs) =>
          App(rec(left, depth), vs.map(a => rec(a, depth)))
        case sem.Projection(vv, s) =>
          Projection(rec(vv, depth), s)
        case sem.Split(s, names) =>
          Split(rec(s, depth), names.mapValues(v => rec(v(Seq(sem.OpenReference(ccc, 0))), nd)))

        case sem.Pi(size, inside) =>
          val ps = (0 until size).map(a => sem.OpenReference(ccc, a))
          val ts = inside(ps)
          Pi(ts._1.map(a => rec(a, nd)), rec(ts._2, nd))
        case sem.Sigma(ms, ts) =>
          val ps = ms.indices.map(a => sem.OpenReference(ccc, a))
          Sigma(ms, ts(ps).map(a => rec(a, nd)))
        case sem.Sum(ts) =>
          Sum(ts.mapValues(c => rec(c, depth)))
        case sem.Universe() => Universe()
        case sem.DebugPoison() => throw new Exception("I am poisoned!")
      }
    }
    rec(v, -1)
  }


  // needs to ensure term is well typed first!
  def eval(term: Term, ctx: Seq[Seq[(String, Value)]], debugText: Boolean = false): Value = {
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
          if (b > depth) s"sem.Generic(${sem.names.emitScala(ctx(b - depth - 1)(s)._1, '$')})"
          //if (b > depth) s"sem.OpenReference(${b - depth - 1}, $s)"
          else s"b${depth - b}($s)"
//        case Generic(a) =>
//          s"sem.Generic(${sem.names.emitScala(a, '$')})"
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
      case LocalReference(b, s) =>
        sem.Generic(ctx(b)(s)._1)
      case Universe() => sem.Universe()
      case GlobalReference(str) => sem.GlobalReference(str)
      case _ =>
        val text = emitScala(term, -1)
        if (DebugNbe && debugText) delog("\t" + text)
        val twitterEval = new Eval()
        twitterEval.apply[Value](text)
    }
  }

  val DebugNbe = Debug && false


  def nbe(t: Term) = {
    if (DebugNbe) delog("NbE: " + t)
    val e = eval(t, Seq.empty, debugText = true)
    val rb = readback(e, Seq.empty)
    if (DebugNbe) delog("\t" + rb)
    rb
  }


}



trait TypeCheck extends Normalization {

  import sem.force

  def global(g: GlobalReference) = sem.global(g.str)

  val inferCache = mutable.Map.empty[Term, Value]

  var debuggingInferCheck = false
  var level = 0

  val str = "                                                                                                           "

  def lstr(): String = str.take(level * 2)

  // local typing context
  // the context is so that the head is index 0
  case class Context(ctx: Seq[Seq[(String, Value)]]) {

    def head = ctx.head.map(_._2)
    // new index
    def el() = this.copy(ctx = Seq.empty +: ctx)

    def el(s: Seq[Value]) = this.copy(ctx = s.map(a => (sem.Generic().name, a)) +: ctx)


    // new small index
    def es(v: Value) = this.copy(ctx = (ctx.head :+ (sem.Generic().name, v)) +: ctx.tail)

    def local(l: LocalReference): Value = ctx(l.big)(l.small)._2


    // return the type of a term in semantics world
    def infer(term: Term, fromCheck: Boolean = false): Value = {
      term match {
        case GlobalReference(g) =>
          val res = sem.global(g).stype // early return!!!
          if (Debug && !debuggingInferCheck) delog(lstr() + "Inferred. global reference " + g)
          return res  // ALERT: early return!!!
        case _ => Unit // drop out
      }
      val termClosed = term.closed()
      if (termClosed) {
        inferCache.get(term) match {
          case Some(a) =>
            if (Debug && !debuggingInferCheck) delog(lstr() + "Inferred. Cache hit for closed term " + term)
            return a // ALERT: early return!!!
          case _ => Unit
        }
      }
      if (!debuggingInferCheck) {
        delog(lstr() + "Inferring " + term + ". Context: " + ctx.reverse.map(a => a.map(k => k._1 +":" + readback(k._2, ctx)).mkString(" __ ")).mkString(" || "))
        level += 1
      }
      def checkLambdaArgs(is: Seq[Option[Term]]): Context = {
        assert(is.forall(a => a.nonEmpty))
        is.map(_.get).foldLeft(el()) { (c, p) =>
          c.es(c.checkIsTypeThenEval(p))
        }
      }
      val res = term match {
        case g: GlobalReference => throw new IllegalStateException("Should be short cut")
        case l: LocalReference => local(l)
        case Fix(t) =>
          def checkFixType(a: Term) = el().es(sem.Universe()).infer(a)
          t.head match {
            case Ascription(tt, ty) =>
              el().es(checkIsTypeThenEval(ty)).infer(tt)
            case Lambda(is, Ascription(tt, ty)) =>
              val c = checkLambdaArgs(is)
              val vty = c.checkIsTypeThenEval(ty)
              c.check(tt, vty)
              eval(Pi(c.head.map(a => readback(a, c.ctx)), readback(vty, c.ctx)), ctx)
            case a: Sum =>
              checkFixType(a)
            case a: Pi =>
              checkFixType(a)
            case a: Sigma =>
              checkFixType(a)
            case _ => throw new Exception("Cannot infer Fix")
          }
        case Ascription(left, right) =>
          val r = checkIsTypeThenEval(right)
          check(left, r)
          r

        case Lambda(is, body) =>
          val c = checkLambdaArgs(is)
          val v = c.infer(body)
          // the readback might contains sem.OpenReference,
          // but after we eval with a Pi
          // it can happens that the binding will rebind inside the eval
          eval(Pi(c.head.map(a => readback(a, c.ctx)), readback(v, c.ctx)), ctx)
        case App(l, rs) =>
          force(infer(l)) match {
            case sem.Pi(size, inside) =>
              var i = 0
              var confirmed = Seq.empty[Value]
              while (i < size) { // check i-th type
                val test = confirmed ++ (i until size).map(_ => sem.DebugPoison())
                val applied = inside(test)
                val expected = applied._1(i) // it should NOT contain any new open variables...
                if (Debug) {
                  readback(expected, ctx)
                }
                check(rs(i), expected)
                confirmed = confirmed :+ eval(rs(i), ctx)
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
              val res = ts(ms.map(a => eval(Projection(left, a), ctx)))
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
          a.is.foldLeft(el()) { (c, t) =>
            c.es(c.checkIsTypeThenEval(t))
          }.checkIsType(a.to)
          sem.Universe()
        case a: Sum =>
          a.ts.values.foreach(k => checkIsType(k))
          sem.Universe()
        case a: Sigma =>
          a.ts.foldLeft(el()) { (c, t) =>
            c.es(c.checkIsTypeThenEval(t))
          }
          sem.Universe()
        case Universe() =>
          sem.Universe()
      }
      val forced = res
      if (termClosed) inferCache.put(term, forced)
      if (!debuggingInferCheck) {
        level -= 1
        if (Debug) {
          delog(lstr() + "Inferred: " + readback(forced, ctx))
          if (!fromCheck) {
            debuggingInferCheck = true
            check(term, forced)
            debuggingInferCheck = false
          }
        }
      }
      forced
    }


    // this can be considered just a wrapper for infer(term) subtypeOf t
    // only that: it calls force
    // it handles parameter less lambda
    // so ALWAYS call this instead of subtypeOf directly
    // (unless you know what you are doing)
    // and if you look at the code above,
    // you can see that the check is basically used by
    // function application.....
    // and fix..................
    def check(term: Term, ty0: Value): Unit = {
      val ty = force(ty0)
      term match {
        case GlobalReference(g) =>
          assert(sem.global(g).stype subtypeOf ty) // early return!!!
          delog(lstr() + "Checked global reference " + g + " with " + readback(ty, ctx))
          return
        case _ => Unit // drop out
      }
      if (term.closed()) {
        inferCache.get(term) match {
          case Some(a) =>
            assert(a subtypeOf ty)
            if (Debug && !debuggingInferCheck) delog("Checked. Cache hit for closed term " + term + " with " + readback(ty, ctx))
            if (!debuggingInferCheck) return
          case _ => Unit // ALERT dropout!
        }
      }
      if (!debuggingInferCheck) {
        delog(lstr() + "Checking " + term + " with " + readback(ty, ctx) + ". Context: " + ctx.reverse.map(a => a.map(k => k._1 +":" + readback(k._2, ctx)).mkString(" __ ")).mkString(" || "))
        level += 1
      }
      (term, ty) match {
        case (Lambda(is, body), p@sem.Pi(size, inside)) =>
          assert(size == is.size)
          if (is.forall(_.nonEmpty)) {
            assert(infer(term, fromCheck = true) subtypeOf p)
          } else if (is.forall(_.isEmpty)) {
            val t = inside(is.indices.map(a => sem.Generic())) // TODO fix this with generics!!!
            el(t._1).check(body, t._2)
          } else {
            throw new Exception(".. this is not implemented yet")
          }
        case (Record(ms, ts), sem.Sigma(ms0, ts0)) =>
          assert(ms == ms0)
          val size = ms.size
          var i = 0
          var confirmed = Seq.empty[Value]
          while (i < size) { // check i-th type
            val test = confirmed ++ (i until size).map(_ => sem.DebugPoison())
            val applied = ts0(test)
            val expected = applied(i) // it should NOT contain any new open variables...
            if (Debug) {
              readback(expected, ctx)
            }
            check(ts(i), expected)
            confirmed = confirmed :+ eval(ts(i), ctx)
            i += 1
          }
        case (e, t) =>
          if (!debuggingInferCheck) assert(infer(e, fromCheck = true) subtypeOf t)
      }
      if (!debuggingInferCheck) {
        level -= 1
        if (Debug) {
          delog(lstr() + "Checked.")
        }
      }
    }

    def checkIsUniverse(t: Term) = t match {
      case Universe() => Unit
      case _ => throw new Exception("Check is universe failed")
    }


    def checkIsTypeThenEval(t: Term) = {
      // here, we might check the type of a reference
      checkIsType(t)
      // then directly give it here - BAD!
      eval(t, ctx)
    }
    // no need to go inside check for now
    def checkIsType(t: Term): Unit = assert(infer(t) subtypeOf sem.Universe())
  }

  object Context {
    val Empty = Context(Seq.empty)
  }

  def check(f: Module): Unit = {
    f.ds.foreach(d => {
      val ty = Context.Empty.infer(d._2)
      sem.defs += (d._1 -> sem.Global(eval(d._2, Seq.empty), ty))
    })
  }
}

object tests extends scala.App with TypeCheck {

  def r(b: Int, r: Int) = LocalReference(b, r) // reference
  def a(t: Term, ts: Term*) = App(t, ts) // app
  def pi(t: Term*) = Pi(t.dropRight(1), t.last)
  def lam(t: Term*) = Lambda(t.dropRight(1).map(a => Some(a)), t.last)
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
  def abort() = {
    println("")
    System.out.flush()
    println("")
    if (2 + 1 == 3) throw new Exception("Abort mission!")
  }
  def eminfer(t: Term) = readback(Context.Empty.infer(t), Seq.empty)
  def emcheck(t: Term, v: Term) = eminfer(Ascription(t, v))

  def fails(a: => Unit) = {
    Try(a) match {
      case Success(_) => throw new Exception()
      case Failure(_) => Unit
    }
  }

  def eqs(a: Term, b: Term) = assert(a == b)


  def debugDefine(name: String, a: Term, ty: Term, nf: Term = null, isCheck: Boolean = false): Term = {
    println("### Defining " + name + ". Assert ty is type")
    Context.Empty.checkIsType(ty)
    println("### Assert a is of type ty")
    emcheck(a, ty)

//    println("### Assert normal forms is correct")
//    if (nf != null) assert(nbe(a) == nf)
//    else assert(nbe(a) == tps(a))
//    assert(nbe(ty) == tps(ty))

    sem.addGlobal(name, sem.Global(eval(a, Seq.empty), eval(ty, Seq.empty)))
    val res = GlobalReference(name)
    println("### End defining " + name + "\n\n")
    res
  }

  def debugVal(a: Term): Value = {
    a match {
      case GlobalReference(k) => sem.global(k).svalue
      case _ => throw new Exception("")
    }
  }

  Debug = false

  Debug = true

  val u = debugDefine("u",
    Universe(),
    Universe()
  )

  // \(x : type, y: x, z: x) => x
  val t1 = debugDefine("t1",
    lam(u, r(0, 0), r(0, 0), r(0, 0)),
    pi(u, r(0, 0), r(0, 0), u)
  )


  // record[]
  val unit = debugDefine("unit",
    Sigma(Seq.empty, Seq.empty),
    u
  )

  val unit0 = debugDefine("unit0",
    Record(Seq.empty, Seq.empty),
    unit,
    isCheck = true
  )

  // \(a: type, x: a) => x
  val id = debugDefine("id",
    lam(u, r(0, 0), r(0, 1)),
    pi(u, r(0, 0), r(0, 0))
  )

  val a_id_u_unit = debugDefine("a_id_u_unit",
    a(id, u, unit),
    u,
    nf = unit
  )

  val a_id_unit_unit0 = debugDefine("a_id_unit_unit0",
    a(id, unit, unit0),
    unit,
    nf = unit0
  )

  val idc = debugDefine("idc",
    lam(u, lam(r(1, 0), r(0, 0))),
    pi(u, pi(r(1, 0), r(1, 0)))
  )

  val a_idc_u_unit = debugDefine("a_idc_u_unit",
    a(a(idc, u), unit),
    u,
    nf = unit
  )

  val a_idc_unit_unit0 = debugDefine("a_idc_unit_unit0",
    a(a(idc, unit), unit0),
    unit,
    nf = unit0
  )




  val id_u = debugDefine("id_u",
    lam(u, r(0, 0)),
    pi(u, u)
  )

  assert(readback(debugVal(id_u), Seq.empty) == nbe(lam(u, a(id, u, r(0, 0)))))




  // \(x: type, f: x -> x, a: x) => f (f a)
  val mapp = debugDefine("mapp",
    lam(u, pi(r(1, 0), r(1, 0)), r(0, 0), a(r(0, 1), r(0, 2))),
    pi(u, pi(r(1, 0), r(1, 0)), r(0, 0), r(0, 0))
  )


  // \(x: type, f: x -> x, a: x) => f (f a)
  val double = debugDefine("double",
    lam(u, pi(r(1, 0), r(1, 0)), r(0, 0), a(r(0, 1), a(r(0, 1), r(0, 2)))),
    pi(u, pi(r(1, 0), r(1, 0)), r(0, 0), r(0, 0))
  )

  // fix self => sum(zero: unit, succ: self)
  val num = debugDefine("num",
    fix(Sum(Map("zero" -> unit, "succ" -> r(0, 0)))),
    u
  )


  val n0 = debugDefine("n0", Construct("zero", unit0), num)
  val n1 = debugDefine("n1", Construct("succ", n0), num)
  val n2 = debugDefine("n2", Construct("succ", n1), num)
  val n3 = debugDefine("n3", Construct("succ", n2) , num)
  val n4 = debugDefine("n4", Construct("succ", n3) , num)
  val n5 = debugDefine("n5", Construct("succ", n4) , num)
  val n6 = debugDefine("n6", Construct("succ", n5) , num)
  val n7 = debugDefine("n7",  Construct("succ", n6) , num)
  val n8 = debugDefine("n8",  Construct("succ", n7) , num)
  val n9 = debugDefine("n9", Construct("succ", n8) , num)
  val n10 = debugDefine("n10", Construct("succ", n9) , num)
  val n11 = debugDefine("n11", Construct("succ", n10) , num)
  val n12 = debugDefine("n12", Construct("succ", n11) , num)
  val n13 = debugDefine("n13", Construct("succ", n12) , num)
  val n14 = debugDefine("n14", Construct("succ", n13) , num)
  val n15 = debugDefine("n15", Construct("succ", n14), num)
  val n16 = debugDefine("n16", Construct("succ", n15) , num)


  val n0t16 = Seq(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)

  val succ = debugDefine("succ",
    lam(num, Construct("succ", r(0, 0))),
    pi(num, num)
  )

  abort()

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

  abort()

  val pair = lam(u, u, Sigma(Seq("_1", "_2"), Seq(r(1, 0), r(1, 1))))
  val pair_num_num = Sigma(Seq("_1", "_2"), Seq(num, num))
  assert(nbe(pair) == tps(pair))
  def mk_pair(seq: Term*) = Record(Seq("_1", "_2"), seq)
  assert(nbe(a(pair, num, num)) == pair_num_num)

  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self k b) }
  val plus = fix(lam(num, num, Split(r(0, 0), Map("zero" -> r(1, 1), "succ" -> Construct("succ", a(r(2, 0), r(0, 0), r(1, 1)))))))
  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self b k) }
  val plus1 = fix(lam(num, num, Split(r(0, 0), Map("zero" -> r(1, 1), "succ" -> Construct("succ", a(r(2, 0), r(1, 1), r(0, 0)))))))
  // fix self => (b, a: nat) => split a { case zero => b; case succ k => succ(self k b) }
  val plus2 = fix(lam(num, num, Split(r(0, 1), Map("zero" -> r(1, 0), "succ" -> Construct("succ", a(r(2, 0), r(0, 0), r(1, 0)))))))
  // fix self => (b, a: nat) => split a { case zero => b; case succ k => succ(self b k) }
  val plus3 = fix(lam(num, num, Split(r(0, 1), Map("zero" -> r(1, 0), "succ" -> Construct("succ", a(r(2, 0), r(1, 0), r(0, 0)))))))
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
  val mult = fix(lam(num, num, Split(r(0, 0), Map("zero" -> n0, "succ" -> a(plus, r(1, 1), a(r(2, 0), r(0, 0), r(1, 1)))))))
  val mult1 = fix(lam(num, num, Split(r(0, 0), Map("zero" -> n0, "succ" -> a(plus, r(1, 1), a(r(2, 0), r(1, 1), r(0, 0)))))))

  for (i <- 0 to 16) {
    for (j <- 0 to 16) {
      if (i * j <= 16) {
        assert(nbe(a(mult, n0t16(i), n0t16(j))) == n0t16(i * j))
        assert(nbe(a(mult1, n0t16(i), n0t16(j))) == n0t16(i * j))
      }
    }
  }

}
