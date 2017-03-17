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

object DebugLevel extends  UtilsCommon {
  var level = 0
  val lllstr = "                                                                                                           "
  def lstr(): String = lllstr.take(level * 2)
  var debuggingInferCheck = false
}; import DebugLevel._


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
      if (Debug && !debuggingInferCheck) delog(lstr() + "subtype called...")
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
          val k = Generic()
          t0(k) subtypeOf t1(k)
        case (sem.Fix(t), a) =>
          val k = this
          t(k) subtypeOf a
        case (a, sem.Fix(t)) =>
          val k = o
          a subtypeOf t(k)
        case (GlobalReference(g1), GlobalReference(g2)) =>
          if (g1 == g2) true
          else sem.global(g1).svalue subtypeOf sem.global(g2).svalue
        case (GlobalReference(g1), g2) =>
          sem.global(g1).svalue subtypeOf g2
        case (g1, GlobalReference(g2)) =>
          g1 subtypeOf sem.global(g2).svalue
        case (sem.Sigma(ms0, ts0), sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]
          true // TODO
        case (sem.Pi(vl, vs), sem.Pi(vl1, vs1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
          val g = Generic()
          (vl1 subtypeOf vl) && (vs(g) subtypeOf vs1(g))
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
            val k = Generic()
            t0(k) :/\: t1(k)
          case (sem.Fix(t), a) =>
            val k = this
            t(k) :/\: a
          case (a, sem.Fix(t)) =>
            val k = o
            a :/\: t(k)
          case (s0@sem.Sigma(ms0, ts0), s1@sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]
            if (s0 subtypeOf s1) { // TODO bad
              s0
            } else if (s1 subtypeOf s0) {
              s1
            } else {
              Bottom
            }
          case (p0@sem.Pi(left, inside), p1@sem.Pi(left1, inside1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
            if (p0 subtypeOf p1) { // TODO bad
              p0
            } else if (p1 subtypeOf p0) {
              p1
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
            val k = Generic()
            t0(k) :\/: t1(k)
          case (sem.Fix(t), a) =>
            val k = this
            t(k) :\/: a
          case (a, sem.Fix(t)) =>
            val k = o
            a :\/: t(k)
          case (s0@sem.Sigma(ms0, ts0), s1@sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]
            if (s0 subtypeOf s1) { // TODO bad
              s1
            } else if (s1 subtypeOf s0) {
              s0
            } else {
              Bottom
            }
          case (p0@sem.Pi(left, inside), p1@sem.Pi(left1, inside1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
            if (p0 subtypeOf p1) { // TODO bad
              p1
            } else if (p1 subtypeOf p0) {
              p0
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
    def app(seq: Value): Value = throw new Exception()
    def split(bs: Map[String, Value => Value]): Value = throw new Exception()
  }



  def join(seq: Seq[Value]): Value = {
    if (seq.isEmpty) Bottom
    else seq.tail.fold(seq.head) { (v0, v1) => v0 :\/: v1}
  }

  def meet(seq: Seq[Value]): Value = {
    assert(seq.nonEmpty)
    seq.tail.fold(seq.head) { (v0, v1) => v0 :/\: v1}
  }

  // these are where stuck state starts
  sealed abstract class Stuck extends Value {
    override def app(seq: Value): Value = App(this, seq)
    override def projection(s: String) = Projection(this, s)
    override def split(bs: Map[String, Value => Value]) = Split(this, bs)
  }

  // lazy values are these only reduce when eliminated or forced
  sealed abstract class Lazy extends Value

  case class Fix(t: Value => Value) extends Lazy {
    override def app(seq: Value) = t(this).app(seq)
    override def projection(s: String) = t(this).projection(s)
    override def split(bs: Map[String, Value => Value]) = t(this).split(bs)
  }


  // global reference is kind of like fix, they are "wrapped" so that we don't expand all things
  // referenced by it when reading back
  // this also means that global reference is NOT reduced inside lambda
  // so it plays a role in syntax equality
  case class GlobalReference(name: String) extends Lazy {
    override def app(seq: Value) = global(name).svalue.app(seq)
    override def projection(s: String) = global(name).svalue.projection(s)
    override def split(bs: Map[String, Value => Value]) = global(name).svalue.split(bs)
  }

  def force(v: Value): Value = {
    def loop(v: Value): Value = v match {
      case a: Lazy => a match { // for totality check
        case sem.GlobalReference(s) => sem.global(s).svalue
        case f@sem.Fix(k) => k(f)
      }
      case c => c
    }
    var p = v
    var n = loop(p)
    while (n != p) {
      p = n
      n = loop(p)
    }
    if (Debug && p != v) delog(lstr() + "forced a value...")
    n
  }

  // ... it cannot be a because consider when you read back the
  // lam a: (x: type, y: x) => a.y
  // so it needs to be a stuck term
  case class OpenReference(depth: Int) extends Stuck

  // we DON'T really support reading REALLY open references, because their semantics is not complete
  // all "open" reference in term lang, will be turn into some Generic inside the semantics world
  // the name is from the paper "miniTT"
  case class Generic(name: String = newUniqueName()) extends Stuck // this references the things in our env

  // accumulators
  case class Projection(value: Stuck, str: String) extends Stuck
  case class App(atom: Stuck, app: Value) extends Stuck
  case class Split(s: Stuck, names:  Map[String, Value => Value]) extends Stuck



  case class Lambda(fun: Value => Value) extends Value {
    override def app(seq: Value) = fun(seq)
  }
  case class Construct(name: String, apps: Value) extends Value {
    override def split(bs: Map[String, Value => Value]) = bs(name)(apps)
  }
  case class Record(ms: Seq[String], vs: Seq[Value]) extends Value {
    override def projection(s: String) = vs(ms.indexOf(s))
  }

  case class Universe() extends Value

  case class Srh(t: Value, f: Value => Srh) // we use null to means STOPed here.. you should not really need this but..
  case class Sigma(ms: Seq[String], ts: Srh) extends Value
  case class Pi(left: Value, inside: Value => Value) extends Value
  case class Sum(ts: Map[String, Value]) extends Value



  case class Def(svalue: Value, stype: Value)

  object global extends (String => Def) {
    val defs = mutable.Map.empty[String, Def] // defined global variables, and their normal form and type
    def apply(str: String) = defs(str)
    def add(str: String, global: Def) = {
      if (defs.contains(str)) {
        throw new Exception("Not allowed, global have same name")
      } else {
        defs += str -> global
      }
    }
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

  type TypingCtx = Seq[(String, Value)]

}

import sem.Value
import sem.TypingCtx

trait Normalization {

  def readback(v: Value, ctx: TypingCtx, testOnlyForceFullValue: Boolean = false, isDebug: Boolean = false): Term = {
    def rec(v: Value, depth: Int): Term = {
      val nd = depth + 1
      val ccc = -nd - 1
      v match {
        case sem.GlobalReference(n) =>
          if (testOnlyForceFullValue) rec(sem.global(n).svalue, 0)
          else GlobalReference(n)
        case sem.Generic(g) =>
          for (i <- ctx.indices) {
            val cs = ctx(i)
            if (cs._1 == g) {
              val local = LocalReference(depth + 1 + i)
              local.debugGeneric = g
              return local
            }
          }
          throw new Exception("")
        case sem.Fix(f) =>
          Fix(rec(f(sem.OpenReference(ccc)), nd))
        case sem.OpenReference(d) => // we don't want truely open references, all local reference must be from a Generic
          assert(d < 0)
          LocalReference(d + depth + 1)
        case sem.Lambda(fun) =>
          Lambda(None, rec(fun(sem.OpenReference(ccc)), nd))
        case sem.Construct(name, value) =>
          Construct(name, rec(value, depth))
        case sem.Record(ms, vs) =>
          Record(ms, vs.map(v => rec(v, depth)))

        case sem.App(left, vs) =>
          App(rec(left, depth), rec(vs, depth))
        case sem.Projection(vv, s) =>
          Projection(rec(vv, depth), s)
        case sem.Split(s, names) =>
          Split(rec(s, depth), names.mapValues(v => rec(v(sem.OpenReference(ccc)), nd)))

        case sem.Pi(left, fun) =>
          Pi(rec(left, depth), rec(fun(sem.OpenReference(ccc)), nd))
        case sem.Sigma(ms, ts) =>
          val nnts = ms.indices.foldLeft((Seq.empty[Term], ts)) { (pair, i) =>
            val terms = pair._1
            val srh = pair._2
            val nterms = terms :+ rec(srh.t, depth + i)
            (nterms, srh.f(sem.OpenReference(ccc - i)))
          }._1
          Sigma(ms, nnts)
        case sem.Sum(ts) =>
          Sum(ts.mapValues(c => rec(c, depth)))
        case sem.Universe() => Universe()
      }
    }
    val res = rec(v, -1)
    if (Debug && !isDebug) delog(lstr() + "Readback " + res)
    res
  }


  // needs to ensure term is well typed first!
  def eval(term: Term, ctx: TypingCtx): Value = {
    // TODO eval cache for closed terms...
    def emitScala(t: Term, depth: Int): String = {
      t match {
        case GlobalReference(str) =>
          s"sem.GlobalReference(${sem.names.emitScala(str)})"
        case LocalReference(b) =>
          // the reason we use a global depth for the big index, is because
          // all free variables is inside the term
          // and all our structural recursive read back function ensures us
          // when reconstructing the term, the depth of binding site is stable when we construct them
          // and when the reference is constructed, the depth of the term is table, and so we can get
          // back the index
          if (b > depth) s"sem.Generic(${sem.names.emitScala(ctx(b - depth - 1)._1, '$')})"
          //if (b > depth) s"sem.OpenReference(${b - depth - 1}, $s)"
          else s"b${depth - b}"
//        case Generic(a) =>
//          s"sem.Generic(${sem.names.emitScala(a, '$')})"
        case Fix(ttt) =>
          val d = depth + 1
          s"sem.Fix(b$d => ${emitScala(ttt, d)})"
        case Ascription(left, _) =>
          emitScala(left, depth) // TODO?
        case Lambda(_, body) =>
          val d = depth + 1
          s"sem.Lambda(b$d => ${emitScala(body, d)})"
        case App(left, right) =>
          s"${emitScala(left, depth)}.app(${emitScala(right, depth)})"
        case Pi(vs, body) =>
          val d = depth + 1
          s"sem.Pi(${emitScala(vs, depth)}, b$d => ${emitScala(body, d)})"
        case Universe() => s"sem.Universe()"
        case Record(ms, ts) =>
          s"sem.Record(Seq(${ms.map(a => sem.names.emitScala(a, '@')).mkString(", ")}), Seq(${ts.map(a => emitScala(a, depth)).mkString(", ")}))"
        case Sigma(ms, vs) =>
          val body = vs.zipWithIndex.foldRight("null") { (tm, txt) =>
            val t = tm._1
            val index = tm._2
            s"sem.Srh(${emitScala(t, depth + index)}, b${depth + index + 1} => $txt)"
          }
          s"sem.Sigma(Seq(${ms.map(a => sem.names.emitScala(a, '@')).mkString(", ")}), $body)"
        case Projection(left, right) =>
          s"${emitScala(left, depth)}.projection(${sem.names.emitScala(right, '@')})"
        case Sum(ts) =>
          s"sem.Sum(Map(${ts.map(p =>  sem.names.emitScala(p._1, '#') + " -> " + emitScala(p._2, depth)).mkString(", ")}))"
        case Construct(name, tt) =>
          s"sem.Construct(${sem.names.emitScala(name, '#')}, ${emitScala(tt, depth)})"
        case Split(left, right) =>
          val d = depth + 1
          s"${emitScala(left, depth)}.split(Map(${right.map(p => sem.names.emitScala(p._1, '#') + " -> (b" + d  + " => " + emitScala(p._2, d) + ")").mkString(", ")}))"
      }
    }
    // we skip code generation for atom ones, it might be faster than compiling the code...?
    term match {
      case LocalReference(b) =>
        sem.Generic(ctx(b)._1)
      case Universe() => sem.Universe()
      case GlobalReference(str) => sem.GlobalReference(str)
      case _ =>
        var time = 0L
        var timeEmitted = 0L
        if (Debug) time = System.currentTimeMillis()
        val text = emitScala(term, -1)
        if (Debug) timeEmitted = System.currentTimeMillis() - time
        val twitterEval = new Eval()
        val res = twitterEval.apply[Value](text)
        if (Debug) delog(lstr() + "Emitted in " + timeEmitted + ". Compiled in " + (System.currentTimeMillis() - time - timeEmitted) + ". " + text)
        res
    }
  }

  val DebugNbe = Debug && false
  def nbe(t: Term, testOnlyForceFullValue: Boolean = false) = {
    if (DebugNbe) delog("NbE: " + t)
    val e = eval(t, Seq.empty)
    val rb = readback(e, Seq.empty, testOnlyForceFullValue = testOnlyForceFullValue)
    if (DebugNbe) delog("\t" + rb)
    rb
  }
}



trait TypeCheck extends Normalization {

  import sem.force

  def global(g: GlobalReference) = sem.global(g.str)

  val closedTermMinimalTypeCache = mutable.Map.empty[Term, Value]


  // local typing context
  // the context is so that the head is index 0
  case class Context(ctx: TypingCtx) {

    def expand(v: Value) = {
      val pair = (sem.Generic().name, v)
      this.copy(ctx = pair +: ctx)
    }

    def expand(pair: (String, Value)) = this.copy(ctx = pair +: ctx)

    def head: (String, Value) = ctx.head

    def debugContextStr() = ctx.reverse.map(k => k._1 +":" + readback(k._2, ctx, isDebug = true)).mkString(" __ ")

    def local(l: LocalReference): Value = ctx(l.big)._2

    // return the type of a term in semantics world
    def infer(term: Term, debugFromCheck: Boolean = false): Value = {
      term match {
        case GlobalReference(g) =>
          val res = sem.global(g).stype // early return!!!
          if (Debug && !debuggingInferCheck) delog(lstr() + "Inferred, global reference " + g)
          return res  // ALERT: early return!!!
        case _ => Unit // drop out
      }
      val termClosed = term.closed()
      if (termClosed) {
        closedTermMinimalTypeCache.get(term) match {
          case Some(a) =>
            if (Debug && !debuggingInferCheck) delog(lstr() + "Inferred, cache hit for closed term " + term)
            return a // ALERT: early return!!!
          case _ => Unit
        }
      }
      if (Debug && !debuggingInferCheck) {
        delog(lstr() + "Inferring " + term + ". Context: " + debugContextStr())
        level += 1
      }
      val res: Value = term match {
        case _: GlobalReference => throw new IllegalStateException("Should be short-cut")
        case l: LocalReference => local(l)
        case Fix(t) =>
          def checkFixType(a: Term) = {
            expand(sem.Universe()).checkIsType(a)
            sem.Universe()
          }
          t match {
            case Ascription(tt, ty) =>
              val vty = checkIsTypeAndEval(ty)
              expand(vty).check(tt, vty)
              vty
            case Lambda(Some(is), Ascription(tt, ty)) =>
              // some fishy thing going on here... but it is ok...
              val pty = checkIsTypeAndEval(is)
              val ct = expand(pty)
              val vty = ct.checkIsTypeAndEval(ty)
              val ttt = eval(Pi(readback(pty, ctx), readback(vty, ct.ctx)), ctx)
              expand(ttt).expand(ct.head).check(tt, vty)
              ttt
            case a: Sum =>
              checkFixType(a)
            case a: Pi =>
              checkFixType(a)
            case a: Sigma =>
              checkFixType(a)
            case _ => throw new Exception("Cannot infer Fix")
          }
        case Ascription(left, ty) =>
          val r = checkIsTypeAndEval(ty)
          check(left, r)
          r

        case Lambda(Some(is), body) =>
          val pty = checkIsTypeAndEval(is)
          val ct = expand(pty)
          val vty = ct.infer(body)
          eval(Pi(readback(pty, ctx), readback(vty, ct.ctx)), ctx) // TODO make ways to NOT readback the first one, only the second one
        case Lambda(None, _) => throw new Exception("Cannot infer lambda without parameter types")
        case App(l, rs) =>
          force(infer(l)) match {
            case sem.Pi(left, right) =>
              val t = checkAndEval(rs, left)
              right(t)
            case _ => throw new Exception("Cannot infer App")
          }

        case Record(ms, ts) => // mmm... we actually don't use this branch that much for dependent records
          val vs = ts.map(a => infer(a))
          sem.Sigma(ms, vs.foldRight(null: sem.Srh) { (sig, txt) =>
            sem.Srh(sig, _ => txt)
          })
        case Projection(left, right) =>
          force(infer(left)) match {
            case sem.Sigma(ms, ts) =>
              val index = ms.indexOf(right)
              // in this sigma, their is NO mutual reference
              var i: sem.Srh = ts
              var k = 0
              while (k < index) {
                i = i.f(eval(left, ctx).projection(ms(k)))
                k += 1
              }
              i.t
            case _ => throw new Exception("Cannot infer Projection")
          }

        case Construct(name, v) =>
          sem.Sum(Map(name -> infer(v))) // a minimal type
        case Split(left, right) =>
          force(infer(left)) match {
            case sem.Sum(ts) => // right is bigger
              assert((ts.keySet -- right.keySet).isEmpty)
              sem.join(ts.toSeq.map(a => {
                val at = a._2
                val term = right(a._1)
                expand(at).infer(term)
              }))
            case _ => throw new Exception("Cannot infer Split")
          }
        case a: Pi =>
          expand(checkIsTypeAndEval(a.is)).checkIsType(a.to)
          sem.Universe()
        case a: Sum =>
          a.ts.values.foreach(k => checkIsType(k))
          sem.Universe()
        case a: Sigma =>
          a.ts.foldLeft(this) { (c, t) =>
            c.expand(c.checkIsTypeAndEval(t))
          }
          sem.Universe()
        case Universe() =>
          sem.Universe()
      }
      if (termClosed) closedTermMinimalTypeCache.put(term, res)
      if (Debug && !debuggingInferCheck) {
        level -= 1
        delog(lstr() + "Inferred: " + readback(res, ctx, isDebug = true))
        if (!debugFromCheck) {
          debuggingInferCheck = true
          check(term, res)
          debuggingInferCheck = false
        }
      }
      res
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
          delog(lstr() + "Checked global reference " + g + " with " + readback(ty, ctx, isDebug = true))
          return
        case _ => Unit // drop out
      }
      if (term.closed()) {
        closedTermMinimalTypeCache.get(term) match {
          case Some(a) =>
            assert(a subtypeOf ty)
            if (Debug && !debuggingInferCheck) delog("Checked. Cache hit for closed term " + term + " with " + readback(ty, ctx, isDebug = true))
            if (!debuggingInferCheck) return
          case _ => Unit // ALERT dropout!
        }
      }
      if (Debug && !debuggingInferCheck) {
        delog(lstr() + "Checking " + term + " with " + readback(ty, ctx, isDebug = true) + ". Context: " + debugContextStr())
        level += 1
      }
      (term, ty) match {
        case (Lambda(is, body), p@sem.Pi(left, inside)) =>
          if (is.nonEmpty) {
            assert(infer(term, debugFromCheck = true) subtypeOf p)
          } else {
            val gs = sem.Generic()
            val t = inside(gs)
            expand((gs.name, left)).check(body, t)
          }
        case (Record(ms, ts), sem.Sigma(ms0, ts0)) => // this is all similar to app...
          assert(ms == ms0)
          val size = ms.size
          var i = 0
          var srh = ts0
          while (i < size) { // check i-th type
            val v = checkAndEval(ts(i), srh.t)
            expand(v)
            srh = srh.f(v)
            i += 1
          }
        case (e, t) =>
          if (!debuggingInferCheck) assert(infer(e, debugFromCheck = true) subtypeOf t)
      }
      if (Debug && !debuggingInferCheck) {
        level -= 1
        delog(lstr() + "Checked.")
      }
    }

    def checkIsUniverse(t: Term) = t match {
      case Universe() => Unit
      case _ => throw new Exception("Check is universe failed")
    }

    def checkAndEval(t: Term, v: Value) = { check(t, v); eval(t, ctx) }
    def checkIsTypeAndEval(t: Term) = { checkIsType(t); eval(t, ctx) }
    // no need to go inside check for now
    def checkIsType(t: Term): Unit = assert(infer(t) subtypeOf sem.Universe())
  }

  object Context {
    val Empty = Context(Seq.empty)
  }

  def check(f: Module): Unit = {
    f.ds.foreach(d => {
      val ty = Context.Empty.infer(d._2)
      sem.global.add(d._1, sem.Def(eval(d._2, Seq.empty), ty))
    })
  }
}

object tests extends scala.App with TypeCheck {

  val TermUnit0 = Record(Seq.empty, Seq.empty)
  val TermUnit = Sigma(Seq.empty, Seq.empty)
  def r(b: Int) = LocalReference(b) // reference
  def rr(b: Int, y: Int) = Projection(r(b), "_" + y)
  def a(t: Term, ts: Term*) =  // app
    if (ts.isEmpty) App(t, TermUnit0)
    else if (ts.size == 1) App(t, ts.head)
    else App(t, Record((0 until ts.size).map("_" + _), ts))
  def pi(t: Term*) =
    if (t.isEmpty) throw new Exception("")
    else if (t.size == 1) Pi(TermUnit, t.head)
    else if (t.size == 2) Pi(t.head, t(1))
    else Pi(Sigma((0 until t.size - 1).map("_" + _), t.dropRight(1)), t.last)

  def lam(t: Term*) =
    if (t.isEmpty) throw new Exception("")
    else if (t.size == 1) Lambda(Some(TermUnit), t.head)
    else if (t.size == 2) Lambda(Some(t.head), t(1))
    else Lambda(Some(Sigma((0 until t.size - 1).map("_" + _), t.dropRight(1))), t.last)

  def tps(t: Term) = t match { // trim parameters
    case l: Lambda =>
      l.copy(is = None)
    case f@Fix(a) => a match {
      case l: Lambda =>
        Fix(l.copy(is = None))
      case _ => f
    }
    case a => a
  }
  def fix(t: Term) = Fix(t)
  def abort() = {
    println("")
    System.out.flush()
    println("")
    if (2 + 1 == 3) throw new Exception("Abort mission!")
  }
  def eminfer(t: Term) = readback(Context.Empty.infer(t), Seq.empty, isDebug = true)
  def emcheck(t: Term, v: Term) = eminfer(Ascription(t, v))

  def fails(a: () => Any) = {
    Try(a()) match {
      case Success(_) => throw new Exception("Should fail!")
      case Failure(_) =>
        level = 0
        println("\n\nYes!!! It failed!!!!\n\n")
    }
  }

  def eqs(a: Term, b: Term) = assert(a == b)


  def debugDefine(name: String, a: Term, ty: Term, nf: Term = null, isCheck: Boolean = false): Term = {
    println("### Defining " + name)
    emcheck(a, ty)

//    println("### Assert normal forms is correct")
//    if (nf != null) assert(nbe(a) == nf)
//    else assert(nbe(a) == tps(a))
//    assert(nbe(ty) == tps(ty))

    sem.global.add(name, sem.Def(eval(a, Seq.empty), eval(ty, Seq.empty)))
    val res = GlobalReference(name)
    println("### End defining " + name + "\n\n")
    res
  }

  def fff(a: Term): Term = nbe(a, testOnlyForceFullValue = true)

  Debug = false


  val u = debugDefine("u",
    Universe(),
    Universe()
  )


  // \(x : type, y: x, z: x) => x
  val t1 = debugDefine("t1",
    lam(u, r(0), r(1), rr(0, 0)),
    pi(u, r(0), r(1), u)
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

  val lam_just_unit = debugDefine("lam_just_unit",
    lam(unit0),
    pi(unit)
  )

  val lam_unit_unit = debugDefine("lam_unit_unit",
    lam(unit, r(0)),
    pi(unit, unit)
  )

  val lam_unit_unit_unit = debugDefine("lam_unit_unit_unit",
    lam(unit, unit, rr(0, 0)),
    pi(unit, unit, unit)
  )

  val lam_unit_unit_unit1 = debugDefine("lam_unit_unit_unit1",
    lam(unit, unit, rr(0, 1)),
    pi(unit, unit, unit)
  )

  Debug = true

  val silly_app = debugDefine("silly_app",
    lam(pi(unit, unit), unit, a(Ascription(Lambda(None, a(rr(1, 0), r(0))), pi(unit, unit)), rr(0, 1))),
    pi(pi(unit, unit), unit, unit)
  )

  abort()

  val record_unit_unit0_unit0 = debugDefine("record_unit_unit0_unit0",
    Record(Seq("a", "b", "c"), Seq(unit, unit0, unit0)),
    Sigma(Seq("a", "b", "c"), Seq(u, unit, unit))
  )

  val record_unit_unit0_unit0_1 = debugDefine("record_unit_unit0_unit0_1",
    Record(Seq("a", "b", "c"), Seq(unit, unit0, unit0)),
    Sigma(Seq("a", "b", "c"), Seq(u, r(0), unit))
  )

  val record_unit_unit0_unit0_2 = debugDefine("record_unit_unit0_unit0_2",
    Record(Seq("a", "b", "c"), Seq(unit, unit0, unit0)),
    Sigma(Seq("a", "b", "c"), Seq(u, unit, r(1)))
  )

  val record_unit_unit0_unit0_3 = debugDefine("record_unit_unit0_unit0_3",
    Record(Seq("a", "b", "c"), Seq(unit, unit0, unit0)),
    Sigma(Seq("a", "b", "c"), Seq(u, r(0), r(1)))
  )

  fails(() =>
    debugDefine("fails",
      Record(Seq("a", "b", "c"), Seq(u, unit0, unit0)),
      Sigma(Seq("a", "b", "c"), Seq(u, r(0), r(1)))
    )
  )

  fails(() =>
    debugDefine("fails",
      Record(Seq("a", "b", "c"), Seq(unit, unit, unit0)),
      Sigma(Seq("a", "b", "c"), Seq(u, r(0), r(1)))
    )
  )


  // \(a: type, x: a) => x
  val id = debugDefine("id",
    lam(u, r(0), rr(0, 1)),
    pi(u, r(0), rr(0, 0))
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
    lam(u, lam(r(0), r(0))),
    pi(u, pi(r(0), r(1)))
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
    lam(u, r(0)),
    pi(u, u)
  )

  assert(fff(id_u) == fff(lam(u, a(id, u, r(0)))))




  // \(x: type, f: x -> x, a: x) => f (f a)
  val mapp = debugDefine("mapp",
    lam(u, pi(r(0), r(1)), r(1), a(rr(0, 1), rr(0, 2))),
    pi(u, pi(r(0), r(1)), r(1), rr(0, 0))
  )


  // \(x: type, f: x -> x, a: x) => f (f a)
  val double = debugDefine("double",
    lam(u, pi(r(0), r(1)), r(1), a(rr(0, 1), a(rr(0, 1), rr(0, 2)))),
    pi(u, pi(r(0), r(1)), r(1), rr(0, 0))
  )

  // fix self => sum(zero: unit, succ: self)
  val num = debugDefine("num",
    fix(Sum(Map("zero" -> unit, "succ" -> r(0)))),
    u
  )

  Debug = true

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
    lam(num, Construct("succ", r(0))),
    pi(num, num)
  )

  assert(fff(a(succ, n1)) == fff(n2))
  assert(fff(a(succ, n2)) == fff(n3))
  assert(fff(a(succ, n3)) == fff(n4))
  assert(fff(a(succ, n4)) == fff(n5))
  assert(fff(a(succ, n5)) == fff(n6))
  assert(fff(a(succ, a(succ, n1))) == fff(n3))
  assert(fff(a(succ, a(succ, n2))) == fff(n4))
  assert(fff(a(succ, a(succ, n3))) == fff(n5))
  assert(fff(a(succ, a(succ, n4))) == fff(n6))



  val pair = debugDefine("pair",
    lam(u, u, Sigma(Seq("_0", "_1"), Seq(rr(0, 0), rr(1, 1)))),
    pi(u, u, u)
  )

  assert(fff(a(pair, num, num)) == fff(Sigma(Seq("_0", "_1"), Seq(num, num))))


  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self k b) }
  val plus = debugDefine("plus",
    fix(lam(num, num, Ascription(Split(rr(0, 0), Map("zero" -> rr(1, 1), "succ" -> Construct("succ", a(r(2), r(0), rr(1, 1))))), num))),
    pi(num, num, num)
  )


  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self b k) }
  val plus1 = debugDefine("plus1",
    fix(lam(num, num, Ascription(Split(rr(0, 0), Map("zero" -> rr(1, 1), "succ" -> Construct("succ", a(r(2), rr(1, 1), r(0))))), num))),
    pi(num, num, num)
  )
  // fix self => (b, a: nat) => split a { case zero => b; case succ k => succ(self k b) }
  // fix self => (b, a: nat) => split a { case zero => b; case succ k => succ(self b k) }

  for (i <- 0 to 16) {
    for (j <- 0 to 16) {
      if (i + j <= 16) {
        assert(fff(a(plus, n0t16(i), n0t16(j))) == fff(n0t16(i + j)))
        assert(fff(a(plus1, n0t16(i), n0t16(j))) == fff(n0t16(i + j)))
      }
    }
  }

  // fix self => (a, b: nat) => split a { case zero => 0; case succ k => (plus b (self k b) }
  val mult = fix(lam(num, num, Split(rr(0, 0), Map("zero" -> n0, "succ" -> a(plus, rr(1, 1), a(r(2), r(0), rr(1, 1)))))))
  val mult1 = fix(lam(num, num, Split(rr(0, 0), Map("zero" -> n0, "succ" -> a(plus, rr(1, 1), a(r(2), rr(1, 1), r(0)))))))

  for (i <- 0 to 16) {
    for (j <- 0 to 16) {
      if (i * j <= 16) {
        assert(fff(a(mult, n0t16(i), n0t16(j))) == fff(n0t16(i * j)))
        assert(fff(a(mult1, n0t16(i), n0t16(j))) == fff(n0t16(i * j)))
      }
    }
  }

  abort()
}
