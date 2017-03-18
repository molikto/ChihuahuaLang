import com.twitter.util.Eval
import org.snailya.mygame.UtilsCommon
import sem.Generic

import scala.collection.mutable
import scala.util.{Failure, Success, Try}


// TODO: subtyping
// TODO: what about this part...??? euqlity type? inductive family? cubicaltt?

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

  // these are some normal forms, such that you need to apply to go on?
  // these are normal forms?
  var i = 0
  def newUniqueName(): String = {
    i += 1
    i.toString
  }

  sealed abstract class Value {

    def projection(s: String): Value = throw new Exception()
    def app(seq: Value): Value = throw new Exception()
    def split(bs: Map[String, Value => Value]): Value = throw new Exception()
  }


  val Bottom = Sum(Map.empty)

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

    override def equals(obj: scala.Any) = obj match {
      case Fix(tt) => val g = Generic(); t(g) == tt(g)
      case _ => false
    }
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

  val Poison = Generic()

  // accumulators
  case class Projection(value: Stuck, str: String) extends Stuck
  case class App(atom: Stuck, app: Value) extends Stuck
  case class Split(s: Stuck, names:  Map[String, Value => Value]) extends Stuck {
    override def equals(obj: scala.Any) = obj match {
      case Split(ss, n) =>
        ss == s && names.keySet == n.keySet && names.keySet.forall(s => {val g = Generic(); names(s)(g) == n(s)(g)})
      case _ => false
    }
  }



  case class Lambda(fun: Value => Value) extends Value {
    override def app(seq: Value) = fun(seq)

    override def equals(obj: scala.Any) = obj match {
      case Lambda(f) => val g = Generic(); fun(g) == f(g)
      case _ => false
    }
  }
  case class Construct(name: String, apps: Value) extends Value {
    override def split(bs: Map[String, Value => Value]) = bs(name)(apps)
  }
  case class Record(ms: Seq[String], vs: Seq[Value]) extends Value {
    override def projection(s: String) = vs(ms.indexOf(s))
  }

  case class Universe() extends Value

  // we use null to means STOPed here.. you should not really need this but..
  case class Srh(t: Value, f: Value => Srh) {
    override def equals(obj: scala.Any) = obj match {
      case Srh(tt, ff) => tt == t && {val g = Generic(); ff(g) == f(g)}
      case _ => false
    }
  }
  case class Sigma(ms: Seq[String], ts: Srh) extends Value {
    override def equals(obj: scala.Any) = obj match {
      case Sigma(m, t) => ms == m && t == ts
      case _ => false
    }
  }
  case class Pi(left: Value, inside: Value => Value) extends Value {
    override def equals(obj: scala.Any) = obj match {
      case Pi(l, ii) => left == l && { val g = Generic(); inside(g) == ii(g) }
      case _ => false
    }
  }
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

  type TypingCtx = Seq[(sem.Generic, Value)]

}

import sem.Value
import sem.TypingCtx

object normal {

  def readback(v: Value, ctx: TypingCtx, testOnlyForceFullValue: Boolean = false, isDebug: Boolean = false): Term = {
    def rec(v: Value, depth: Int): Term = {
      val nd = depth + 1
      val ccc = -nd - 1
      v match {
        case sem.GlobalReference(n) =>
          if (testOnlyForceFullValue) rec(sem.global(n).svalue, 0)
          else GlobalReference(n)
        case gg@sem.Generic(g) =>
          if (gg == sem.Poison) {
            val lr = LocalReference(1000)
            lr.debugGeneric = "poison"
            return lr
          }
          for (i <- ctx.indices) {
            val cs = ctx(i)
            if (cs._1.name == g) {
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
  // it is REALLY REALLY SLOW NOW. but we can emit JVM bytecode directly
  // or we can use a vitrual machine!
  def eval(term: Term, ctx: TypingCtx): Value = {
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
          if (b > depth) s"sem.Generic(${sem.names.emitScala(ctx(b - depth - 1)._1.name, '$')})"
          //if (b > depth) s"sem.OpenReference(${b - depth - 1}, $s)"
          else s"b${depth - b}"
//        case Generic(a) =>
//          s"sem.Generic(${sem.names.emitScala(a, '$')})"
        case Fix(ttt) =>
          val d = depth + 1
          s"sem.Fix(b$d => ${emitScala(ttt, d)})"
        case Ascription(left, _) =>
          emitScala(left, depth)
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
        ctx(b)._1
      case Universe() => sem.Universe()
      case GlobalReference(str) => sem.GlobalReference(str)
      case k =>
        evalCache.get(k) match {
          case Some(a) =>
            if (Debug && !debuggingInferCheck) delog(lstr() + "Eval cache hit")
            a
          case None =>
            var time = 0L
            var timeEmitted = 0L
            if (Debug) time = System.currentTimeMillis()
            val text = emitScala(term, -1)
            if (Debug) timeEmitted = System.currentTimeMillis() - time
            val twitterEval = new Eval()
            val res = twitterEval.apply[Value](text)
            if (Debug && !debuggingInferCheck) delog(lstr() + "Emitted in " + timeEmitted + ". Compiled in " + (System.currentTimeMillis() - time - timeEmitted) + ". " + text)
            if (k.closed()) evalCache.put(k, res)
            res
        }
    }
  }

  val evalCache = mutable.HashMap.empty[Term, Value]

  val DebugNbe = Debug && false
  def nbe(t: Term, testOnlyForceFullValue: Boolean = false) = {
    if (DebugNbe) delog("NbE: " + t)
    val e = eval(t, Seq.empty)
    val rb = readback(e, Seq.empty, testOnlyForceFullValue = testOnlyForceFullValue)
    if (DebugNbe) delog("\t" + rb)
    rb
  }
}



trait TypeCheck {

  import sem.force
  import normal._

  def global(g: GlobalReference) = sem.global(g.str)

  val closedTermMinimalTypeCache = mutable.Map.empty[Term, Value]


  // local typing context
  // the context is so that the head is index 0
  case class Context(ctx: TypingCtx) {

    def expand(v: Value) = {
      val pair = (sem.Generic(), v)
      this.copy(ctx = pair +: ctx)
    }

    def expand(pair: (sem.Generic, Value)) = this.copy(ctx = pair +: ctx)

    def head: (sem.Generic, Value) = ctx.head

    def debugContextStr() = ctx.reverse.map(k => k._1.name +":" + readback(k._2, ctx, isDebug = true)).mkString(" __ ")

    def local(l: LocalReference): Value = { val res = ctx(l.big)._2; if (res == sem.Poison) throw new Exception("I am poisoned") else res }

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
          def fixCheckType(a: Term) = {
            expand(sem.Universe()).checkIsType(a)
            sem.Universe()
          }
          t match {
            case Ascription(tt, ty) =>
              val vty = expand(sem.Poison).checkIsTypeAndEval(ty)
              expand(vty).check(tt, vty)
              vty
            case Lambda(Some(is), Ascription(tt, ty)) =>
              // some fishy thing going on here... but it is ok...
              // NOTICE: instead of remembering the context in a value like in miniTT,
              // we use a different approach, make sure this in the checker level,
              // this can make the eval (read back ) thing works, some variables once
              // bounded to
              val pos = expand(sem.Poison)
              val pty = pos.checkIsTypeAndEval(is)
              val ct = pos.expand(pty)
              val vty = ct.checkIsTypeAndEval(ty)
              val ttt = eval(Pi(readback(pty, pos.ctx), readback(vty, ct.ctx)), pos.ctx)
              // if they are referring some generic, it is ok to expand here, they
              // will get rebind to them correctly after read back
              expand(ttt).expand(ct.head).check(tt, vty)
              ttt
            case a: Sum =>
              fixCheckType(a)
            case a: Pi =>
              fixCheckType(a)
            case a: Sigma =>
              fixCheckType(a)
            case unknown => expand(sem.Poison).infer(unknown)
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
              right(checkAndEval(rs, left))
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
              join(ts.toSeq.map(a => {
                val at = a._2
                val term = right(a._1)
                expand(at).infer(term)
              }))
            case _ => throw new Exception("Cannot infer Split")
          }
        case Pi(is, to) =>
          expand(checkIsTypeAndEval(is)).checkIsType(to)
          sem.Universe()
        case Sum(ts) =>
          ts.values.foreach(k => checkIsType(k))
          sem.Universe()
        case Sigma(_, ts) =>
          // in miniTT... ESig p a b -> checkT k rho gma (EPi p a b)
          // they replaced with a Pi rule.. interesting...
          ts.foldLeft(this) { (c, t) =>
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
          assert(subtypeOf(sem.global(g).stype, ty)) // early return!!!
          delog(lstr() + "Checked global reference " + g + " with " + readback(ty, ctx, isDebug = true))
          return
        case _ => Unit // drop out
      }
      if (term.closed()) {
        closedTermMinimalTypeCache.get(term) match {
          case Some(a) =>
            assert(subtypeOf(a, ty))
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
            assert(subtypeOf(infer(term, debugFromCheck = true), p))
          } else {
            val gs = sem.Generic()
            val t = inside(gs)
            expand((gs, left)).check(body, t)
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
          /*
          assert(subseqOf(ms0, ms))
          val size = ms.size
          var i = 0
          var srh = ts0
          while (i < size) { // check i-th type
            if (ms0.contains(ms(i))) {
              val v = checkAndEval(ts(i), srh.t)
              expand(v)
              srh = srh.f(v)
            }
            i += 1
          }*/
        case (e, t) =>
          if (!debuggingInferCheck) assert(subtypeOf(infer(e, debugFromCheck = true), t))
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
    def checkIsType(t: Term): Unit = assert(subtypeOf(infer(t), sem.Universe()))



    // our subtyping seems to be no Top... only Bottom? now?

    def subseqOf(a: Seq[String], b: Seq[String]): Boolean = {
      if (a.isEmpty) true
      else if (b.isEmpty) false
      else if (a.head == b.head) subseqOf(a.tail, b.tail)
      else subseqOf(a, b.tail)
    }

    def merge(a: Seq[String], b: Seq[String]): Seq[String] = {
      if (a.isEmpty) b
      else if (b.isEmpty) a
      else if (a.head == b.head) a.head +: merge(a.tail, b.tail)
      else {
        val aa = a.head; val bb = b.head; val (s, m) = if (aa > bb) (bb, aa) else (aa, bb)
        s +: m +: merge(a.tail, b.tail)
      }
    }

    def subtypeOf(th: Value, o: Value): Boolean = {
      if (Debug && !debuggingInferCheck) delog(lstr() + "subtype called...")
      if (th eq o) return true
      (th, o) match {
        case (sem.Fix(t0), sem.Fix(t1)) =>
          // TODO(low) there are situations Fix(Fix), Fix()... but I don't care now? same for meet and join
          val k = Generic()
          subtypeOf(t0(k), t1(k))
        case (sem.Fix(t), a) =>
          subtypeOf(t(th), a)
        case (a, sem.Fix(t)) =>
          subtypeOf(a, t(o))

        case (sem.GlobalReference(g1), sem.GlobalReference(g2)) =>
          if (g1 == g2) true
          else subtypeOf(sem.global(g1).svalue, sem.global(g2).svalue)
        case (sem.GlobalReference(g1), g2) =>
          subtypeOf(sem.global(g1).svalue, g2)
        case (g1, sem.GlobalReference(g2)) =>
          subtypeOf(g1, sem.global(g2).svalue)

        case (sem.Pi(vl, vs), sem.Pi(vl1, vs1)) =>
          // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
          subtypeOf(vl1, vl) && { val g = Generic(); subtypeOf(vs(g), vs1(g)) }
        case (sem.Sum(ts), sem.Sum(ts1)) =>
           // assuming nat <: integer, we have sum[#a nat] <: sum[#a integer, #b type]
          if ((ts.keySet -- ts1.keySet).isEmpty) {
            ts.forall(pair => subtypeOf(pair._2, ts1(pair._1)))
          } else false
//        case (s@sem.Sigma(_, _), s1@sem.Sigma(_, _)) =>
//          // assuming nat <: integer we have [nat, nat] <: [integer]
//          meet(s, s1) == s
        case (a, b) => a == b // these are cases for Universe, Lambda etc
      }
    }

    // meet
    // c = a /\ b
    // then c <: a and c <: b
    def meet(th: Value, o: Value): Value = {
      if (th eq o) return th
      (th, o) match {
        case (sem.Fix(t0), sem.Fix(t1)) =>
          val k = Generic()
          meet(t0(k), t1(k))
        case (sem.Fix(t), a) =>
          meet(t(th), a)
        case (a, sem.Fix(t)) =>
          meet(a, t(o))

        case (g@sem.GlobalReference(g1), sem.GlobalReference(g2)) =>
          if (g1 == g2) g
          else meet(sem.global(g1).svalue, sem.global(g2).svalue)
        case (sem.GlobalReference(g1), g2) =>
          meet(sem.global(g1).svalue, g2)
        case (g1, sem.GlobalReference(g2)) =>
          meet(g1, sem.global(g2).svalue)

        case (p0@sem.Pi(left, inside), p1@sem.Pi(left1, inside1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
          val l = join(left, left1) // we need a parameter that can accept both of the parameters of them
          val g = (Generic(), Generic())
          val ex = expand(g)
          val m = ex.meet(inside(g._1), inside1(g._1))
          eval(Pi(readback(l, ctx), readback(m, ex.ctx)), ctx)

        case (sem.Sum(ts), sem.Sum(ts1)) => // assuming nat <: integer, we have sum[#a nat] <: sum[#a integer, #b type]
          val keys = ts.keySet intersect ts1.keySet
          sem.Sum(keys.map(a => (a, meet(ts(a), ts1(a)))).toMap)

//        case (s0@sem.Sigma(ms0, ts0), s1@sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]
//          // assuming nat <: integer we have [nat, nat] <: [integer]
//          val cx = merge(ms0, ms1)

        case (a, b) => if (a == b) a else sem.Bottom
      }
    }

    // join
    def join(th: Value, o: Value): Value = {
      if (th eq o) return th
      (th, o) match {
        case (sem.Fix(t0), sem.Fix(t1)) =>
          val k = Generic()
          join(t0(k), t1(k))
        case (sem.Fix(t), a) =>
          join(t(th), a)
        case (a, sem.Fix(t)) =>
          join(a, t(o))

        case (g@sem.GlobalReference(g1), sem.GlobalReference(g2)) =>
          if (g1 == g2) g
          else join(sem.global(g1).svalue, sem.global(g2).svalue)
        case (sem.GlobalReference(g1), g2) =>
          join(sem.global(g1).svalue, g2)
        case (g1, sem.GlobalReference(g2)) =>
          join(g1, sem.global(g2).svalue)

        case (p0@sem.Pi(left, inside), p1@sem.Pi(left1, inside1)) => // assuming nat <: integer, we have integer => nat subtypeOf nat => integer
          val l = meet(left, left1) // we need a parameter that can accept both of the parameters of them
        val g = (Generic(), Generic())
          val ex = expand(g)
          val m = ex.join(inside(g._1), inside1(g._1))
          eval(Pi(readback(l, ctx), readback(m, ex.ctx)), ctx)

//        case (s0@sem.Sigma(ms0, ts0), s1@sem.Sigma(ms1, ts1)) => // assuming nat <: integer we have sigma[@a nat, @b type] <: [@a integer]

        case (sem.Sum(ts), sem.Sum(ts1)) => // assuming nat <: integer, we have sum[#a nat] <: sum[#a integer, #b type]
          val keys = ts.keySet intersect ts1.keySet
          sem.Sum(keys.map(a => (a, meet(ts(a), ts1(a)))).toMap)

        case (a, b) => if (a == b) a else throw new Exception("No")
      }
    }



    def join(seq: Seq[Value]): Value = {
      if (seq.isEmpty) sem.Bottom
      else seq.tail.fold(seq.head) { (v0, v1) => join(v0, v1)}
    }

    def meet(seq: Seq[Value]): Value = {
      assert(seq.nonEmpty)
      seq.tail.fold(seq.head) { (v0, v1) => meet(v0, v1) }
    }
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

  import normal._
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

  def sigma(t: Object*) = {
    val k = t.grouped(2).toSeq
    Sigma(k.map(_.head.asInstanceOf[String]), k.map(_(1).asInstanceOf[Term]))
  }

  def record(t: Object*) = {
    val k = t.grouped(2).toSeq
    Record(k.map(_.head.asInstanceOf[String]), k.map(_(1).asInstanceOf[Term]))
  }

  def sum(t: Object*) = {
    val k = t.grouped(2).toSeq.map(a => (a.head.asInstanceOf[String], a(1).asInstanceOf[Term])).toMap
    Sum(k)
  }

  def con(s: String, t: Term) = Construct(s, t)

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

  def fails(a: () => Any, reason: String = "") = {
    Try(a()) match {
      case Success(_) => throw new Exception("Should fail!")
      case Failure(_) =>
        level = 0
        println("\n\nYes!!! It failed!!!! " + reason + "\n\n")
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

  val silly_app = debugDefine("silly_app",
    lam(pi(unit, unit), unit, a(Ascription(Lambda(None, a(rr(1, 0), r(0))), pi(unit, unit)), rr(0, 1))),
    pi(pi(unit, unit), unit, unit)
  )

  // lambda (x: type) = fix self => lambda (y: x) => y as x
  val silly_fix = debugDefine("silly_fix",
    lam(u, fix(lam(r(1), Ascription(r(0), r(2))))),
    pi(u, pi(r(0), r(1)))
  )

  // lambda (x: type, y: x) => fix self => (y as x)
  val silly_asc = debugDefine("silly_asc",
    lam(u, r(0), fix(Ascription(rr(1, 1), rr(1, 0)))),
    pi(u, r(0), rr(0, 0))
  )

  // lambda (x: type, tf: type => type, ff: x => (tf x), b: x): (tf x) = fix self => (ff b)
  val id_2 = debugDefine("id_2",
    lam(u, pi(u, u), pi(r(1), a(r(1), r(2))), r(2), fix(a(rr(1, 2), rr(1, 3)))),
    pi(u, pi(u, u), pi(r(1), a(r(1), r(2))), r(2), a(rr(0, 1), rr(0, 0)))
  )

  // fix self => sum(zero: unit, succ: self)
  val nat = debugDefine("nat",
    fix(Sum(Map("zero" -> unit, "succ" -> r(0)))),
    u
  )

  val unit_pair = debugDefine("unit_pair",
    sigma("a", unit, "b", unit),
    u
  )

  val unit_pair_1_1 = debugDefine("unit_pair_1_1",
    record("a", unit0, "b", unit0),
    unit_pair
  )


//  val unit_pair_1_1_alt = debugDefine("unit_pair_1_1_alt",
//    record("a", unit0, "b", unit0),
//    sigma("a", unit)
//  )
//
//  val unit_pair_1_1_alt1 = debugDefine("unit_pair_1_1_alt1",
//    record("a", unit0, "b", unit0),
//    sigma("b", unit)
//  )

  fails(() => {
    debugDefine("fails",
      record("a", unit0, "b", unit0),
      sigma("c", unit)
    )
  })

  fails(() => {
    debugDefine("fails",
      record("x", unit0),
      sigma("x", nat)
    )
  })



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


  Debug = true
  // lam n: nat => lam(y: (int => int) => {x: type, y: x}): (y (lam x => n)).x = y(lam x => n).y
  val test_lam_eq = debugDefine("test_lam_eq",
    lam(nat, lam(pi(pi(nat, nat), sigma("x", u, "y", r(0))), Projection(a(r(0), Lambda(None, r(2))), "y"))),
    pi(nat, pi(pi(pi(nat, nat), sigma("x", u, "y", r(0))), Projection(a(r(0), Lambda(None, r(2))), "x")))
  )



  

  val n0 = debugDefine("n0", Construct("zero", unit0), nat)
  val n1 = debugDefine("n1", Construct("succ", n0), nat)
  val n2 = debugDefine("n2", Construct("succ", n1), nat)
  val n3 = debugDefine("n3", Construct("succ", n2) , nat)
  val n4 = debugDefine("n4", Construct("succ", n3) , nat)
  val n5 = debugDefine("n5", Construct("succ", n4) , nat)
  val n6 = debugDefine("n6", Construct("succ", n5) , nat)
  val n7 = debugDefine("n7",  Construct("succ", n6) , nat)
  val n8 = debugDefine("n8",  Construct("succ", n7) , nat)
  val n9 = debugDefine("n9", Construct("succ", n8) , nat)
  val n10 = debugDefine("n10", Construct("succ", n9) , nat)
  val n11 = debugDefine("n11", Construct("succ", n10) , nat)
  val n12 = debugDefine("n12", Construct("succ", n11) , nat)
  val n13 = debugDefine("n13", Construct("succ", n12) , nat)
  val n14 = debugDefine("n14", Construct("succ", n13) , nat)
  val n15 = debugDefine("n15", Construct("succ", n14), nat)
  val n16 = debugDefine("n16", Construct("succ", n15) , nat)



  val lam_nat_always_nat = debugDefine("lam_nat_always_nat",
    lam(nat, nat),
    pi(nat, u)
  )

  val lam_silly = debugDefine("lam_silly",
    fix(lam(nat, a(lam_nat_always_nat, n0))),
    pi(nat, u)
  )

  fails(() => {
    debugDefine("fails",
      fix(lam(nat, a(lam_nat_always_nat, Ascription(n0, a(r(1), n0))))),
      pi(nat, u)
    )
  })

  val n0t16 = Seq(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)

  val succ = debugDefine("succ",
    lam(nat, Construct("succ", r(0))),
    pi(nat, nat)
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

  assert(fff(a(pair, nat, nat)) == fff(Sigma(Seq("_0", "_1"), Seq(nat, nat))))


  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self k b) }
  val plus = debugDefine("plus",
    fix(lam(nat, nat, Ascription(Split(rr(0, 0), Map("zero" -> rr(1, 1), "succ" -> Construct("succ", a(r(2), r(0), rr(1, 1))))), nat))),
    pi(nat, nat, nat)
  )


  // fix self => (a, b: nat) => split a { case zero => b; case succ k => succ(self b k) }
  val plus1 = debugDefine("plus1",
    fix(lam(nat, nat, Ascription(Split(rr(0, 0), Map("zero" -> rr(1, 1), "succ" -> Construct("succ", a(r(2), rr(1, 1), r(0))))), nat))),
    pi(nat, nat, nat)
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
  val mult = fix(lam(nat, nat, Split(rr(0, 0), Map("zero" -> n0, "succ" -> a(plus, rr(1, 1), a(r(2), r(0), rr(1, 1)))))))
  val mult1 = fix(lam(nat, nat, Split(rr(0, 0), Map("zero" -> n0, "succ" -> a(plus, rr(1, 1), a(r(2), rr(1, 1), r(0)))))))

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
