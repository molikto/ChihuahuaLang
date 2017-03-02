package org.snailya.mygame

/**
  * Created by molikto on 21/02/2017.
  */

import MyGame._
import MyGame.game._
import com.badlogic.gdx.{Gdx, InputProcessor}
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.GlyphLayout
import com.badlogic.gdx.scenes.scene2d.ui.TextField
import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldStyle

import scala.collection.JavaConverters._
import scala.collection.mutable

abstract class AstBaseWithPositionData {
  var data: Object = null
  // this is a hack now, in case of textural language, it will be something like (line, char)...
}

// base class for a frontend of a programming language with AST type T and a subtype of T which is a hole
// we don't support multi-sorted language, this makes little sense now because we want to support mal-shaped ast
trait LanguageFrontend[T <: AstBaseWithPositionData, H <: T] extends LanguageFrontendStyle {

  case class Error(t: Object, s: String) {
    def cast: Tree = t.asInstanceOf[Tree] // this is the only type cast we use now
  }

  val Lang: Language

  def newHole(): H

  def compile(l: T): Either[String, Seq[Error]]


  case class Acceptance(eager: Boolean)

  abstract class Command() {
    def accept(s: String): Option[Acceptance]
  }

  case class ConstantCommand(s: String, autoCreate: Boolean = false, acc: Acceptance = Acceptance(false)) extends Command {
    def accept(a: String) = if (a == s) Some(acc) else None
  }

  case class AcceptanceCommand(s: String => Option[Acceptance]) extends Command {
    def accept(a: String) = s(a)
  }

  type ToWidget = Seq[Widget] => Widget

  type ToAst = (String, Seq[T]) => (T, Seq[Error])

  case class SyntaxSort(name: String, var forms: Seq[SyntaxForm] /* var only to construct cyclic reference */)

  case class ChildRelationship(sort: SyntaxSort, min: Int, max: Int, init: Int,
    sepCommand: Option[Char] = None,
    createCommand: Option[Char] = None) {
    assert(max > 0 && min >= 0)

    assert(max != min || (sepCommand.isEmpty && createCommand.isEmpty))
  }

  def ChildRelationshipFixed(sort: SyntaxSort, c: Int) = ChildRelationship(sort, c, c, c)

  val MAX_BRANCH = Integer.MAX_VALUE / 100

  case class SyntaxForm(command: Command,
    childs: Seq[ChildRelationship],
    toLayout: ToWidget,
    toAst: ToAst) {

    assert (childs.count(a => a.min != a.max) <= 1)

    val min = childs.map(_.min).sum

    val max = childs.map(_.max).sum

    val varargsPosition = childs.indexOf((k: ChildRelationship) => k.max != k.min)

    val isFixed = varargsPosition == -1

    val headerSize = if (isFixed) max else childs.take(varargsPosition).map(_.max).sum

    val footerSize = if (isFixed) 0 else childs.drop(varargsPosition + 1).map(_.max).sum


    def relation(index: Int, size: Int): Option[(ChildRelationship, Int)] = if (index >= max) {
      None
    } else if (index < headerSize) {
      var c = 0
      var j = 0
      while (index >= c) {
        c += childs(j).max
        j += 1
      }
      Some((childs(j - 1), index))
    } else if (index - headerSize < size - footerSize) {
      Some((childs(varargsPosition), index - headerSize))
    } else {
      val findex = index - (size - footerSize)
      var c = 0
      var j = varargsPosition + 1
      while (findex >= c) {
        c += childs(j).max
        j += 1
      }
      Some((childs(j - 1), findex))
    }


    def sort(index: Int, size: Int): Option[SyntaxSort] = relation(index, size).map(_._1.sort)
  }


  def SyntaxFormConstant(name: String, t: T) =
    SyntaxForm(ConstantCommand(name), Seq.empty, layouts.Inline1, (_, _) => (t, Seq.empty))

  def SyntaxFormApplicative1(name: String, c: SyntaxSort, toAst: ToAst) =
    SyntaxForm(ConstantCommand(name), Seq(ChildRelationshipFixed(c, 1)), layouts.Inline2, toAst)

  case class Language(sorts: Seq[SyntaxSort], forms: Seq[SyntaxForm]) {
  }

  abstract class Widget() {

    var x = 0f
    var y = 0f
    var width = 0f
    var height = 0f
    var measured: Boolean = false
    var bg: Color = null // this is set after the draw call

    def measure(t: Tree, x: Float, y: Float) = {
      this.x = x
      this.y = y
      if (!measured) {
        this.measured = true
        measure0(t)
      }
    }

    var absX: Float = 0
    var absY: Float = 0

    def draw(px: Float, py: Float) = {
      absX = px + x
      absY = py + y
      if (bg != null) drawColor(px + x, py + y, width, height, bg)
    }


    def measure0(tree: Tree)
  }

  def drawChildren(px: Float, py: Float, t: Widget, seq: Widget*) = {
    for (s <- seq) {
      s.draw(px + t.x, py + t.y)
    }
  }

  abstract class WIndentAbs extends Widget {
    override def measure0(t: Tree) = {
      width = ItemIndent
      height = 0
    }
  }

  object WIndent extends WIndentAbs

  case class WSequence(seq: Widget*) extends Widget {
    override def measure0(tree: Tree) = {
      var x = 0f
      for (s <- seq) {
        s.measure(tree, x, 0)
        x += s.width
      }
      width = x
      height = if (seq.isEmpty) 0 else seq.map(_.height).max
    }

    override def draw(px: Float, py: Float) = {
      super.draw(px, py)
      drawChildren(px, py, this, seq: _*)
    }
  }

  case class WVertical(seq: Widget*) extends Widget {
    override def measure0(tree: Tree) = {
      var y = 0f
      for (s <- seq) {
        s.measure(tree, 0, y)
        y += s.height
      }
      height = y
      width = if (seq.isEmpty) 0 else seq.map(_.width).max
    }

    override def draw(px: Float, py: Float) = {
      super.draw(px, py)
      drawChildren(px, py, this, seq: _*)
    }
  }

  abstract class WGlyph() extends Widget {
    var gl: GlyphLayout = null
    var text: String = null
    var color: Color = Color.WHITE

    def measure0(s: String) = {
      gl = Font.measure(s)
      text = s
      width = gl.width
      height = Font.lineHeight
    }

    override def draw(px: Float, py: Float) = {
      super.draw(px, py)
      Font.draw(px + x, py + y, text, color)
    }
  }

  case class WConstant(c: String) extends WGlyph {
    override def measure0(tree: Tree) = measure0(c)
  }

  case class WCommand(replacement: String = "") extends WGlyph {

    override def measure0(tree: Tree) = {
      tree.commandLayout = this
      val placeholderText = if (tree.commandBuffer.nonEmpty) tree.commandBuffer else if (tree.form.isEmpty) "?" else ""
      val text = if (placeholderText.nonEmpty) {
        color = PlaceholderColor
        placeholderText
      } else if (replacement.nonEmpty) replacement else tree.command
      measure0(text)
    }
  }

  object layouts {
    val Inline1: ToWidget = seq => WCommand()
    val Inline2: ToWidget = seq => WSequence(WCommand(), WConstant(" "), seq.head)
    val Default: ToWidget = seq => WVertical(seq: _*)
  }


  def emptyError[T](t: T) = (t, Seq.empty[Error])

  def mismatchError(t: AstBaseWithPositionData, s: SyntaxSort) = Seq(Error(t.data, "syntax sort mismatch, expecting " + s.name))


  class Tree(var form: Option[SyntaxForm]) extends (Int => Tree) {

    var command: String = ""
    val childs: mutable.Buffer[Tree] = mutable.ArrayBuffer.empty

    var parent: Option[Tree] = None

    // reset each time go out insert mode
    var commandBuffer = ""

    // reset each time re-render
    // so don't need to care about these in edit mode
    var commandLayout: WCommand = null
    var layout: Widget = null

    override def toString: String = toString(0)

    def toString(indent: Int): String = {
      "                          ".take(indent) + command +
        (if (size > 0) "\n" + childs.map(_.toString(indent + 2)).mkString("\n") else "")
    }

    def measure(widthHint: Float): Unit = {
      // TODO not used now
      commandLayout = null
      val (min, max, transformer) = form.map(a => (a.min, a.max, a.toLayout)).getOrElse((0, 0, layouts.Inline1))
      val wanted = childs.take(max)
      val redandant = childs.drop(max)
      layout = transformer(wanted.map(a => {a.measure(widthHint); a.layout}))
      if (max < size) {
        layout = layouts.Default(Seq(layout, WSequence(WIndent, WVertical(redandant.map(a => {a.measure(widthHint); a.layout}): _*))))
      }
      // this will measure the rest of the elements just created by the transformer
      // also one child might set the command layout property
      layout.measure(this, 0, 0)
    }

    def ast(): (T, Seq[Error]) = {
      val ret = if (form.isEmpty) {
        (newHole(), Seq.empty)
      } else {
        val c = form.get
        val cast: Seq[(T, Seq[Error])] = childs.take(c.max).map(_.ast())
        val remain = childs.drop(c.max)
        val (ast, nerrors) = c.toAst.apply(command, cast.map(_._1))
        val errors = cast.flatMap(_._2) ++ remain.map(a => Error(a, "redundant term")) ++ nerrors
        (ast, errors)
      }
      ret._1.data = this
      ret
    }

    def copyContent(c: Tree): Unit = {
      childs.clear()
      for (cc <- c.childs) {
        cc.parent = None
        append(cc)
      }
      c.childs.clear()
      form = c.form
      command = c.command
    }

    def apply(i: Int) = childs(i)

    def moveContentAndChildOut(): Tree = {
      val cc = new Tree(form)
      form = None
      cc.command = this.command
      this.command = ""
      cc.childs ++= childs.map(t => t.copy(Some(cc)))
      childs.clear()
      cc
    }

    def copy(p: Option[Tree]): Tree = {
      val cc = new Tree(form)
      cc.parent = p
      cc.command = this.command
      cc.childs ++= childs.map(t => t.copy(Some(cc)))
      cc
    }

    def find(pred: Tree => Boolean): Option[Tree] = {
      if (pred(this)) {
        Some(this)
      } else {
        for (c <- childs) {
          val res = c.find(pred)
          if (res.nonEmpty) return res
        }
        None
      }
    }

    def clear() = childs.clear()

    def head = childs.head

    def headOption = childs.headOption

    def nonEmpty = childs.nonEmpty

    def isEmpty = childs.isEmpty

    def indexOf(t: Tree) = childs.indexOf(t)

    def size = childs.size

    def linearizedNextInSiblings(pred: Tree => Boolean): Option[Tree] = {
      parent match {
        case Some(p) =>
          val i = p.indexOf(this)
          var k = i + 1
          while (k < p.size) {
            val res = p.childs(k).find(pred)
            if (res.nonEmpty) return res
            k += 1
          }
          None
        case None =>
          None
      }
    }

    def last(): Tree = {
      if (childs.isEmpty) this
      else childs.last.last()
    }

    def linearizedPrevious(): Option[Tree] = {
      if (parent.isEmpty) {
        None
      } else {
        val p = parent.get
        val index = p.indexOf(this)
        if (index == 0) {
          Some(p)
        } else {
          val s = p(index - 1)
          Some(s.last())
        }
      }
    }

    def linearizedNext(): Option[Tree] = linearizedNext(_ => true)

    def linearizedNext(pred: Tree => Boolean): Option[Tree] = {
      for (c <- childs) {
        val res = c.find(pred)
        if (res.nonEmpty) return res
      }
      var c = this
      while (c.parent.nonEmpty) {
        val res = c.linearizedNextInSiblings(pred)
        if (res.nonEmpty) return res
        c = c.parent.get
      }
      None
    }

    def copy(): Tree = copy(parent)

    def append(c: Tree) = {
      assert(c.parent.isEmpty)
      c.parent = Some(this)
      childs.append(c)
    }

    def insert(i: Int, c: Tree) = {
      assert(c.parent.isEmpty)
      c.parent = Some(this)
      childs.insert(i, c)
    }

    def appendNew(): Tree = {
      val c = new Tree(None)
      c.parent = Some(this)
      childs.append(c)
      c
    }

    def remove(t: Tree): Unit = {
      assert(t.parent.get == this)
      t.parent = None
      childs.remove(indexOf(t))
    }
  }

}
