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

  abstract class Command() {
    def accept(s: String): Boolean
  }

  case class ConstantCommand(s: String) extends Command {
    def accept(a: String) = a == s
  }

  case class AcceptanceCommand(s: String => Boolean) extends Command {
    def accept(a: String) = s(a)
  }

  case class ToWidget(cap: Int, fun: Seq[Widget] => Widget)

  type ToAst = (String, Seq[T]) => T

  case class SyntaxSort(name: String, var forms: Seq[SyntaxForm] /* var only to construct cyclic reference */)

  case class SyntaxForm(command: Command, childs: Seq[SyntaxSort], toLayout: ToWidget, toAst: ToAst)

  def SyntaxFormConstant(name: String, t: T) =
    SyntaxForm(ConstantCommand(name), Seq.empty, layouts.Inline1, (_, _) => t)

  def SyntaxFormApplicative1(name: String, c: SyntaxSort, toAst: ToAst) =
    SyntaxForm(ConstantCommand(name), Seq(c), layouts.Inline2, toAst)

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
      val placeholderText = if (tree.commandBuffer.nonEmpty) tree.commandBuffer else if (tree.content.isEmpty) "?" else ""
      val text = if (placeholderText.nonEmpty) {
        color = PlaceholderColor
        placeholderText
      } else if (replacement.nonEmpty) replacement else tree.command
      measure0(text)
    }
  }

  object layouts {
    val Inline1 = ToWidget(0, seq => WCommand())
    val Inline2 = ToWidget(1, seq => WSequence(WCommand(), WConstant(" "), seq.head))
    val Default = ToWidget(-1, seq => WVertical(seq: _*))
  }


  class Tree(var content: Option[SyntaxForm]) {

    var command: String = ""
    val childs: mutable.Buffer[Tree] = mutable.ArrayBuffer.empty

    var parent: Option[Tree] = None

    // reset each time go out insert mode
    var commandBuffer = ""

    // reset each time re-render
    var commandLayout: WCommand = null
    var layout: Widget = null

    def measure(widthHint: Float): Unit = {
      // TODO not used now
      commandLayout = null
      val transformer = content.map(_.toLayout).getOrElse(layouts.Inline1)
      val cwidgets = childs.map(a => {
        a.measure(widthHint)
        a.layout
      })
      layout = transformer.fun(cwidgets)
      if (transformer.cap >= 0 && transformer.cap < childs.size) {
        layout = layouts.Default.fun(Seq(layout, WSequence(WIndent, WVertical(cwidgets.drop(transformer.cap): _*))))
      }
      // this will measure the rest of the elements just created by the transformer
      // also one child might set the command layout property
      layout.measure(this, 0, 0)
    }

    def ast(): (T, Seq[Error]) = {
      val ret = if (content.isEmpty) {
        (newHole(), Seq.empty)
      } else {
        val c = content.get
//        def sortMismatchError(tree: Tree, s: SyntaxSort) = {
//          Error(tree, "sort mismatch, expecting " + s.name)
//        }
       val cast: Seq[(T, Seq[Error])] = childs.take(if (c.toLayout.cap>= 0) c.toLayout.cap else childs.size).map(_.ast())
        val remain = if (c.toLayout.cap >= 0) childs.drop(c.toLayout.cap) else Seq.empty
        val ast = c.toAst.apply(command, cast.map(_._1))
        val errors = cast.flatMap(_._2) ++ remain.map(a => Error(a, "redundant term"))
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
      content = c.content
      command = c.command
    }

    def moveContentOut(): Tree = {
      val cc = new Tree(content)
      content = None
      cc.command = this.command
      this.command = ""
      cc.childs ++= childs.map(t => t.copy(Some(cc)))
      childs.clear()
      cc
    }

    def copy(p: Option[Tree]): Tree = {
      val cc = new Tree(content)
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

    def linearizedNextInSiblings(pred: Tree => Boolean): Option[Tree] = {
      parent match {
        case Some(p) =>
          val i = p.childs.indexOf(this)
          var k = i + 1
          while (k < p.childs.size) {
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
        val index = p.childs.indexOf(this)
        if (index == 0) {
          Some(p)
        } else {
          val s = p.childs(index - 1)
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
      childs.remove(childs.indexOf(t))
    }
  }

}
