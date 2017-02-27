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

import scala.collection.mutable

class ScreenMain extends ScreenBase {


  /**
    * style
    */
  object style {
    val BackgroundColor = new Color(0x2b303bFF)
    val SelectionColor = new Color(0xFFFFFF33)
    val PlaceholderColor = new Color(0xFFFFFF77)
    val Size8 = size(8)
    val ItemIndent = size(20)
  }

  import style._

  /**
    * Keys
    */


  case class SyntaxSort(name: String, var forms: Seq[SyntaxForm] /* var only to construct cyclic reference */)

  case class SyntaxForm(command: String, specs: Seq[SyntaxSort], transformer: LayoutTransformer)

  case class Language(sorts: Seq[SyntaxSort], forms: Seq[SyntaxForm])

  abstract class Widget() {

    var x = 0f
    var y = 0f
    var width = 0f
    var height = 0f
    var measured: Boolean = false

    def measure(t: Tree, x: Float, y: Float) = {
      this.x = x
      this.y = y
      if (!measured) {
        this.measured = true
        measure0(t)
      }
    }

    def draw(px: Float, py: Float)


    def measure0(tree: Tree)
  }

  def drawChildren(px: Float, py: Float, t: Widget, seq: Widget*) = {
    for (s <- seq) {
      s.draw(px + t.x, py + t.y)
    }
  }

  case class LayoutTransformer(cap: Int, fun: Seq[Widget] => Widget)

  case object LIndent extends Widget {
    override def measure0(t: Tree) = {
      width = ItemIndent
      height = Font.lineHeight
    }

    override def draw(px: Float, py: Float) = {}
  }
  case class LSequence(seq: Widget*) extends Widget {
    override def measure0(tree: Tree) = {
      var x = 0f
      for (s <- seq) {
        s.measure(tree, x, 0)
        x += s.width
      }
      width = x
      height = seq.map(_.height).max
    }

    override def draw(px: Float, py: Float) = drawChildren(px, py, this, seq: _*)
  }
  case class LVertical(seq: Widget*) extends Widget {
    override def measure0(tree: Tree) = {
      var y = 0f
      for (s <- seq) {
        s.measure(tree, 0, y)
        y += s.height
      }
      height = y
      width = seq.map(_.width).max
    }

    override def draw(px: Float, py: Float) = drawChildren(px, py, this, seq: _*)
  }

  abstract class LGlyph() extends Widget {
    var gl: GlyphLayout = null
    var text: String = null
    var color: Color = Color.WHITE

    def measure0(s: String) = {
      gl = Font.measure(s)
      text = s
      width = gl.width
      height = Font.lineHeight
    }

    override def draw(px: Float, py: Float) = Font.draw(px + x, py + y, text, color)
  }

  case class DConstant(c: String) extends LGlyph {
    override def measure0(tree: Tree) = measure0(c)
  }

  case class LCommand() extends LGlyph {


    var bg: Color = null // this is set after the draw call

    override def measure0(tree: Tree) = {
      tree.commandLayout = this
      val placeholderText = if (tree.commandBuffer.nonEmpty) tree.commandBuffer else if (tree.content.isEmpty) "?" else ""
      val text = if (placeholderText.nonEmpty) {
        color = PlaceholderColor
        placeholderText
      } else tree.content.get.command
      measure0(text)
    }

    override def draw(px: Float, py: Float) = {
      if (bg != null) drawColor(px + x, py + y, width, height, SelectionColor)
      super.draw(px, py)
    }
  }

  val LayoutInline1 = LayoutTransformer(0, seq => LCommand())
  val LayoutInline2 = LayoutTransformer(1, seq => LSequence(LCommand(), seq.head))
  val LayoutDefault = LayoutTransformer(-1, seq => LVertical(seq: _*))

  def SyntaxForm1(name: String) = SyntaxForm(name, Seq.empty, LayoutInline1)
  def SyntaxForm2(name: String, c: SyntaxSort) = SyntaxForm(name, Seq(c), LayoutInline2)

  object UAE {

    val Term = SyntaxSort("term", null)

    val True = SyntaxForm1("true")
    val False = SyntaxForm1("false")
    val IfThenElse = SyntaxForm("if", Seq(Term, Term, Term),
      LayoutTransformer(3, (seq) => {
        LVertical(
          LSequence(LCommand(),  seq(0)),
          LSequence(LIndent, seq(1)),
          DConstant("else"),
          LSequence(LIndent, seq(2))
        )
      })
    )
    val Zero = SyntaxForm1("0")
    val Succ = SyntaxForm2("succ", Term)
    val Pred = SyntaxForm2("pred", Term)
    val IsZero = SyntaxForm2("iszero", Term)

    Term.forms = Seq(True, False, IfThenElse, Zero, Succ, Pred, IsZero)

    val Lang = Language(Seq(Term), Term.forms)
  }

  class Tree(var content: Option[SyntaxForm]) {

    var commandBuffer = ""
    val childs: mutable.Buffer[Tree] = mutable.ArrayBuffer.empty
    var parent: Option[Tree] = None

    var width, height: Float = 0
    var commandLayout: LCommand = null

    def measure(widthHint: Float): Widget = {
      commandLayout = null
      val transformer = content.map(_.transformer).getOrElse(LayoutInline1)
      val cwidgets = childs.map(a => {a.measure(widthHint)})
      var layout = transformer.fun(cwidgets)
      if (transformer.cap >= 0 && transformer.cap < childs.size) {
        layout = LayoutDefault.fun(Seq(layout) ++ cwidgets.drop(transformer.cap))
      }
      // this will measure the rest of the elements just created by the transformer
      // also one child might set the command layout property
      layout.measure(this, 0, 0)
      layout
    }

    def copy(p: Option[Tree]): Tree = {
      val cc = new Tree(content)
      cc.parent = p
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

    def findSiblings(pred: Tree => Boolean): Option[Tree] = {
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

    def findAfterThis(pred: Tree => Boolean): Option[Tree] = {
      for (c <- childs) {
        val res = c.find(pred)
        if (res.nonEmpty) return res
      }
      var c = this
      while (c.parent.nonEmpty) {
        val res = c.findSiblings(pred)
        if (res != null) return res
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

    def appendNew() = {
      val c = new Tree(None)
      c.parent = Some(this)
      childs.append(c)
    }

    def remove(t: Tree): Unit = {
      assert(t.parent.get == this)
      t.parent = None
      childs.remove(childs.indexOf(t))
    }
  }

  object state {
    val Lang = UAE.Lang
    val root = new Tree(None)
    var selection: Option[Tree] = Some(root)
    var isInsert = false
    var clipboard: Option[Tree] = None
  }

  /**
    * UI
    */

  val Font = Roboto20



  def stateInsertAtNextHoleOrExit() = { // TODO make it better
    state.selection.get.commandBuffer = ""
    val res = state.selection.get.findAfterThis(_.content.isEmpty)
    if (res.nonEmpty) state.selection = res
    else {
      state.isInsert = false
    }
  }

  def stateCommitCommand(): Unit = {
    assert (state.selection.nonEmpty)
    val selection = state.selection.get
    assert (selection.content.isEmpty)
    val command = selection.commandBuffer
    selection.commandBuffer = ""
    if (command.isEmpty) {
      state.isInsert = false
    } else {
      state.Lang.forms.find(_.command == command) match {
        case Some(f) =>
          selection.content = Some(f)
          f.specs.foreach(_ => selection.appendNew())
          stateInsertAtNextHoleOrExit()
        case None =>
      }
    }
  }

  Gdx.input.setInputProcessor(new InputProcessor {

    override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int) = false

    override def mouseMoved(screenX: Int, screenY: Int) = false

    override def touchUp(screenX: Int, screenY: Int, pointer: Int, button: Int) = false

    override def touchDragged(screenX: Int, screenY: Int, pointer: Int) = false

    override def scrolled(amount: Int) = false

    override def keyDown(keycode: Int) = false

    override def keyUp(keycode: Int) = {
      if (state.isInsert) {
        if (keycode == Keys.ESCAPE) {
          if (state.selection.nonEmpty) {
            state.isInsert = false
            state.selection.get.commandBuffer = ""
            true
          }
        }
      }
      false
    }

    override def keyTyped(character: Char) = if (state.isInsert) {
      delog("key typed: " + Integer.toHexString(character.toInt))
      assert(state.selection.isDefined)
      val selected = state.selection.get
      if (selected.content.isEmpty) {
        if (character == ' ' || character == '\n') {
          stateCommitCommand()
        } else if (character == '\b') {
          if (selected.commandBuffer.nonEmpty) selected.commandBuffer = selected.commandBuffer.dropRight(1)
        } else if (character >= '!' && character <= '~') {
          // TODO valid commands and identifiers
          selected.commandBuffer = selected.commandBuffer + character
        }
      }
      true
    } else {
      character match {
        case 'i' => // enter insert mode
          state.isInsert = true
        case 'n' => // new empty node
          state.selection match {
            case Some(t) =>
              if (t.content.nonEmpty) {
                val c = new Tree(None)
                t.append(c)
                state.selection = Some(c)
              }
            case None =>
          }
        case 'k' => // go to parent
          state.selection match {
            case Some(t) =>
              state.selection = t.parent
            case _ =>
          }
        case 'j' => // go to first child
          state.selection match {
            case Some(t) =>
              if (t.childs.nonEmpty) state.selection = Some(t.childs.head)
            case _ => state.selection = Some(state.root)

          }
        case 'l' => // go to next sibling
          state.selection match {
            case Some(t) =>
              t.parent match {
                case Some(p) =>
                  val selection = Math.min(p.childs.indexOf(t) + 1, p.childs.size - 1)
                  state.selection = Some(p.childs(selection))
                case None =>
              }
            case _ =>
          }
        case 'h' => // go to previous sibling
          state.selection match {
            case Some(t) =>
              t.parent match {
                case Some(p) =>
                  val selection = Math.max(p.childs.indexOf(t) - 1, 0)
                  state.selection = Some(p.childs(selection))
                case None =>
              }
            case _ =>
          }
        case 'd' => // delete an item
          state.selection match {
            case Some(t) =>
              if (t.childs.nonEmpty || t.content.nonEmpty) {
                val tt = new Tree(t.content)
                tt.childs ++= t.childs
                state.clipboard = Some(tt)
                t.content = None
                t.childs.clear()
              } else {
                t.parent match {
                  case Some(p) =>
                    p.remove(t)
                    state.selection = Some(p)
                  case None =>
                }
              }
            case None =>
          }
        case 'p' => // paste an item
          state.selection match {
            case Some(t) =>
              state.clipboard match {
                case Some(c) =>
                  val cc = c.copy()
                  t.append(cc)
                  state.selection = Some(cc)
                case None =>
              }
            case None =>
          }
        case _ =>
      }
      true
    }


  })



  override def render(delta: Float) = {
    clearColor(BackgroundColor)
    begin()
    val layout = state.root.measure(screenPixelWidth - Size8 * 2)
    state.selection.foreach(a => a.commandLayout.bg = SelectionColor)
    layout.draw(Size8, Size8)
    delog("redrawn")
    end()
  }
}
