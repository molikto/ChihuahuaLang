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
    val EditingColor = new Color(0xe3322d99)
    val ErrorColor = new Color(0xe3322d99)
    val PlaceholderColor = new Color(0xFFFFFF77)
    val Size8 = size(8)
    val ItemIndent = size(20)
  }

  import style._

  /**
    * Keys
    */

  abstract class Command() {
    def accept(s: String): Boolean
  }
  case class ConstantCommand(s: String) extends Command {
    def accept(a: String) = a == s
  }
  case class AcceptanceCommand(s: String => Boolean) extends Command {
    def accept(a: String) = s(a)
  }

  case class SyntaxSort(name: String, var forms: Seq[SyntaxForm] /* var only to construct cyclic reference */)

  case class SyntaxForm(command: Command, specs: Seq[SyntaxSort], toLayout: ToLayout)

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

    def draw(px: Float, py: Float) = if (bg != null) drawColor(px + x, py + y, width, height, bg)


    def measure0(tree: Tree)
  }

  def drawChildren(px: Float, py: Float, t: Widget, seq: Widget*) = {
    for (s <- seq) {
      s.draw(px + t.x, py + t.y)
    }
  }

  case class ToLayout(cap: Int, fun: Seq[Widget] => Widget)

  abstract class WIndentAbs extends Widget {
    override def measure0(t: Tree) = {
      width = ItemIndent
      height = Font.lineHeight
    }
  }
  object WIndent extends WIndentAbs

  case object WExtraParameters extends WIndentAbs {
    bg = ErrorColor
  }

  case class WSequence(seq: Widget*) extends Widget {
    override def measure0(tree: Tree) = {
      var x = 0f
      for (s <- seq) {
        s.measure(tree, x, 0)
        x += s.width
      }
      width = x
      height = seq.map(_.height).max
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
      width = seq.map(_.width).max
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

  case class WCommand() extends WGlyph {


    override def measure0(tree: Tree) = {
      tree.commandLayout = this
      val placeholderText = if (tree.commandBuffer.nonEmpty) tree.commandBuffer else if (tree.content.isEmpty) "?" else ""
      val text = if (placeholderText.nonEmpty) {
        color = PlaceholderColor
        placeholderText
      } else tree.command
      measure0(text)
    }
  }

  object layouts {
    val Inline1 = ToLayout(0, seq => WCommand())
    val Inline2 = ToLayout(1, seq => WSequence(WCommand(), WConstant(" "), seq.head))
    val Default = ToLayout(-1, seq => WVertical(seq: _*))
  }


  def SyntaxForm1(name: String) = SyntaxForm(ConstantCommand(name), Seq.empty, layouts.Inline1)

  def SyntaxForm2(name: String, c: SyntaxSort) = SyntaxForm(ConstantCommand(name), Seq(c), layouts.Inline2)

  object UntypedArithmetic {
    val Term = SyntaxSort("term", null)
    val True = SyntaxForm1("true")
    val False = SyntaxForm1("false")
    val IfThenElse = SyntaxForm(ConstantCommand("if"), Seq(Term, Term, Term),
      ToLayout(3, (seq) => {
        WVertical(
          WSequence(WCommand(), WConstant(" "), seq(0)),
          WSequence(WIndent, seq(1)),
          WConstant("else"),
          WSequence(WIndent, seq(2))
        )
      })
    )
    val Succ = SyntaxForm2("succ", Term)
    val Pred = SyntaxForm2("pred", Term)
    val IsZero = SyntaxForm2("iszero", Term)
    def isPositiveNumber(s: String) = {
      s.length > 0 && s.forall(a => "0123456789".contains(a))
    }
    def isNumber(s: String): Boolean = {
      if (s.startsWith("-")) isPositiveNumber(s.substring(1))
      else isPositiveNumber(s)
    }
    val Number = SyntaxForm(AcceptanceCommand(isNumber), Seq(), layouts.Inline1)
    Term.forms = Seq(True, False, IfThenElse, Number, Succ, Pred, IsZero)
    val Lang = Language(Seq(Term), Term.forms)
  }

  object UntypedLambdaCalculus {

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
        layout = layouts.Default.fun(Seq(layout, WSequence(WExtraParameters, WVertical(cwidgets.drop(transformer.cap): _*))))
      }
      // this will measure the rest of the elements just created by the transformer
      // also one child might set the command layout property
      layout.measure(this, 0, 0)
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
    val Lang = UntypedArithmetic.Lang
    val root = new Tree(None)
    var selection: Option[Tree] = Some(root)
    var isInsert = false
    var clipboard: Option[Tree] = None
    var hPosition: Float = -1F
  }

  /**
    * UI
    */

  val Font = Roboto20


  def stateInsertAtNextHoleOrExit() = {
    // TODO make it better
    state.selection.get.commandBuffer = ""
    val res = state.selection.get.linearizedNext(_.content.isEmpty)
    if (res.nonEmpty) state.selection = res
    else {
      state.isInsert = false
    }
  }

  def stateCommitCommand(jump: Boolean): Unit = {
    assert(state.selection.nonEmpty)
    val selection = state.selection.get
    assert(selection.content.isEmpty)
    val command = selection.commandBuffer
    selection.commandBuffer = ""
    if (command.isEmpty) {
      state.isInsert = false
    } else {
      state.Lang.forms.find(_.command.accept(command)) match {
        case Some(f) =>
          selection.content = Some(f)
          selection.command = command
          f.specs.foreach(_ => selection.appendNew())
          if (jump) stateInsertAtNextHoleOrExit()
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
      needsRender = true
      if (state.isInsert) {
        if (keycode == Keys.ESCAPE) {
          if (state.selection.nonEmpty) {
            if (state.selection.get.content.isEmpty) stateCommitCommand(false)
            state.isInsert = false
            state.selection.get.commandBuffer = ""
            true
          }
        }
      }
      false
    }

    override def keyTyped(character: Char) = {
      needsRender = true
      if (state.isInsert) {
        delog("key typed: " + Integer.toHexString(character.toInt))
        assert(state.selection.isDefined)
        val selected = state.selection.get
        if (selected.content.isEmpty) {
          if (character == ' ' || character == '\n') {
            stateCommitCommand(true)
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
            if (state.selection.isDefined) state.isInsert = true
          case 'n' => // new empty node
            state.selection.foreach(t => {
              if (t.content.nonEmpty) {
                val c = new Tree(None)
                t.append(c)
                state.selection = Some(c)
              }
            })
          case 'k' => // go up
            state.selection.foreach(t => {
              // TODO
            })
          case 'j' => // go down
            state.selection match {
              case Some(t) =>
              // TODO
              case None =>
                state.selection = Some(state.root)
            }
          case 'l' => // go right
            state.selection match {
              case Some(t) =>
                t.linearizedNext().foreach(s => {
                  state.selection = Some(s)
                  //state.hPosition = s.commandLayout.globalCenterH()
                })
              case None => state.selection = Some(state.root)
            }
          case 'h' => // go left
            state.selection.foreach(t => {
              state.selection = t.linearizedPrevious()
            })
          case 'K' => // go to parent
            state.selection.foreach(t => {
              state.selection = t.parent
            })
          case 'J' => // go to first child
            state.selection match {
              case Some(t) =>
                if (t.childs.nonEmpty) state.selection = Some(t.childs.head)
              case _ => state.selection = Some(state.root)
            }
          case 'L' => // go to next sibling
            state.selection.foreach(t => {
              t.parent match {
                case Some(p) =>
                  val selection = Math.min(p.childs.indexOf(t) + 1, p.childs.size - 1)
                  state.selection = Some(p.childs(selection))
                case None =>
              }
            })
          case 'H' => // go to previous sibling
            state.selection.foreach(t => {
              t.parent match {
                case Some(p) =>
                  val selection = Math.max(p.childs.indexOf(t) - 1, 0)
                  state.selection = Some(p.childs(selection))
                case None =>
              }
            })
          case 'd' => // delete an item
            state.selection.foreach(t => {
              if (t.childs.nonEmpty || t.content.nonEmpty) {
                val tt = t.moveContentOut()
                t.content = None
                t.childs.clear()
                state.clipboard = Some(tt)
              }
              t.parent match {
                case Some(p) =>
                  val cap = p.content.map(_.toLayout.cap).getOrElse(-1)
                  if (p.childs.size == cap) p.appendNew()
                  p.remove(t)
                  state.selection = Some(p)
                case None =>
              }
            })
          case 'p' => // paste an item
            state.selection.foreach(t => {
              state.clipboard match {
                case Some(c) =>
                  if (t.content.isEmpty && t.childs.isEmpty) {
                    t.copyContent(c)
                  } else {
                    val cc = c.copy()
                    t.append(cc)
                    state.selection = Some(cc)
                  }
                case None =>
              }
            })
          case _ =>
        }
        true
      }
    }
  })

  var needsRender = true

  override def render(delta: Float) = {
    if (needsRender) {
      needsRender = false
      var t = System.nanoTime()
      clearColor(BackgroundColor)
      begin()
      state.root.measure(screenPixelWidth - Size8 * 2)
      state.selection.foreach(a => {
        a.layout.bg = SelectionColor
        a.commandLayout.bg = if (state.isInsert) EditingColor else SelectionColor
      })
      state.root.ast()
      state.root.layout.draw(Size8, Size8)
      delog("redrawn " + (System.nanoTime() - t) / 1000000 + "ms")
      end()
    }
  }
}
