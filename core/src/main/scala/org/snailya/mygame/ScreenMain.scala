package org.snailya.mygame

/**
  * Created by molikto on 21/02/2017.
  */

import MyGame._
import MyGame.game._
import com.badlogic.gdx.{Gdx, InputProcessor}
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Color
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
  case class SyntaxForm(name: String, command: String, specs: Seq[SyntaxSort] = Seq.empty)
  case class Language(root: SyntaxSort)

  object UAE {

    val Term = SyntaxSort("term", null)

    val True = SyntaxForm("true", "true")
    val False = SyntaxForm("false", "false")
    val IfThenElse = SyntaxForm("if $1 then $2 else $3", "if", Seq(Term, Term, Term))
    val Zero = SyntaxForm("0", "0")
    val Succ = SyntaxForm("succ $1", "succ", Seq(Term))
    val Pred = SyntaxForm("pred $1", "pred", Seq(Term))
    val IsZero  = SyntaxForm("iszero $1", "iszero", Seq(Term))

    Term.forms = Seq(True, False, IfThenElse, Zero, Succ, Pred)

    val Lang = Language(Term)
  }

  import UAE._


  class Tree(var content: Option[SyntaxForm]) {
    val childs: mutable.Buffer[Tree] = mutable.ArrayBuffer.empty
    var parent: Option[Tree] = None


    def copy(p: Option[Tree]): Tree = {
      val cc = new Tree(content)
      cc.parent = p
      cc.childs ++= childs.map(t => t.copy(Some(cc)))
      cc
    }

    def copy(): Tree = copy(parent)

    def append(c: Tree) = {
      assert(c.parent.isEmpty)
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
    val root = new Tree(None)
    var selection: Option[Tree] = Some(root)
    var isInsert = false
    var clipboard: Option[Tree] = None
    var commandBuffer: String = ""
  }

  /**
    * UI
    */

  def drawTree(tree: Tree, left: Float, top: Float): Float = {
    val font = Roboto20
    var height = top
    if (state.selection.contains(tree)) {
      val width = font.measure(if (tree.content.isEmpty) "?" else tree.content.get.name)
      draw(left, height, width, font.height, SelectionColor)
    }
    if (tree.content.isEmpty) {
      font.draw(left, height, "?", PlaceholderColor)
    } else {
      font.draw(left, height, tree.content.get.name)
    }
    height += font.lineHeight
    for (c <- tree.childs) {
      height = drawTree(c, left + ItemIndent, height)
    }
    height
  }

  Gdx.input.setInputProcessor(new InputProcessor {

    override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int) = false

    override def mouseMoved(screenX: Int, screenY: Int) = false

    override def touchUp(screenX: Int, screenY: Int, pointer: Int, button: Int) = false

    override def touchDragged(screenX: Int, screenY: Int, pointer: Int) = false

    override def scrolled(amount: Int) = false

    override def keyDown(keycode: Int) = false

    override def keyUp(keycode: Int) = false

    override def keyTyped(character: Char) = if (state.isInsert) {
      assert(state.selection.isDefined)
      val selected = state.selection.get
      true
    } else {
      character match {
        case 'i' =>  // enter insert mode
          state.isInsert = true
        case 'n' => // new empty node
          state.selection match {
            case Some(t) =>
              val c = new Tree(None)
              t.append(c)
              state.selection = Some(c)
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
              t.parent match {
                case Some(p) =>
                  state.selection = Some(p)
                  p.remove(t)
                  state.clipboard = Some(t)
                case None =>
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
    drawTree(state.root, Size8, Size8)
    delog("redrawn")
    end()
  }
}
