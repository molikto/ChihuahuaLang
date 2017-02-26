package org.snailya.mygame

/**
  * Created by molikto on 21/02/2017.
  */

import MyGame._
import MyGame.game._
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.ui.TextField
import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldStyle

import scala.collection.mutable

class ScreenMain extends ScreenBase {


  /**
    * style
    */
  val BackgroundColor = new Color(0x2b303bFF)
  val SelectionColor = new Color(0xFFFFFF33)
  val PlaceholderColor = new Color(0xFFFFFF77)

  val Size8 = size(8)

  val ItemIndent = size(20)


  case class SyntaxSort(name: String, var forms: Seq[SyntaxForm] /* var only to construct cyclic reference */)
  val Term = SyntaxSort("term", null)

  case class SyntaxForm(name: String, specs: Seq[SyntaxSort] = Seq.empty)
  val True = SyntaxForm("true")
  val False = SyntaxForm("false")
  val IfThenElse = SyntaxForm("if $1 then $2 else $3", Seq(Term, Term, Term))
  val Zero = SyntaxForm("0")
  val Succ = SyntaxForm("succ $1", Seq(Term))
  val Pred = SyntaxForm("pred $1", Seq(Term))
  val IsZero  = SyntaxForm("iszero $1", Seq(Term))

  Term.forms = Seq(True, False, IfThenElse, Zero, Succ, Pred)

  case class Language(root: SyntaxSort)

  val UAE = Language(Term)

  class Tree(var content: Option[SyntaxForm]) {
    val childs: mutable.Buffer[Tree] = mutable.ArrayBuffer.empty
    var parent: Option[Tree] = None

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
    var debugText = 0
    var clipboard: Option[Tree] = None

    def newName() = {
      debugText += 1
      debugText.toString
    }

  }

  def drawTree(tree: Tree, left: Float, top: Float): Float = {
    var font = Roboto20
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

  override def render(delta: Float) = {
    if (!state.isInsert) {
      if (Gdx.input.isKeyJustPressed(Keys.U)) {
        state.isInsert = true
      } else if (Gdx.input.isKeyJustPressed(Keys.N)) {
        state.selection match {
          case Some(t) =>
            val c = new Tree(Some(randomItem(Term.forms)))
            t.append(c)
            state.selection = Some(c)
          case None =>
        }
      } else if (Gdx.input.isKeyJustPressed(Keys.I)) {
        state.selection match {
          case Some(t) =>
            state.selection = t.parent
          case _ =>
        }
      } else if (Gdx.input.isKeyJustPressed(Keys.K)) {
        state.selection match {
          case Some(t) =>
            if (t.childs.nonEmpty) state.selection = Some(t.childs.head)
          case _ => state.selection = Some(state.root)
        }
      } else if (Gdx.input.isKeyJustPressed(Keys.L)) {
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
      } else if (Gdx.input.isKeyJustPressed(Keys.J)) {
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
      } else if (Gdx.input.isKeyJustPressed(Keys.D)) {
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
      } else if (Gdx.input.isKeyJustPressed(Keys.P)) {
        state.selection match {
          case Some(t) =>
            state.clipboard match {
              case Some(c) =>
                t.append(c)
                state.selection = Some(c)
              case None =>
            }
          case None =>
        }
      }
    } else {
      assert(state.selection.isDefined)
      val selected = state.selection.get
    }
    clearColor(BackgroundColor)
    begin()
    drawTree(state.root, Size8, Size8)
    end()
  }
}
