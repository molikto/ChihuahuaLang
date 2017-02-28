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

/**
  * Created by molikto on 27/02/2017.
  */
trait LanguageFrontendDynamics[T <: AstBase, H <: T] extends LanguageFrontend[T, H] {


  object state {
    val root = new Tree(None)
    var selection: Option[Tree] = Some(root)
    var isInsert = false
    var clipboard: Option[Tree] = None
    var hPosition: Float = -1F

    var errors: Seq[Error] = Seq.empty

  }



  /**
    * UI
    */



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
      Lang.forms.find(_.command.accept(command)) match {
        case Some(f) =>
          selection.content = Some(f)
          selection.command = command
          f.childs.foreach(_ => selection.appendNew())
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
          case 'n' => // new empty sibling node next to this node
            state.selection.foreach(t => {
              if (t.parent.isEmpty) {
                state.selection = Some(t.appendNew())
              } else {
                val p = t.parent.get
                val i = p.childs.indexOf(t)
                val c = new Tree(None)
                p.insert(i + 1, c)
                state.selection = Some(c)
              }
              state.isInsert = true
            })
          case 'N' => // new empty child node
            state.selection.foreach(t => {
              if (t.content.nonEmpty) {
                state.selection = Some(t.appendNew())
                state.isInsert = true
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
                  if (p.childs.size > cap) {
                    p.remove(t)
                    state.selection = Some(p)
                  }
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

  import style._

  def renderFrontend(delta: Float) = {
    if (needsRender) {
      needsRender = false
      var t = System.nanoTime()

      // compile information
      val (ast, lerrors) = state.root.ast()
      val res = compile(ast)
      state.errors = (lerrors ++ (res match {
        case Left(_) => Seq.empty
        case Right(a) => a
      }))


      // measure and rendering
      clearColor(BackgroundColor)
      begin()
      state.root.measure(screenPixelWidth - Size8 * 2)

      for (p <- state.errors) {
        p.t.layout.bg = ErrorColor
      }

      state.selection.foreach(a => {
        a.layout.bg = SelectionColor
        a.commandLayout.bg = if (state.isInsert) EditingColor else SelectionColor
      })
      state.root.layout.draw(Size8, Size8)

      for (p <- state.errors) {
        val glyph = Font.measure(p.s)
        Font.draw(screenPixelWidth - Size8 - glyph.width, p.t.layout.absY, p.s)
      }
      delog("redrawn " + (System.nanoTime() - t) / 1000000 + "ms")
      end()
    }
  }
}
