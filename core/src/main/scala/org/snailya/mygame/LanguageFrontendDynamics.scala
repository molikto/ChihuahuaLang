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
  * this class holds the global state object and have the rendering method
  */
trait LanguageFrontendDynamics[T <: AstBaseWithPositionData, H <: T] extends LanguageFrontend[T, H] {


  object state {
    val root = new Tree(None)
    var selection: Option[Tree] = Some(root)
    var isInsert = false
    var clipboard: Option[Tree] = None
    var hPosition: Float = -1F

    var errors: Seq[Error] = Seq.empty
  }

  def insertAtNextHoleOrExit() = {
    // TODO make it better
    state.selection.get.commandBuffer = ""
    val res = state.selection.get.linearizedNext(_.form.isEmpty)
    if (res.nonEmpty) state.selection = res
    else {
      state.isInsert = false
    }
  }

  def commitCommand(jump: Boolean): Unit = {
    assert(state.selection.nonEmpty)
    val selection = state.selection.get
    assert(selection.form.isEmpty)
    val command = selection.commandBuffer
    selection.commandBuffer = ""
    if (command.isEmpty) {
      state.isInsert = false
    } else {
      val (forms, sortSpcific) = selection.parent match {
        case None => (Lang.forms, false)
        case Some(p) =>
          p.form match {
            case None => (Lang.forms, false)
            case Some(f) =>
              val index = p.indexOf(selection)
              val sort = f.sort(index, p.childs.size)
              (sort.forms, true)
          }
      }
      def commitGlobal() = {
        Lang.forms.find(_.command.accept(command)) match {
          case Some(f) =>
            selection.form = Some(f)
            selection.command = command
            f.childs.foreach(c => {
              for (i <- 0 until (c.min max 1)) {
                val n = selection.appendNew()
              }
            })
            if (jump) insertAtNextHoleOrExit()
          case None =>
        }
      }
      forms.find(_.command.accept(command)) match {
        case Some(f) =>
          selection.form = Some(f)
          selection.command = command
          f.childs.foreach(c => {
            for (i <- 0 until (c.min max 1)) {
              val n = selection.appendNew()
            }
          })
          if (jump) insertAtNextHoleOrExit()
        case None =>
          if (sortSpcific && forms.size == 1) {
            val only = forms.head
            val auto = only.command match {
              case ConstantCommand(c, a) if a => Some(c)
              case _ => None
            }
            if (only.min == 1 && auto.isDefined) {
              val sort = only.childs.find(_.min == 1).get.sort
              sort.forms.find(_.command.accept(command)) match {
                case Some(f) =>
                  selection.form = Some(only)
                  selection.command = auto.get
                  only.childs.foreach(c => {
                    for (i <- 0 until (c.min max 1)) {
                      val n = selection.appendNew()
                      if (c.min == 1) {
                        n.form = Some(f)
                        n.command = command
                        state.selection = Some(n)
                      }
                    }
                  })
                  if (jump) insertAtNextHoleOrExit()
                case None =>
                  commitGlobal()
              }
            } else {
              commitGlobal()
            }
          } else {
            commitGlobal()
          }
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

    override def keyUp(keycode: Int): Boolean = {
      needsRemeasure = true
      if (state.isInsert) {
        if (keycode == Keys.ESCAPE) {
          if (state.selection.nonEmpty) {
            if (state.selection.get.form.isEmpty) commitCommand(false)
            state.isInsert = false
            state.selection.get.commandBuffer = ""
            return true
          }
        }
      }
      false
    }

    override def keyTyped(character: Char) = {
      needsRemeasure = true
      if (state.isInsert) {
        delog("key typed: " + Integer.toHexString(character.toInt))
        assert(state.selection.isDefined)
        val selected = state.selection.get
        if (selected.form.isEmpty) {
          if (character == ' ' || character == '\n') {
            commitCommand(true)
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
          case '~' =>
            delog(state.root.toString)
          case 'i' => // enter insert mode
            if (state.selection.isDefined) state.isInsert = true
          case 'n' => // new empty sibling node next to this node
            state.selection.foreach(t => {
              if (t.parent.isEmpty) {
                state.selection = Some(t.appendNew())
              } else {
                val p = t.parent.get
                val i = p.indexOf(t)
                val c = new Tree(None)
                p.insert(i + 1, c)
                state.selection = Some(c)
              }
              state.isInsert = true
            })
          case 'N' => // new empty child node
            state.selection.foreach(t => {
              if (t.form.nonEmpty) {
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
                if (t.nonEmpty) state.selection = Some(t.head)
              case _ => state.selection = Some(state.root)
            }
          case 'L' => // go to next sibling
            state.selection.foreach(t => {
              t.parent match {
                case Some(p) =>
                  val selection = Math.min(p.indexOf(t) + 1, p.size - 1)
                  state.selection = Some(p(selection))
                case None =>
              }
            })
          case 'H' => // go to previous sibling
            state.selection.foreach(t => {
              t.parent match {
                case Some(p) =>
                  val selection = Math.max(p.indexOf(t) - 1, 0)
                  state.selection = Some(p(selection))
                case None =>
              }
            })
          case 'd' => // delete an item
            state.selection.foreach(t => {
              val isEmpty: Boolean = if (t.nonEmpty || t.form.nonEmpty) {
                val tt = t.moveContentOut()
                t.form = None
                t.clear()
                state.clipboard = Some(tt)
                false
              } else {
                true
              }
              if (isEmpty) {
                t.parent match {
                  case Some(p) =>
                    val cap = p.form.map(_.min).getOrElse(0)
                    if (p.size > cap) {
                      p.remove(t)
                      state.selection = Some(p)
                    }
                  case None =>
                }
              }
            })
          case 'p' => // paste an item
            state.selection.foreach(t => {
              state.clipboard match {
                case Some(c) =>
                  if (t.form.isEmpty && t.isEmpty) {
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

  var needsRemeasure = true

  def renderFrontend(delta: Float) = {
    val timeStart = System.nanoTime()
    var timeMeasureEnd = timeStart
    if (needsRemeasure) {
      needsRemeasure = false
      // compile information
      val (ast, lerrors) = state.root.ast()
      val res = compile(ast)
      state.errors = lerrors ++ (res match {
        case Left(_) => Seq.empty
        case Right(a) => a
      })

      state.root.measure(screenPixelWidth - Size8 * 2)

      for (p <- state.errors) {
        p.cast.layout.bg = ErrorColor
      }

      state.selection.foreach(a => {
        a.layout.bg = SelectionColor
        a.commandLayout.bg = if (state.isInsert) EditingColor else SelectionColor
      })

      timeMeasureEnd = System.nanoTime()
    }

    clearColor(BackgroundColor)
    begin()
    state.root.layout.draw(Size8, Size8)
    for (p <- state.errors) {
      val glyph = Font.measure(p.s)
      Font.draw(screenPixelWidth - Size8 - glyph.width, p.cast.layout.absY, p.s)
    }
    val t1 = (timeMeasureEnd - timeStart) / 1000000
    val t2 = (System.nanoTime() - timeMeasureEnd) / 1000000
    if (t1 != 0 || t2 != 0) delog("remeasure " + t1 + "ms; redrawn " + t2 + "ms")
    end()
  }
}
