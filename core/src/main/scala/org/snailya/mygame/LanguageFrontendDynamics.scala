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
    var cursorPosition = 0

    var errors: Seq[Error] = Seq.empty
  }


  var debugPreviousCommand: Char = ' '

  def startInsert(b: Option[Int]) = {
    state.isInsert = b.isDefined
    state.cursorPosition = if (b.isEmpty) 0 else b.get
  }

  def insertAtNextHoleOrExit() = {
    // TODO make it better
    state.selection.get.commandBuffer = ""
    val res = state.selection.get.linearizedNext(_.form.isEmpty)
    if (res.nonEmpty) state.selection = res
    else {
      startInsert(None)
    }
  }

  def tryCommitCommandEager(jump: Boolean): Unit = {
    assert(state.selection.nonEmpty)
    val selection = state.selection.get
    val command = selection.commandBuffer
    if (selection.form.isEmpty) { // TODO eager commit with non-empty form
      val (forms, sortSpcific) = selection.parent match {
        case None => (Lang.forms, false)
        case Some(p) =>
          p.form match {
            case None => (Lang.forms, false)
            case Some(f) =>
              val index = p.indexOf(selection)
              val sort = f.sort(index, p.childs.size)
              (sort.map(_.forms).getOrElse(Lang.forms), true)
          }
      }
      forms.find(_.command.accept(command).exists(_.eager)) match {
        case Some(f) =>
          selection.form = Some(f)
          selection.command = command
          f.childs.foreach(c => {
            for (i <- 0 until c.init) {
              val n = selection.appendNew()
            }
          })
          if (jump) insertAtNextHoleOrExit()
        case None =>
      }
    }
  }

  def commitCommand(jump: Boolean): Unit = {
    assert(state.selection.nonEmpty)
    val selection = state.selection.get
    val command = selection.commandBuffer
    selection.commandBuffer = ""
    val hasFormBefore = selection.form.nonEmpty
    if (command.isEmpty) {
      startInsert(None)
    } else {
      val (forms, sortSpcific) = selection.parent match {
        case None => (Lang.forms, false)
        case Some(p) =>
          p.form match {
            case None => (Lang.forms, false)
            case Some(f) =>
              val index = p.indexOf(selection)
              val sort = f.sort(index, p.childs.size)
              if (sort.isDefined) (sort.get.forms, true)
              else (Lang.forms, false)
          }
      }

      def simpleFillCommand(selection: Tree, f: SyntaxForm): Unit = {
        selection.form = Some(f)
        selection.command = command
        if (!hasFormBefore) { // TODO deal with has form before
          f.childs.foreach(c => {
            for (i <- 0 until c.init) {
              val n = selection.appendNew()
            }
          })
          if (jump) insertAtNextHoleOrExit() else startInsert(None)
        } else {
          var hasNew = false
          while (selection.size < f.min) {
            hasNew = true
            selection.appendNew()
          }
          if (hasNew && jump) insertAtNextHoleOrExit()
          else startInsert(None)
        }
      }

      def commitGlobal() = {
        Lang.forms.find(_.command.accept(command).nonEmpty)  match {
          case Some(f) =>
            simpleFillCommand(selection, f)
          case None =>
        }
      }
      forms.find(_.command.accept(command).nonEmpty) match {
        case Some(f) =>
          simpleFillCommand(selection, f)
        case None =>
          if (!hasFormBefore && sortSpcific && forms.size == 1) { // TODO also here, deal with has form before
            val only = forms.head
            val auto = only.command match {
              case ConstantCommand(c, a, _) if a => Some(c)
              case _ => None
            }
            if (only.min == 1 && auto.isDefined) {
              val sort = only.childs.find(_.min == 1).get.sort
              sort.forms.find(_.command.accept(command).nonEmpty) match {
                case Some(f) =>
                  selection.form = Some(only)
                  selection.command = auto.get
                  only.childs.foreach(c => {
                    for (i <- 0 until c.init) {
                      val n = selection.appendNew()
                      if (c.min == 1) {
                        n.form = Some(f)
                        n.command = command
                        state.selection = Some(n)
                      }
                    }
                  })
                  if (jump) insertAtNextHoleOrExit()
                  else startInsert(None)
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
            startInsert(None)
            state.selection.get.commandBuffer = ""
            return true
          }
        } else if (keycode == Keys.LEFT) {
        } else if (keycode == Keys.RIGHT) {

        }
      }
      false
    }

    override def keyTyped(character: Char): Boolean = {
      needsRemeasure = true
      debugPreviousCommand = character
      if (state.isInsert) {
        delog("key typed: " + Integer.toHexString(character.toInt))
        assert(state.selection.isDefined)
        val selected = state.selection.get
        // TODO cursor movement
        // TODO support raw char input

        def tryCommandSep(s: Tree): Boolean = {
          var c = selected
          var p = selected.parent
          while (p.isDefined) {
            val pt = p.get
            pt.form.foreach(f => {
              if (pt.size < f.max) {
                f.childs.find(_.sepCommand.contains(character)).foreach(rel => {
                  val index = pt.indexOf(c)
                  val sort = pt.form.get.relation(index, pt.size)
                  if (sort.exists(_._1 == rel)) {
                    val t = new Tree(None)
                    state.selection = Some(t)
                    pt.insert(index + 1, t)
                    commitCommand(true) // we want to finish current command
                    startInsert(Some(0))
                    return true // ATTENTION: early return!!
                  }
                })
              }
            })
            c = pt
            p = pt.parent
          }
          false
        }

        def tryCommandChild(s: Tree): Boolean = {
          var p: Option[Tree] = Some(selected)
          while (p.isDefined) {
            val pt = p.get
            pt.form.foreach(f => {
              if (pt.size < f.max) {
                f.childs.find(_.createCommand.contains(character)).foreach(rel => {
                  val t = new Tree(None)
                  pt.insert(pt.size - f.footerSize, t)
                  state.selection = Some(t)
                  startInsert(Some(0))
                  return true
                })
              }
            })
            p = pt.parent
          }
          false
        }

        if (commandDelimiter.contains(character)) { // current char is NOT part of the command buffer, it will end the command first and try to run the command
          commitCommand(false)
          if (tryCommandSep(selected)) return true
          else if (tryCommandChild(selected)) return true
        }
        if (character == ' ') {
          commitCommand(true)
        } else if (character == '\n') {
          commitCommand(false)
        } else if (character == '\b') {
          if (selected.commandBuffer.nonEmpty) selected.commandBuffer = selected.commandBuffer.dropRight(1)
        } else if (character >= '!' && character <= '~') {
          // TODO valid commands and identifiers
          selected.commandBuffer = selected.commandBuffer + character
          tryCommitCommandEager(true)
        }
        true
      } else {
        character match {
          case '~' =>
            delog(state.root.toString)
          case 'i' => // enter insert mode
            if (state.selection.isDefined) startInsert(Some(0))
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
              startInsert(Some(0))
            })
          case 'N' => // new empty child node
            state.selection.foreach(t => {
              if (t.form.nonEmpty) {
                val n = new Tree(None)
                t.insert(t.form.get.headerSize, n)
                state.selection = Some(n)
                startInsert(Some(0))
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
//          case 'r' =>
//            state.selection.foreach(t => {
//              t.form = None
//              t.command
//            })
          case 'd' => // delete an item and childs, leave a ?, or delete a ?
            state.selection.foreach(t => {
              val isEmpty: Boolean = if (t.nonEmpty || t.form.nonEmpty) {
                val tt = t.moveContentAndChildOut()
                t.form = None
                t.clear()
                state.clipboard = Some(tt)
                false
              } else {
                true
              }
              if (true && isEmpty) {
                t.parent match {
                  case Some(p) =>
                    val index = p.indexOf(t)
                    val cap = p.form.map(_.min).getOrElse(0)
                    if (p.size > cap) {
                      p.remove(t)
                      if (index < p.size) state.selection = Some(p(index))
                      else state.selection = Some(p)
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

  var inited = false
  var needsRemeasure = true

  val commandDelimiter: Seq[Char] = Seq.empty

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
    val dpc = if (debugPreviousCommand == ' ') "SPACE" else if (debugPreviousCommand == '\n') "NEWLINE" else debugPreviousCommand.toString
    Font.draw(Size8, screenPixelHeight - Size8 - Font.height, "command: " + dpc)
    end()
  }
}
