package org.snailya.mygame


import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Color, GL20, Pixmap, Texture}
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator
import Gdx._
import com.badlogic.gdx.Application.ApplicationType
import com.badlogic.gdx.graphics.Pixmap.Format
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeFontParameter
import com.badlogic.gdx.graphics.g2d._
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.math.Interpolation
import com.badlogic.gdx.scenes.scene2d.ui.Button.ButtonStyle
import com.badlogic.gdx.scenes.scene2d.ui.ImageButton.ImageButtonStyle
import com.badlogic.gdx.scenes.scene2d._
import com.badlogic.gdx.scenes.scene2d.actions.Actions
import com.badlogic.gdx.scenes.scene2d.actions.Actions.sequence
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle
import com.badlogic.gdx.scenes.scene2d.ui.TextButton.TextButtonStyle
import com.badlogic.gdx.scenes.scene2d.ui.Window.WindowStyle
import com.badlogic.gdx.scenes.scene2d.ui._
import com.badlogic.gdx.scenes.scene2d.utils.{ClickListener, Drawable, NinePatchDrawable, TextureRegionDrawable}

import MyGame._
import MyGame.game._
/**
  * Created by molikto on 18/01/2017.
  */
//class AlertDialog(val title: String = "", val isAlert: Boolean = false) extends Dialog("", {
//  val style = new WindowStyle(KenFuture14, colors.TEXT, null)
//  style.stageBackground = TransBlack
//  style
//}) {
//
//  var cb = true
//
//  setModal(true)
//
//  var hasTitle = title != ""
//
//  var titlePaddings = size(8)
//  val titleTableAlt = new Table().pad(titlePaddings).padBottom(titlePaddings * 2).background(RedPanel)
//  val titleLabelAlt = new Label(title, new LabelStyle(KenFuture14, colors.TEXT_WHITE))
//  titleTableAlt.add(titleLabelAlt)
//
//  if (hasTitle) {
//    val newTable = new Table().background(GreyPanel)
//    getCell(getContentTable).space(0).setActor(titleTableAlt).pad(0)
//    getCell(getButtonTable).space(0).setActor(newTable).pad(0).padTop(-titlePaddings)
//    newTable.add(getContentTable).expand().fill().pad(size(12))
//    newTable.row()
//    newTable.add(getButtonTable).fillX().pad(size(12)).padTop(0)
//  } else {
//    getCell(getContentTable).space(0).pad(size(12))
//    getCell(getButtonTable).space(0).pad(size(12)).padTop(0)
//    background(GreyPanel)
//  }
//
//  def cancelable(b: Boolean): AlertDialog = {
//    cb = b
//    this
//  }
//
//
//  override def show(stage: Stage): Dialog = {
//    if (cb) {
//      addListener(new MyClickEventListener() {
//        override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
//          if (y > getHeight || y < 0 || x < 0 || x > getWidth) hide()
//        }
//      })
//    }
//
//    show(stage, sequence(Actions.alpha(0), Actions.fadeIn(0.2f, Interpolation.fade)))
//    setPosition(Math.round((stage.getWidth - getWidth) / 2), Math.round((stage.getHeight - getHeight) / 2))
//    this
//  }
//
//
//  override def hide() = {
//    hide(sequence(Actions.fadeOut(0.2f, Interpolation.fade), Actions.removeListener(ignoreTouchDown, true), Actions.removeActor()))
//  }
//
//  override def button(text: String, o: scala.Any) = {
//    button(text, o, ButtonGreen)
//  }
//
//  override def text(t: String) = {
//    text(t, new LabelStyle(SohoGotic14, colors.TEXT_BLACK))
//  }
//}
