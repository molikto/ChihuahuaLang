package org.snailya.mygame

import com.badlogic.gdx.graphics.OrthographicCamera
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.utils.{Align, Scaling}
import com.badlogic.gdx.utils.viewport.{ScalingViewport, ScreenViewport}
import com.badlogic.gdx.{Gdx, Input, InputProcessor, Screen}
import MyGame._
import MyGame.game._
import Gdx._
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle
import com.badlogic.gdx.scenes.scene2d.ui.TextButton.TextButtonStyle
import com.badlogic.gdx.scenes.scene2d.ui.{Dialog, Label, Table, TextButton}

import scala.util.Try

/**
  * Created by molikto on 08/01/2017.
  */
trait ScreenBase {


  val stage = new Stage(new ScreenViewport(new OrthographicCamera()), batch)

  input.setInputProcessor(stage)

  def resize(width: Int, height: Int) = {
    stage.getViewport.update(width, height, true)
  }

  def dispose(): Unit = {
    stage.dispose()
    input.setInputProcessor(null)
  }

  def hide(): Unit = {}

  /**
    * remove affections on global things like input
    */

  def pause(): Unit = {}

  def render(delta: Float): Unit = {
    if (Debug) {
      if (Gdx.input.isKeyJustPressed(Input.Keys.D)) {
        stage.setDebugAll(true)
      } else if (Gdx.input.isKeyJustPressed(Input.Keys.E)) {
        stage.setDebugAll(false)
      }
    }
    stage.act(delta)
    stage.draw()
    if (Debug) {
      val fps = Gdx.graphics.getFramesPerSecond
      batch.begin()
      //SohoGotic12.draw(batch, fps.toString, 0, size(12))
      batch.end()
    }
  }

  def show(): Unit = {}

  def resume(): Unit = {}

}
