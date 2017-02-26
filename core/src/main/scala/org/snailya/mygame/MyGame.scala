package org.snailya.mygame


import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Color, GL20}
import Gdx._
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator

import scala.util._

object MyGame extends UtilsUi with UtilsGraphics with UtilsApp with UtilsSound {
  val game: MyGame = new MyGame()
}


class MyGame extends MyGameStyle {



  def change(s: => ScreenBase): Unit = {
    if (current != null) {
      current.dispose()
    }
    current = s
  }


  var current: ScreenBase = null






  def resume(): Unit = {
    if (current != null) current.resume()
  }

  def dispose(): Unit = {
    if (current != null) {
      current.dispose()
    }
  }

  def pause(): Unit = {
    if (current != null) current.pause()
  }

  def render(): Unit = {
    if (current != null) current.render(Gdx.graphics.getDeltaTime)
  }




  def resize(width: Int, height: Int): Unit = {
    if (current != null) current.resize(width, height)
  }


  MyGame.postRun(() => {
    change(new ScreenMain())
  })
}
