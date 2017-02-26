package org.snailya.mygame

import com.badlogic.gdx._
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g2d.SpriteBatch

import scala.collection.mutable

object GameWrapper {
  var optionalScreenWidth = -1
  var optionalOverrideDpi: Float = 0
}
class GameWrapper(optionalScreenWidth: Int = -1, optionalOverrideDpi: Float = 0) extends ApplicationListener {

  GameWrapper.optionalScreenWidth = optionalScreenWidth
  GameWrapper.optionalOverrideDpi = optionalOverrideDpi
  var gameInner: MyGame = null

  override def create(): Unit = {
    gameInner = MyGame.game
  }

  override def dispose(): Unit = {
    if (gameInner != null) {
      gameInner.dispose()
      gameInner = null
    }
  }


  override def pause(): Unit = {
    if (gameInner != null) gameInner.pause()
  }

  override def render(): Unit = {
    if (gameInner != null) gameInner.render()
  }

  override def resume(): Unit = {
    if (gameInner != null) gameInner.resume()
  }

  override def resize(width: Int, height: Int): Unit = {
    if (gameInner != null) gameInner.resize(width, height)
  }
}
