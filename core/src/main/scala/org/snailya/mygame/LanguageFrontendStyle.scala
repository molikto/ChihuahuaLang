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

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Created by molikto on 01/03/2017.
  */
trait LanguageFrontendStyle {



  val Font = DebugFont20

  /**
    * style
    */

  val BackgroundColor = DefaultBgColor
  val SelectionColor = new Color(0xFFFFFF33)
  val EditingColor = new Color(0x3c963d66)
  val ErrorColor = new Color(0xe3322d33)
  val PlaceholderColor = new Color(0xFFFFFF77)
  val Size8 = size(8)
  val ItemIndent = size(20)

}
