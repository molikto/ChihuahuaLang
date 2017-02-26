package org.snailya.mygame

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{Color, GL20, Texture}
import com.badlogic.gdx.graphics.g2d.{BitmapFont, SpriteBatch}
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeFontParameter

/**
  * Created by molikto on 22/02/2017.
  */
trait MyGameStyle extends MyGameStyleBase {




  val batch = new SpriteBatch()

  Gdx.graphics.setContinuousRendering(false)

  val Roboto = new FreeTypeFontGenerator(Gdx.files.internal("Roboto-Regular.ttf"))

  val Roboto20 = new Font(Roboto, size(20))



  def begin() = batch.begin()

  def end() = batch.end()


  val DebugClearColor = new Color(0x427db4FF)

  def clearColor(c: Color): Unit = {
    Gdx.gl.glClearColor(c.r, c.g, c.b, c.a)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
  }


  class Font(gen: FreeTypeFontGenerator, size: Float) {
    val internal = gen.generateFont({val p = new FreeTypeFontParameter(); p.size = size.toInt; p})
    val height = internal.getAscent - internal.getDescent + internal.getCapHeight
    val lineHeight = internal.getLineHeight

    private val heightOffset = internal.getAscent

    delog(
      s"""size: $size,
         |ascent: ${internal.getAscent}
         |descent: ${internal.getDescent}
         |height: $height,
         |line height: $lineHeight
         |cap height: ${internal.getCapHeight}
         |x height: ${internal.getXHeight}""".stripMargin)
    def draw(x: Float, y: Float, str: String, c: Color = Color.WHITE) = {
      internal.setColor(c)
      val layout = internal.draw(batch, str, x, screenPixelHeight - y - heightOffset)
      internal.setColor(Color.WHITE)
      layout.width
    }

    def measure(str: String) = {
      internal.getCache.addText(str, 0, 0).width
    }

  }



  val pureColorTexture = new Texture("pure_white.png")

  def draw(x: Float, y: Float, w: Float, h: Float, c: Color): Unit = {
    batch.setColor(c)
    batch.draw(pureColorTexture, x, screenPixelHeight - y - h, w, h)
    batch.setColor(Color.WHITE)
  }
}

