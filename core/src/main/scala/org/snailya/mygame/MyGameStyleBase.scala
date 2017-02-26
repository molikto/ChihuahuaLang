package org.snailya.mygame
import  com.badlogic.gdx.Gdx.graphics
/**
  * Created by molikto on 22/02/2017.
  *
  */
trait MyGameStyleBase {



  def screenPixelWidth = graphics.getBackBufferWidth
  def screenPixelHeight = graphics.getBackBufferHeight

  private val dpiPixel =
    if (GameWrapper.optionalScreenWidth != -1) {
      graphics.getBackBufferWidth * 1F / GameWrapper.optionalScreenWidth
    } else if (GameWrapper.optionalOverrideDpi != 0) {
      GameWrapper.optionalOverrideDpi
    } else {
      graphics.getDensity
    }

  delog("screen density " + dpiPixel)

  def size(f: Float): Float = dpiPixel * f


}
