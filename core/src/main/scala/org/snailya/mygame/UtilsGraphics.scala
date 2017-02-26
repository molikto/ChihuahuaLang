package org.snailya.mygame

import com.badlogic.gdx.graphics.g2d.{NinePatch, TextureAtlas, TextureRegion}
import com.badlogic.gdx.scenes.scene2d.utils.{Drawable, NinePatchDrawable, TextureRegionDrawable}

/**
  * Created by molikto on 31/01/2017.
  */
trait UtilsGraphics {


  implicit def textureRegion_to_Drawable(a: TextureRegion): TextureRegionDrawable = new TextureRegionDrawable(a)

  def padded(a: TextureRegion, left: Float = 0, top: Float = 0, right: Float = 0, bottom: Float = 0): TextureRegionDrawable = {
    val d = new TextureRegionDrawable(a)
    if (left != 0) d.setLeftWidth(left)
    if (bottom != 0) d.setBottomHeight(bottom)
    if (right != 0) d.setRightWidth(right)
    if (top != 0) d.setTopHeight(top)
    d
  }

  implicit def ninePatch_to_Drawable(a: NinePatch): Drawable = new NinePatchDrawable(a)


  def loadNinePatchSized(altas: TextureAtlas, p: String, width: Float, height: Float): NinePatch = {
    val region = altas.findRegion(p)
    if (region == null) {
      null
    } else {
      val splits = region.splits
      if (splits == null) throw new IllegalArgumentException("Region does not have ninepatch splits: " + p)
      val patch = new NinePatch(region, splits(0), splits(1), splits(2), splits(3))
      if (region.pads != null) patch.setPadding(region.pads(0), region.pads(1), region.pads(2), region.pads(3))
      val scaleX = width / region.getRegionWidth
      val scaleY = height / region.getRegionHeight
      patch.scale(scaleX, scaleY)
      patch
    }
  }
}
