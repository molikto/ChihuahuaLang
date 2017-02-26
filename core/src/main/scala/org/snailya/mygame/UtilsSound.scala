package org.snailya.mygame

import com.badlogic.gdx.Gdx

/**
  * Created by molikto on 31/01/2017.
  */
trait UtilsSound {


  def sound(s: String) = Gdx.audio.newSound(Gdx.files.internal(s + ".ogg"))
}
