package org.snailya.mygame

import com.badlogic.gdx.Gdx

/**
  * Created by molikto on 31/01/2017.
  */
trait UtilsApp {

  def postRun(runnable: Runnable) = Gdx.app.postRunnable(runnable)

  implicit def function_runnable(f: () => Unit): Runnable = {
    new Runnable {
      override def run(): Unit = f()
    }
  }

  def postRun(f: () => Unit) = {
    Gdx.app.postRunnable(function_runnable(f))
  }
}
