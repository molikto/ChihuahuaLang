package org.snailya.mygame

import com.badlogic.gdx.scenes.scene2d.ui.{Button, Widget}
import com.badlogic.gdx.scenes.scene2d.{Actor, EventListener, InputEvent}
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener

/**
  * Created by molikto on 31/01/2017.
  */
trait UtilsUi {


  implicit def function_to_clickListener(f: () => Any): MyEventListener = {
    new MyClickEventListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = f()
    }
  }

  trait MyEventListener extends EventListener {
  }
  class MyClickEventListener extends ClickListener with MyEventListener {

  }

  def setListener(v: Actor, a: EventListener): Unit = {
    var i = 0
    while (i < v.getListeners.size) {
      val l = v.getListeners.get(i)
      if (l.isInstanceOf[MyEventListener]) {
        v.removeListener(l)
      } else {
        i += 1
      }
    }
    if (a != null) v.addListener(a)
    v match {
      case b: Button => b.setDisabled(a == null)
      case _ =>
    }
  }



  def fillp[T <: Widget](v: T): T = {
    v.setFillParent(true)
    v
  }
}
