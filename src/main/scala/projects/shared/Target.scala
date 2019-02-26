package projects.shared

import lib.p5
import lib.traits.Locatable
import scalafx.scene.paint.Color

class Target extends Locatable {
  val size = 20

  def render(): Unit = {
    p5.fill(Color.Red)
    p5.ellipse(x, y, size)
  }
}
