package projects.pathfinder

import lib.traits.{Locatable, Moving}
import lib.{Vec2, p5}
import scalafx.scene.paint.Color

object Pathfinder extends Locatable with Moving {
  var size = 30

  def render(): Unit = {
    p5.translate(x, y)

    val deg = Math.acos(speed * Vec2(0, -1) / (speed.length * Vec2(0, 1).length)) * 180 / Math.PI
    p5.rotate(deg)

    p5.fill(Color.Black)
    p5.polygon(Seq(
      (0, -size / 2),
      (0 - size / 2, 0 + size / 2),
      (0, 0 + size * 1 / 4),
      (0 + size / 2, 0 + size / 2)))
    p5.rotate(-deg)

    p5.translate(-x, -y)
  }
}
