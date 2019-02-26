package projects.shared

import lib.traits.{Locatable, Moving}
import lib.{Vec2, p5}
import scalafx.scene.paint.Color

class Pathfinder extends Locatable with Moving {
  var size = 30

  def render(): Unit = {
    p5.fill(Color.Black)

    p5.translate(x, y)

    var deg: Double = 0

    // Rotate pathfinder so it points to hit current speed
    if (!speed.isZero) {
      deg = Math.acos(speed * Vec2(0, -1) / (speed.length * Vec2(0, 1).length)) * 180 / Math.PI
      p5.rotate(deg)
    }

    // Render
    p5.fill(Color.Black)
    p5.polygon(Seq(
      (0, -size / 2),
      (0 - size / 2, 0 + size / 2),
      (0, 0 + size * 1 / 4),
      (0 + size / 2, 0 + size / 2)))

    // Rotate back
    if (!speed.isZero) p5.rotate(-deg)

    p5.translate(-x, -y)
  }
}
