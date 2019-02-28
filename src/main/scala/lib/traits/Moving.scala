package lib.traits

import lib.math.Point

trait Moving {
  this: Locatable =>

  var speed: Point = (0.0, 0.0)
  var acceleration: Point = (0.0, 0.0)

  def isMoving: Boolean = !speed.isZero

  def move(): Unit = {
    speed = speed + acceleration
    location = location + speed
  }
}
