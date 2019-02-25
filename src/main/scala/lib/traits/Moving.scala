package lib.traits

import lib.Vec2

trait Moving {
  this: Locatable =>

  var speed: Vec2 = (0.0, 0.0)
  var acceleration: Vec2 = (0.0, 0.0)

  def isMoving: Boolean = !speed.isZero

  def move(): Unit = {
    speed = speed + acceleration
    location = location + speed.toPoint
  }
}
