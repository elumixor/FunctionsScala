package lib.traits

import lib.math.Point

trait Locatable {
  var x: Double = 0
  var y: Double = 0
  def location_=(p: Point): Unit = {
    x = p.x
    y = p.y
  }
  def location: Point = Point(x, y)
}
