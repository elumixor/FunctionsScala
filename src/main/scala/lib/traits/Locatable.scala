package lib.traits

import lib.Point

trait Locatable {
  var x: Int = 0
  var y: Int = 0
  def location_=(p: Point): Unit = {
    x = p.x
    y = p.y
  }
  def location: Point = Point(x, y)
}
