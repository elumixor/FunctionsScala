package lib

import scala.language.implicitConversions

case class Point(x: Int, y: Int) {
  def +(another: Point): Point = (x + another.x, y + another.y)
}

object Point {
  implicit def toPoint(t: (Int, Int)): Point = Point(t._1, t._2)
}
