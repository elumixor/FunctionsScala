package lib

import scala.language.implicitConversions

case class Vec2(x: Double, y: Double) {
  def isZero: Boolean = x == 0 && y == 0
  def *(scalar: Double): Vec2 = (x * scalar, y * scalar)
  def *(another: Vec2): Double = x * another.x + y * another.y
  def +(another: Vec2): Vec2 = (x + another.x, y + another.y)
  def length: Double = Math.sqrt(x * x + y * y)
  def normalized: Vec2 = (x / length, y / length)

  implicit def toPoint: Point = Point(x.toInt, y.toInt)
}

object Vec2 {
  implicit def toVec2(t: (Double, Double)): Vec2 = Vec2(t._1, t._2)
}
