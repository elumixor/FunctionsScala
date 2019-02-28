package lib.math

import scala.language.implicitConversions

case class Point(x: Double, y: Double) {
  def isZero: Boolean = x == 0 && y == 0
  def *(scalar: Double): Point = (x * scalar, y * scalar)
  def *:(scalar: Double): Point = (x * scalar, y * scalar)
  def *(another: Point): Double = x * another.x + y * another.y
  def +(another: Point): Point = (x + another.x, y + another.y)
  def unary_- : Point = (-x, -y)
  def -(another: Point): Point = this + -another
  def length: Double = Math.sqrt(x * x + y * y)
  def normalized: Point = (x / length, y / length)
}

object Point {
  implicit def toVec2(t: (Double, Double)): Point = Point(t._1, t._2)
}
