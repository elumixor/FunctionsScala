package lib

/** Object for custom mathematical functions */
package object math {
  import scala.{math => smath}

  /** Map value from one range to another proportionally */
  def map(value: Double, min1: Double, max1: Double, min2: Double, max2: Double): Double = {
    min2 + (max2 - min2) * (value - min1) / (max1 - min1)
  }

  /** Random number in range */
  def random(min: Double, max: Double): Double = map(smath.random(), 0, 1, min, max)

  /** Restricts a value to a range */
  def clamp(value: Double, min: Double, max: Double): Double = smath.min(max, smath.max(min, value))
}
