package lib

import scala.language.implicitConversions

/** Object for custom mathematical functions */
package object math {
  /** Precision for double checking */
  val Precision = 1e-13

  import scala.{math => smath}

  /** Map value in range (0, 1) to another proportionally */
  @inline def map(value: Double, min: Double, max: Double): Double = {
    min + (max - min) * value
  }

  /** Map value from one range to another proportionally */
  @inline def map(value: Double, min1: Double, max1: Double, min2: Double, max2: Double): Double = {
    min2 + (max2 - min2) * (value - min1) / (max1 - min1)
  }

  /** Random number in range */
  @inline def random(min: Double, max: Double): Double = {
    map(smath.random(), 0, 1, min, max)
  }

  /** Restricts a value to a range */
  @inline def clamp(value: Double, min: Double, max: Double): Double = {
    smath.min(max, smath.max(min, value))
  }

  implicit class RichDouble(value: Double) {
    def ~=(another: Double)(implicit p: Double = Precision): Boolean = (value - another).abs < p

    /** Checks if value lies in interval */
    @inline def isInInterval(min: Double, max: Double): Boolean = value > min && value < max
  }

  /** Ordered interval */
  case class Interval(min: Double, max: Double)

  implicit def toInterval(min: Double, max: Double): Interval = Interval(min, max)
  implicit def toInterval(max: Double): Interval = Interval(0, max)
  implicit def toInterval(t: (Double, Double)): Interval = Interval(t._1, t._2)

  /** Point of segments intersection.
    * [[http://www.cs.swan.ac.uk/~cssimon/line_intersection.html Solution from here]]
    *
    * @return Somw tish point of intersection or None if segments do not intersect
    */
  def segmentsIntersection(p1: Point, p2: Point, p3: Point, p4: Point): Option[Point] = {
    val denominator = (p4.x - p3.x) * (p1.y - p2.y) - (p1.x - p2.x) * (p4.y - p3.y)
    if (denominator == 0) None
    else {
      val t1: Double = ((p3.y - p4.y) * (p1.x - p3.x) + (p4.x - p3.x) * (p1.y - p3.y)) / denominator
      val t2: Double = ((p1.y - p2.y) * (p1.x - p3.x) + (p2.x - p1.x) * (p1.y - p3.y)) / denominator

      if (t1.isInInterval(0, 1) && t2.isInInterval(0, 1)) Some(p1 + t1 *: (p2 - p1))
      else None
    }
  }

  /** Finds an average in a sequence */
  @inline def average(numbers: Double*): Double = numbers.sum / numbers.length

  /** Finds an average in a sequence, regarding weights */
  def weightedAverage(weightedNumbers: (Double, Double)*): Double = {
    val totalGain = weightedNumbers.map(_._2).sum
    weightedNumbers.map(v => v._1 * v._2 / totalGain).sum / weightedNumbers.size
  }


}
