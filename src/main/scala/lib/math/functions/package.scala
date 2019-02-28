package lib.math

import lib.math

package object functions {

  /** Finds the value of an argument of monotonic function in interval (min, max) where it equals
    * a value (0 if unspecified).
    *
    * @param f   First function
    * @param min interval start
    * @param max interval end
    * @return Some containing value of argument where functions equals , else None
    */
  def approximateMonotonicFunction(f: Double => Double, min: Double, max: Double, value: Double = 0)
                                  (implicit precision: Double = math.Precision): Option[Double] = {
    val avg = average(min, max)
    if (min ~= max) {
      val closest = Seq(min, avg, max).map(x => (x, (f(x) - value).abs)).minBy(_._2)
      if (closest._2 > precision) None else Some(closest._1)
    } else {
      val vMin = f(min)
      val vMax = f(max)
      if (!value.isInInterval(vMin, vMax)) return {
        if (value ~= vMin) Some(min)
        else if (value ~= vMax) Some(max)
        else None
      }
      val mid = avg

      if (f(mid) > value) approximateMonotonicFunction(f, min, mid, value)
      else approximateMonotonicFunction(f, mid, max, value)
    }
  }

  /** Finds a point of intersection of two monotonic functions in interval (a, b)
    *
    * @param f First function
    * @param g Second function
    * @param a interval start
    * @param b interval end
    * @return Some containing value of argument where functions intersect, else None
    */
  def findFunctionsIntersection(f: Double => Double, g: Double => Double, a: Double, b: Double): Seq[Point] = {
    // assert a < b ? Does reversing the range influence anything? (probably not)
    val fa = f(a)
    val fb = f(b)

    val ga = g(a)
    val gb = g(b)

    if (fa ~= ga) return Seq((a, average(fa, ga)))
    if (fb ~= gb) return Seq((b, average(fb, gb)))

    /** Returns (min, max) bounds of function values in range */
    def sortedBounds(a: Double, b: Double): Interval = if (a > b) (b, a) else (a, b)

    val boundsF = sortedBounds(fa, fb)
    val boundsG = sortedBounds(ga, gb)

    if (boundsF.min > boundsG.max || boundsF.max < boundsG.min) return Seq()

    val p1 = Point(a, fa)
    val p2 = Point(b, fb)
    val p3 = Point(a, ga)
    val p4 = Point(b, gb)

    val same = math.segmentsIntersection(p1, p2, p3, p4)
    (if (same.isDefined) {
      findFunctionsIntersection(f, g, a, same.get.x) ++ findFunctionsIntersection(f, g, same.get.x, b)
    } else {
      val different = math.segmentsIntersection(p1, p4, p3, p2)
      findFunctionsIntersection(f, g, a, different.get.x) ++ findFunctionsIntersection(f, g, different.get.x, b)
    }).distinct
  }
}
