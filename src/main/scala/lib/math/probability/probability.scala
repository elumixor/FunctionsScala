package lib.math

package object probability {

  import scala.math._

  // todo seeding, ets

  private val r = new scala.util.Random(System.currentTimeMillis())

  /** Generates a random number in (0, 1) */
  @inline def random(): Double = r.nextDouble()

  /** Generates a random number in range (0, max)
    *
    * @param max higher bound of random number
    */
  @inline def random(max: Double): Double = lib.math.map(random(), 0, max)

  /** Generates a random number in range (min, max)
    *
    * @param min lower bound of random number
    * @param max higher bound of random number
    */
  @inline def random(min: Double, max: Double): Double = lib.math.map(random(), min, max)

  /** Chooses random element from collection */
  def randomIn[A](elements: Seq[A]): A = elements((random() * elements.length).floor.toInt)

  /** Generate a similar number
    * todo doc
    *
    * @param reference  number to which the similar should be generated.
    * @param similarity likelihood of generating a number that is close to reference.
    * @param divergence maximum difference a generated number and reference can have
    * @return randomly generated similar number
    */
  def similar(reference: Double, similarity: Double = 0, divergence: Double = 1.0): Double = {
    reference + signum(random() - 0.5) * pow(random(), tan((similarity + 1) * Pi / 4)) * divergence
  }

  def similar2(r1: Double, r2: Double, s1: Double, s2: Double, div1: Double, div2: Double): Option[Double] = {
    val fToMax: Double => Double = R => r1 + pow(R, tan((s1 + 1) * Pi / 4)) * div1
    val fToMin: Double => Double = R => r1 - pow(R, tan((s1 + 1) * Pi / 4)) * div1

    val gToMax: Double => Double = R => r2 + pow(1 - R, tan((s2 + 1) * Pi / 4)) * div2
    val gToMin: Double => Double = R => r2 - pow(1 - R, tan((s2 + 1) * Pi / 4)) * div2

    def matchSeq(roots: Seq[Point]): Option[(Interval, Interval)] = {
      println(roots)
      roots.size match {
        case 0 => None
        case 1 => Some((roots.head.x, 1.0), (1 - roots.head.x, 1.0))
        case 2 =>
          val m = max(roots.head.x, roots(1).x)
          Some((m, 1.0), (1 - m, 1.0))
      }
    }

    val intervals = Seq((lib.math.functions.findFunctionsIntersection(fToMax, gToMax, 0, 1), fToMax, gToMax),
      (lib.math.functions.findFunctionsIntersection(fToMax, gToMin, 0, 1), fToMax, gToMin),
      (lib.math.functions.findFunctionsIntersection(fToMin, gToMax, 0, 1), fToMin, gToMax),
      (lib.math.functions.findFunctionsIntersection(fToMin, gToMin, 0, 1), fToMin, gToMin))
                    .map(x => (matchSeq(x._1.toSeq), x._2, x._3)).filter(_._1.isDefined).map(x => (x._1.get, x._2, x._3))

    if (intervals.isEmpty) None
    else {
      val i1 = randomIn(intervals)

      if (randomIn(Seq(0, 1)) == 0) Some(i1._2(map(random(), i1._1._1.min, i1._1._1.max)))
      else Some(i1._3(map(random(), i1._1._2.min, i1._1._2.max)))
    }
  }
}
