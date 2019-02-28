package lib

package object probability {

  import scala.math._
  import scala.{math => smath}

  // todo seeding, ets

  /** Generates a random number in (0, 1) */
  @inline def random(): Double = smath.random()

  /** Generates a random number in range (0, max)
    *
    * @param max higher bound of random number
    */
  @inline def random(max: Double): Double = math.map(random(), 0, max)

  /** Generates a random number in range (min, max)
    *
    * @param min lower bound of random number
    * @param max higher bound of random number
    */
  @inline def random(min: Double, max: Double): Double = math.map(random(), min, max)

  /** Generate a similar number
    * todo doc
    *
    * @param reference  number to which the similar should be generated.
    * @param similarity likelihood of generating a number that is close to reference.
    * @param divergence maximum difference a generated number and reference can have
    * @return randomly generated similar number
    */
  def similar(reference: Double, similarity: Double = 0, divergence: Double = 1.0): Double = {
    reference + signum(random()) * pow(random(), tan(similarity * Pi / 2)) * divergence
  }

}
