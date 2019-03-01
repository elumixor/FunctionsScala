package projects.guesser

import lib.math._

class Guesser(val target: Double, val min: Double = -1, val max: Double = 1) {
  private var _guess: Double = lib.math.probability.random(min, max)
  def guess: Double = _guess
  private val MSE = lib.math.functions.StringFunction("((target - guess)/2)^2")(Map("target" -> target))

  /** Map of guess -> gain */
  var guessesToGain: Seq[(Double, Double)] = Seq((_guess, gain(_guess)))
  /** Maximum records in map */
  val maxData: Double = 100

  private var _iterations = 0
  def iterations: Int = _iterations

  def error: Double = Math.abs(target - _guess)

  /** How good a guess is */
  def gain(guess: Double): Double = 1.0 - MSE(guess).value.get

  var _min = 0.0
  var _max = 0.0

  /** ===Guesses next (closer to the value) number===
    * Iteratively gets closer towards the value
    */
  def approximate(): Option[Double] = {
    _iterations += 1

    //    val occurrences = guessesToGain.count(_._1 == _guess)
    //    val similarity = math.pow(gain(_guess), occurrences + 1) * 2 - 1 // more occurrences -> lower similarity
    //
    //    _guess = clamp(similar(_guess, similarity), -1, 1)
    val first = .25
    val second = -.25

    val g = probability.similar2(first, second, 0.9, 0.9, .5, .5)

    println(s"($min, $max)")

    if (g.isDefined) {
      if (g.get < _min) _min = g.get
      else if (g.get > _max) _max = g.get

      guessesToGain = guessesToGain :+ (g.get, gain(g.get))
      //    guessesToGain = guessesToGain :+ (_guess, gain(_guess))
      if (guessesToGain.size > maxData) guessesToGain = guessesToGain.drop(1)
    }

    g
  }
}
