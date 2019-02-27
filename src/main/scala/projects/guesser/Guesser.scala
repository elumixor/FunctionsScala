package projects.guesser

import lib.p5

import scala.collection.SortedMap

class Guesser(val target: Double, val min: Double = -1, val max: Double = 1) {
  private var guess: Double = p5.random(min, max)
  def getGuess: Double =  guess
  private val MSE = lib.Function("((target - guess)/2)^2")(Map("target" -> target))

  /** Map of guess -> gain */
  var guessesToGain: Map[Double, Double] = Map(guess -> gain(guess))
  /** Maximum records in map */
  val maxData: Double = 100

  private var _iterations = 0
  def iterations: Int = _iterations

  def error: Double = Math.abs(target - guess)

  /** How good a guess is */
  def gain(guess: Double): Double = 1.0 - MSE(guess).value.get

  /** Generate a similar value */
  def similar(n: Double, maxDifference: Double): Double = {
    val g: Double = p5.random(0, 1)
    val sign: Double = if (p5.random(0, 1) >= 0.5) 1 else -1

    p5.map(g, 0, 1, n, n + maxDifference * sign)
  }

  /** ===Guesses next (closer to the value) number===
    * Iteratively gets closer towards the value
    */
  def approximate(): Double = {
    _iterations += 1

    var s = 0.0
    do { s = similar(guess, 1.0  -  gain(guess)) } while (s < -1 || s  > 1)
    guess = s

    guessesToGain += (guess -> gain(guess))
    if (guessesToGain.size > maxData) guessesToGain =  guessesToGain.drop(1)

    guess
  }
}
