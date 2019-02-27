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

  /** Generate a similar value
    * @param similarity shows how similar or different generated number should be:<br/>
    *                   `1.0` means generating exactly `n` (same number)<br/>
    *                   `-1.0` means generating  `-n` (exactly opposite)<br/>
    *                   `0.0` means generating indifferent to `n`
    */
  def similar(n: Double, similarity: Double): Double = {
    val R: Double = p5.random(0, 1)
    val sign: Double = if (p5.random(0, 1) >= 0.5) 1 else -1

    val V =p5.map(n, -1, 1, 0, 1)
    val S =  Math.tan(p5.map(similarity, -1, 1, Math.PI / 2, 0))
    p5.map(V + sign * (R - 1) * S, 0, 1, -1, 1)
  }

  /** Generate value similar to multiple values
    * @param guesses is a map of weighted previous guesses.
    */
//  def similarAverage(guesses: Map[Double, Double]): Double = {
//
//  }

  /** ===Guesses next (closer to the value) number===
    * Iteratively gets closer towards the value
    */
  def approximate(): Double = {
    _iterations += 1

    var s = 0.0
    do { s = similar(guess, gain(guess)) } while (s < -1 || s  > 1)
    guess = s

    guessesToGain += (guess -> gain(guess))
    if (guessesToGain.size > maxData) guessesToGain =  guessesToGain.drop(1)

    guess
  }
}
