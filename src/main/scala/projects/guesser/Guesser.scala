package projects.guesser

import lib.p5

class Guesser(val target: Double, val min: Double = -1, val max: Double = 1) {
  private var guess: Double = lib.math.random(min, max)
  def getGuess: Double = guess
  private val MSE = lib.math.Function("((target - guess)/2)^2")(Map("target" -> target))

  /** Map of guess -> gain */
  var guessesToGain: Map[Double, Double] = Map(guess -> gain(guess))
  /** Maximum records in map */
  val maxData: Double = 100

  private var _iterations = 0
  def iterations: Int = _iterations

  def error: Double = Math.abs(target - guess)

  /** How good a guess is */
  def gain(guess: Double): Double = 1.0 - MSE(guess).value.get

  def randomNumber(value: Double, similarity: Double, random: Double): Double = {
    val S = Math.tan(lib.math.map(similarity, -1, 1, Math.PI / 2, 0))
    val sign: Double = if (lib.math.random(0, 1) >= 0.5) 1 else -1
    value + sign * (random - 1) * S
  }

  /** Generate a similar value
    *
    * @param similarity shows how similar or different generated number should be:<br/>
    *                   `1.0` means generating exactly `n` (same number)<br/>
    *                   `-1.0` means generating  `-n` (exactly opposite)<br/>
    *                   `0.0` means generating indifferent to `n`
    */
  def similar(value: Double, similarity: Double): Double = {
    val R: Double = lib.math.random(0, 1)

    randomNumber(value, similarity, R)
  }

  /** Generate value similar to multiple values
    *
    * @param guesses is a map of weighted previous guesses.
    */
  def similarAverage(guesses: Map[Double, Double]): Double = {
    //    val R = p5.random(0, 1)
    //    val mapped = guesses.map(f => {
    //      val V = f._1
    //      val S = Math.tan(p5.map(f._2, -1, 1, 0, Math.PI / 2))
    //      val sign = if (p5.random(0, 1) >= 0.5) 1 else -1
    //
    //      (sign * V * S, sign * S)
    //    })
    //
    //    val numerator = -mapped.keys.sum + R - (guesses.size - 1)
    //    val denominator = mapped.values.sum
    //
    //    numerator/ denominator

    val totalGain = guesses.values.sum
    val number = guesses.map(v => v._1 * v._2 / totalGain).sum / guesses.size
    val similarity = .5
    similar(number, similarity)

  }

  /** ===Guesses next (closer to the value) number===
    * Iteratively gets closer towards the value
    */
  def approximate(): Double = {
    _iterations += 1

    guess = similarAverage(guessesToGain)
    //    guess = similar(guess, gain(guess))
    guessesToGain += (guess -> gain(guess))
    if (guessesToGain.size > maxData) guessesToGain = guessesToGain.drop(1)

    guess
  }
}
