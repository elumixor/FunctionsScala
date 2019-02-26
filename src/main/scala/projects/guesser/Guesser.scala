package projects.guesser

import lib.p5

class Guesser(val target: Double, val min: Double = -1, val max: Double = 1) {
  private var guess: Double = p5.random(min, max)
  private val _error = lib.Function("(target - guess)^2") // <- This is saying "I Know whee the target IS!
  private val derived = _error.derivative("guess")



  private var _iterations = 0
  def iterations: Int = _iterations

  def error: Double = Math.abs(target - guess)

  private val learningRate = .1

  /** ===Guesses next (closer to the value) number===
    * Iteratively gets closer towards the value
    */
  def approximate(): Double = {
    _iterations += 1
    guess -= learningRate * derived(Map("target" -> target, "guess" -> guess)).value.get
    guess
  }
}
