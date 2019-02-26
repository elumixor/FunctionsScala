package projects.guesser

import lib.p5

class Guesser(val target: Double, val min: Double = -1, val max: Double = 1) {
  private var guess: Double = p5.random(min, max)
  private val error = lib.Function("(target - guess)^2")
  private val derived = error.derivative("guess")

  println(error)
  println(derived)

  private val learningRate = .01

  /** ===Guesses next (closer to the value) number===
    * Iteratively gets closer towards the value
    */
  def approximate(): Double = {
    println("Previous guess: " + guess)

    println(error(Map("target" -> target, "guess" -> guess)).value)

    guess -= learningRate * derived(Map("target" -> target, "guess" -> guess)).value.get
    guess
  }
}
