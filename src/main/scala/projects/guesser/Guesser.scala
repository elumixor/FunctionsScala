package projects.guesser

import lib.p5

class Guesser(val target: Double, val min: Double = -1, val max: Double = 1) {
  /** ===Guesses next (closer to the value) number===
    * Iteratively gets closer towards the value
    */
  def approximate(): Double = {
    p5.random(min, max)
  }
}
