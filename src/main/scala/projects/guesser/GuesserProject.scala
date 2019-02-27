package projects.guesser

import lib.p5
import projects.ProjectBase
import projects.shared.{Pathfinder, Target}
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color

object GuesserProject extends ProjectBase {
  override val name = "Guesser"

  /** Range in which  we're guessing */
  object range {
    val min: Double = -1
    val max: Double = 1
  }


  /** Padding from canvas's sides */
  private def padding = 50
  private object screen {
    object x {
      val left: Double = padding
      val right: Double = p5.width - padding
      val center: Double = p5.center.x
    }
    object y {
      val top: Double = padding
      val center: Double = p5.center.y + padding
      val bottom: Double = p5.height - padding
    }
  }

  /** Maps a number (either target or guess) to screen coordinates */
  private def mapVisual(value: Double): Int =
    p5.map(value, range.min, range.max, screen.x.left, screen.x.right).toInt

  /** === Target's position. ===
    * As for 1D problem, we'll only have '''x''' coordinate.
    */
  object target extends Target {
    y = screen.y.center.toInt
    /** Value we're guessing */
    var value: Double = generate()

    /** ===Generates target's value.===
      * Also adjusts x coordinate of visual representation */
    def generate(): Double = {
      value = p5.random(range.min, range.max)

      // We also need to map visual representation coordinate
      x = mapVisual(value)

      // Return value as byproduct
      value
    }
  }

  object pf extends Pathfinder {
    y = screen.y.center.toInt + padding
    private var _value: Option[Double] = None
    def value_=(x: Double): Double = {
      _value = Some(x)
      this.x = mapVisual(x)
      x
    }
    def value: Double = _value.get

    def isDefined: Boolean = _value.isDefined
  }

  val guesser = new Guesser(target.value)
  pf.value = guesser.getGuess

  def guess(): Unit = pf.value = guesser.approximate()
  override def draw(): Unit = {
    p5.background(Color.White)

    // Reference line
    p5.line(screen.x.left, screen.y.center, screen.x.right, screen.y.center)
    p5.text("-1", padding, screen.y.center + padding / 2)
    p5.text("0", screen.x.center, screen.y.center + padding / 2)
    p5.text("1", screen.x.right, screen.y.center + padding / 2)

    // Previous guesses
    guesser.guessesToGain.zipWithIndex.foreach(g2gI => {
      val opacity = (g2gI._2 + 1).toDouble / guesser.guessesToGain.size.toDouble
      p5.stroke(Color.rgb(55, 155, 0, opacity))
      val x = mapVisual(g2gI._1._1)
      val gain = p5.map(g2gI._1._2, 0, 1, screen.y.center, screen.y.top + padding)
      p5.line(x, screen.y.center, x, gain)
    })
    p5.stroke(Color.Black)

    // Draw target text
    target.render()
    p5.text("%.2f".format(target.value), target.x, screen.y.center - padding / 2)

    // Draw current guess
    if (pf.isDefined) {
      pf.render()
      p5.text("%.2f".format(pf.value), mapVisual(pf.value), pf.y + padding)
    }

    // Error data
    p5.text("Error " + "%.6f".format(guesser.error), padding, screen.y.top)

    // Iterations
    p5.text("Iterations " + guesser.iterations, padding, screen.y.top + p5.fontHeight)


  }
  override def keyPressed(): Unit = {
    super.keyPressed()

    if (p5.key.get == KeyCode.Space) guess()
  }
}
