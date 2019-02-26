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
    val left: Double = padding
    def right: Double = p5.width - padding
    def center: Double = p5.center.x
  }

  /** Maps a number (either target or guess) to screen coordinates */
  private def mapVisual(value: Double): Int =
    p5.map(value, range.min, range.max, screen.left, screen.right).toInt

  /** === Target's position. ===
    * As for 1D problem, we'll only have '''x''' coordinate.
    */
  object target extends Target {
    y = p5.center.y
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
    y = p5.center.y + padding
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

  def guess(): Unit = pf.value = guesser.approximate()
  override def draw(): Unit = {
    p5.background(Color.White)

    // Reference line
    p5.line(screen.left, p5.center.y, screen.right, p5.center.y)
    p5.text("-1", padding, p5.center.y + padding / 2)
    p5.text("0", screen.center, p5.center.y + padding / 2)
    p5.text("1", screen.right, p5.center.y + padding / 2)

    // Draw target
    target.render()
    p5.text("%.2f".format(target.value), target.x, p5.center.y - padding / 2)

    // Draw guess
    if (pf.isDefined) {
      pf.render()
      p5.text("%.2f".format(pf.value), mapVisual(pf.value), pf.y + padding)
    }

    // Error data
    if (pf.isDefined) p5.text("Error " + "%.6f".format(guesser.error), padding, padding)
  }
  override def keyPressed(): Unit = {
    super.keyPressed()

    if (p5.key.get == KeyCode.Space) guess()
  }
}
