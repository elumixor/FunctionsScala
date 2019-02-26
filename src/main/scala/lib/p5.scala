package lib

import lib.p5.Coordinates.Coordinates
import scalafx.application.JFXApp
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

object p5 {

  private var _gc: Option[GraphicsContext] = None
  def gc_=(gc: GraphicsContext): Unit = this._gc = Some(gc)
  def gc: GraphicsContext = _gc.get

  private var _stage: Option[JFXApp.PrimaryStage] = None
  def stage_=(stage: JFXApp.PrimaryStage): Unit = this._stage = Some(stage)
  def stage: JFXApp.PrimaryStage = _stage.get

  var key: Option[KeyCode] = None

  /** Exit the program */
  def exit(): Unit = stage.close()

  object Coordinates extends Enumeration {
    type Coordinates = Value
    val Normal, Windowed = Value
  }

  // todo
  val coordinates: Coordinates = Coordinates.Windowed

  /** Map value from one range to another proportionally */
  def map(value: Double, min1: Double, max1: Double, min2: Double, max2: Double): Double = {
    min2 + (max2 - min2) * (value - min1) / (max1 - min1)
  }


  // Math

  // Transformations
  def translate(x: Double, y: Double): Unit = gc.translate(x, y)
  def rotate(degrees: Double): Unit = gc.rotate(degrees)

  /** Random number in range */
  def random(min: Double, max: Double): Double = map(Math.random(), 0, 1, min, max)

  // Graphics


  var height: Int = 0
  var width: Int = 0
  def center: Point = Point(width / 2, height / 2)


  def background(color: Color): Unit = {
    val previousFill = gc.fill
    gc.fill = color
    gc.fillRect(0, 0, width, height)
    gc.fill = previousFill
  }

  def fill(color: Color): Unit = gc.fill = color
  def ellipse(x: Int, y: Int, diameter: Int): Unit = ellipse(x, y, diameter, diameter)
  def ellipse(x: Int, y: Int, width: Int, height: Int): Unit = gc.fillOval(x - width / 2, y - height / 2, width, height)
  def triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double): Unit = {
    gc.fillPolygon(Seq((x1, y1), (x2, y2), (x3, y3)))
  }
  def rectangle(x: Int, y: Int, width: Int, height: Int): Unit = {
    gc.fillRect(x, y, width, height)
  }
  def polygon(points: Seq[(Int, Int)]): Unit = gc.fillPolygon(points.map(p => (p._1.toDouble, p._2.toDouble)))
  def text(text: String, x: Double, y: Double): Unit = gc.strokeText(text, x, y)
  def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit = gc.strokeLine(x1, y1, x2, y2)

  // Register functions
  lib.Function("x ^ (1/2)") as "sqrt"
}
