package lib

import lib.p5.Coordinates.Coordinates
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.input.KeyCode
import scalafx.scene.paint.Color

object p5 {
  object Coordinates extends Enumeration {
    type Coordinates = Value
    val Normal, Windowed = Value
  }

  def rotate(degrees: Double): Unit = gc.rotate(degrees)

  def translate(x: Double, y: Double): Unit = gc.translate(x, y)

  // todo
  val coordinates: Coordinates = Coordinates.Windowed

  def map(value: Double, min1: Double, max1: Double, min2: Double, max2: Double): Double = {
    min2 + (max2 - min2) * (value - min1) / (max1 - min1)
  }

  def random(min: Double, max: Double): Double = map(Math.random(), 0, 1, min, max)

  var key: Option[KeyCode] = None

  private var _gc: Option[GraphicsContext] = None
  def gc_=(gc: GraphicsContext): Unit = this._gc = Some(gc)
  def gc: GraphicsContext = _gc.get

  var width: Int = 0
  var height: Int = 0
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
}
