package projects.pathfinder

import lib.math.Point
import lib.math.probability._
import lib.p5
import projects.ProjectBase
import projects.shared.{Pathfinder, Target}
import scalafx.scene.paint.Color

object PathfinderProject extends ProjectBase {
  //  val distance = lib.math.functions.Function("sqrt((xpf - xt) ^ 2 + (ypf - yt) ^ 2)")
  val distance = lib.math.functions.StringFunction("(c - d) + (a - b)^2")

  private val pf = new Pathfinder() {
    location = p5.center
    speed = Point(1, 2)
  }
  private val target = new Target() {
    location = Point(random(0, p5.width), random(0, p5.height))


  }

  override def draw(): Unit = {
    p5.background(Color.White)
    pf.speed = (random(-5, 5), random(-5, 5))
    pf.move()
    pf.render()
    target.render()
  }
}
