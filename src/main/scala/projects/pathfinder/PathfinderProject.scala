package projects.pathfinder

import lib.math.Point
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
    location = Point(lib.math.random(0, p5.width), lib.math.random(0, p5.height))


  }

  override def draw(): Unit = {
    p5.background(Color.White)
    pf.speed = (lib.math.random(-5, 5), lib.math.random(-5, 5))
    pf.move()
    pf.render()
    target.render()
  }
}
