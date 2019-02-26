package projects.pathfinder

import lib.{Vec2, p5}
import projects.ProjectBase
import projects.shared.{Pathfinder, Target}
import scalafx.scene.paint.Color

object PathfinderProject extends ProjectBase {
//  val distance = lib.Function("sqrt((xpf - xt) ^ 2 + (ypf - yt) ^ 2)")
  val distance = lib.Function("(c - d) + (a - b)^2")

  private val pf = new Pathfinder() {
    location = p5.center
    speed = Vec2(1, 2)
  }
  private val target = new Target() {
    location = Vec2(p5.random(0, p5.width), p5.random(0, p5.height)).toPoint
  }

  override def draw(): Unit = {
    p5.background(Color.White)
    pf.speed = (p5.random(-5, 5), p5.random(-5, 5))
    pf.move()
    pf.render()
    target.render()
  }
}
