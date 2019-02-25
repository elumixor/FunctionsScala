package projects.pathfinder

import lib.{Vec2, p5}
import projects.ProjectBase
import scalafx.scene.paint.Color

import scala.util.Random

object PathfinderProject extends ProjectBase {
  private val pf = Pathfinder
  private val target = new Target()

  override def setup(): Unit = {
    pf.location = p5.center
    pf.speed = Vec2(1, 2)
//    pf.acceleration = Vec2(-1, -1)
    target.location = Vec2(p5.random(0, p5.width), p5.random(0, p5.height)).toPoint
  }

  override def draw(): Unit = {
    p5.background(Color.White)
    pf.speed = (p5.random(-5, 5), p5.random(-5, 5))
    pf.move()
    pf.render()
    target.render()
  }
}
