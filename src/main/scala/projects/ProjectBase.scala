package projects

import lib.p5
import scalafx.scene.input.KeyCode


class ProjectBase {
  val name = "Project"

  def draw(): Unit = {}

  def keyPressed(): Unit = if (p5.key.get == KeyCode.Escape) p5.exit()
}
