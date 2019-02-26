import lib.p5
import projects.ProjectBase
import projects.pathfinder.PathfinderProject
import projects.guesser.GuesserProject
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas._

object Application extends JFXApp {
  val w = 600
  val h = 450
  var project: ProjectBase = _
  val timer = AnimationTimer(_ => project.draw())

  stage = new JFXApp.PrimaryStage {
    width = w
    height = h


    scene = new Scene {
      private val canvas = new Canvas(w, h)
      private val gc = canvas.graphicsContext2D
      content = canvas
      p5.stage = stage
      p5.gc = gc
      p5.height = h
      p5.width = w


      onKeyPressed = k => {
        p5.key = Some(k.code)
        project.keyPressed()
        p5.key = None
      }
    }
  }

  p5.stage = stage
  project = GuesserProject
  stage.title.value = project.name
  timer.start()
}
