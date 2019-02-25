import lib.p5
import projects.ProjectBase
import projects.pathfinder.PathfinderProject
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas._
import scalafx.scene.input.KeyCode

object Application extends JFXApp {
  val w = 600
  val h = 450
  var project: ProjectBase = PathfinderProject

  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = w
    height = h

    scene = new Scene {
      private val canvas = new Canvas(w, h)
      private val gc = canvas.graphicsContext2D
      content = canvas
      p5.gc = gc
      p5.height = h
      p5.width = w

      val timer = AnimationTimer(_ => project.draw())
//      var timerRunning = false
//      private def switchTimer(): Unit = {
//        if (timerRunning) timer.stop()
//        else timer.start()
//        timerRun  ning = !timerRunning
//      }

      onKeyPressed = k => {
        p5.key = Some(k.code)
        project.keyPressed()
        p5.key = None
      }

      project.setup()
      timer.start()
    }
  }
}
