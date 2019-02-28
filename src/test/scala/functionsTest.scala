import org.scalatest._

class functionsTest extends FlatSpec with Matchers {

  import lib.math._

  private def squareApprox(min: Double, max: Double, value: Double = 0): Option[Double]
  = functions.approximateMonotonicFunction(x => x * x, min, max, value)

  it should "solve simple functions" in {
    assert(functions.approximateMonotonicFunction(x => x, -1, 1).get == 0.0)
    assert(squareApprox(-1, 0).get == 0.0)
    assert(squareApprox(0, 1).get == 0.0)
    assert(squareApprox(1, 1).isEmpty)
  }

  squareApprox(0, 10, 4) === (2.0 +- 1e-10)
  squareApprox(0, 10, 2) === (Math.sqrt(2) +- 1e-10)


  it should "work" in {
    val f: Double => Double = x => x  * x
    val g: Double => Double = x => 1 - x
    println(functions.findFunctionsIntersection(f, g, 0, 5))
  }
}
