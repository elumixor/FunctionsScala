import lib.math.{probability, _}
import org.scalatest.{FlatSpec, Matchers}

class probabilityTest extends FlatSpec with Matchers {
  implicit var tolerance: Double = 1e-2

  "average difference of randomly generated values" should "be roughly 50%" in {
    val numbersCount = 100000
    for (_ <- 1 to 10) {
      val reference = probability.random()
      val similar = for (_ <- 0 to numbersCount) yield probability.similar(reference)
      val average = lib.math.average(similar.map(x => (reference - x).abs): _*)
      assert(average ~= 0.5)
    }
  }

  "numbers" should "be similar to both" in {
    val first = 0
    val second = .5

    println(probability.similar2(first, second, 0, 0, 1, 1))

  }
}
