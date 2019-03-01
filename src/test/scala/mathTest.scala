import lib.math._
import org.scalatest._

class mathTest extends FlatSpec with Matchers {
  it should "work" in {
    println((1.0, 2.0).intersects((0.5, 2.5)))
    println((0.0, 1.0).intersects((.5, 2.5)))
  }

  "not intersecting intervals" should "not intersect" in {
    assert((0.0, 1.0).intersects(2.0, 3.0).isEmpty)
    assert((2.0, 3.0).intersects(0.0, 1.0).isEmpty)
  }

  "intersecting intervals" should "intersect" in {
    assert((0.0, 1.0).intersects(0.5, 1.5).get == Interval(0.5, 1.0))
    assert((0.0, 2.0).intersects(0.5, 1.0).get == Interval(0.5, 1.0))
  }


  "multiple intersecting intervals" should "intersect" in {
    assert(Interval.intersections((0.0, 1.0), (0.5, 1.5), (-1.0, 0.75), (0.5, 0.75), (0.0, 0.5)).isDefined)
  }

  "multiple intersecting with one not intersect" should "not intersect" in {
    assert(Interval.intersections((0.0, 1.0), (0.5, 1.5), (-1.0, 0.75), (0.5, 0.75), (0.0, 0.5), (-1.0, 0.0)).isEmpty)
  }


  // Not intersecting

  // Reversed

  (0.0, 1.0).intersects(.5, 2.5) === (0.5, 1.0)
  (1.0, 2.0).intersects(0.5, 2.5) === (0.5, 2.0)
}
