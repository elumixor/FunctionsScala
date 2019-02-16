import org.scalatest._

import scala.collection.immutable.Stack

class FunctionTest extends FlatSpec {
  "const" should "be a valid function" in {
    assert(Function("f = 3").value.get == 3)
    assert(Function("f = 3 * 3").value.get == 9)
  }

  it should "correctly remove redundant braces" in {
    assert(Function("sqrt = x ^ (1/2)").body == "exp(x,div(1,2))")
  }
}
