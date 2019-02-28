import lib.math.functions.StringFunction
import org.scalatest._

class StringFunctionTest extends FlatSpec {
  "const" should "be a valid function" in {
    assert(StringFunction("f = 3").value.get == 3)
    assert(StringFunction("f = 3 * 3").value.get == 9)
  }

  it should "correctly remove redundant braces" in {
    assert(StringFunction("sqrt = x ^ (1/2)").toString == "exp(x,div(1,2))")
  }

  private var count = 1
  private def ordered(f: => Any): Unit = {
    print(count + ": ")
    f
    println()
    count += 1
  }

  it should "work" in {
    lib.math.functions.StringFunction("-a * 2")
    lib.math.functions.StringFunction("-a ^ 2")
    lib.math.functions.StringFunction("c - d + a")
    lib.math.functions.StringFunction("c - d + a - b")
    lib.math.functions.StringFunction("(c - d) + (a - b)")
    lib.math.functions.StringFunction("(c - d) + a")
    lib.math.functions.StringFunction("(c - d) + (a)")
    lib.math.functions.StringFunction("(c - d) + a * 2")
    lib.math.functions.StringFunction("(c - d) + (a - b) *  2")
  }


}
