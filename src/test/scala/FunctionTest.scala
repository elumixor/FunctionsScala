import lib.Function
import org.scalatest._

class FunctionTest extends FlatSpec {
  "const" should "be a valid function" in {
    assert(Function("f = 3").value.get == 3)
    assert(Function("f = 3 * 3").value.get == 9)
  }

  it should "correctly remove redundant braces" in {
    assert(Function("sqrt = x ^ (1/2)").toString == "exp(x,div(1,2))")
  }

  private var count = 1
  private def ordered(f: => Any): Unit = {
    print(count + ": ")
    f
    println()
    count += 1
  }

  it should "work" in {
    lib.Function("-a * 2")
    lib.Function("-a ^ 2")
    lib.Function("c - d + a")
    lib.Function("c - d + a - b")
    lib.Function("(c - d) + (a - b)")
    lib.Function("(c - d) + a")
    lib.Function("(c - d) + (a)")
    lib.Function("(c - d) + a * 2")
    lib.Function("(c - d) + (a - b) *  2")

  }


}
