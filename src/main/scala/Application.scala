object Application {
  def main(args: Array[String]): Unit = {
    var sqrt = Function("sqrt = x ^ (1/2)")
    //    println(cos.body)
    println(Function("f = sqrt(1 - sin(x) ^ 2)")(2))
    //    println(cos(1))
    //    println(Function("f = 1 + 3*x*cos(y + x)").body)
    //    println(Function("f = 1 + 3*x*cos(y + x)")(1, 2).body)
    //        println(Function("f =   3 * (2) + sin(x + y)"))
//        val f = Function("f = sin(cos(3 * ((sin(2) + (4))) + sin(x + y) + x) + 2 + cos(3))")
    //    println(f(3, 4))
    val f = Function("f = sin(cos(3 * ((sin(2) + 4) * (4 - x)) + sin(x + y) + 2) + 2 + cos(3)) + 3*x*cos(y + x)")
//            println(f)
    println(f(1, 2))
    //    val f = Function("f = sin(cos(3 * ((sin(2) + (4)) * (4 - x)) + sin(x + y) + x) + 2 + cos(3)) + 3*x*cos(y + x)")
    //    val f = Function("f = sin(cos(3 * ((sin(2) + (4)) * (4 - x)) + sin(x + y) + x) + 2 + cos(3)) + 3*x*cos(y + x)")
    //    val f = Function("f = sin(cos(3 * ((sin(2) + (4)) * (4 - x)) + sin(x + y) + x) + 2 + cos(3)) + 3*x*cos(y + x)")
    //    val f = Function("f = sin(cos(3 * ((sin(2) + (4)) * (4 - x)) + sin(x + y) + x) + 2 + cos(3)) + 3*x*cos(y + x)")
    //    val f = Function("f = sin(cos(3 * ((sin(2) + (4)) * (4 - x)) + sin(x + y) + x) + 2 + cos(3)) + 3*x*cos(y + x)")
    //    val f = Function("f = sin(cos(3 * ((sin(2) + (4)) * (4 - x)) + sin(x + y) + x) + 2 + cos(3)) + 3*x*cos(y + x)")
    //    val f = Function("f =")
    //    println(s"Successfully parsed function '${f.name}' with args (${f.args.mkString(", ")}) and body ${f.body}")

    val x = 5
    val y = 6

    //    println(s"Value at x = $x, y = $y\nf($x, $y) = ${f(5, 6)}")

    //        val k = Function("f = sin(cos(3 * x * y))")


    //        println(k.body + " at 5, 3")
    //        println(k.args)
    //        println(k(Map("x" -> 5.0, "y" -> 3.0)))

    //    println(Function("f = 3").body)
    //    println(Function("f = 3"))
    //    println(Function("f = (3)").body)
    //    println(Function("f = (3)"))
    //    println(Function("f = (-3)").body)
    //    println(Function("f = (-3)"))
    //    println(Function("f = x"))
    //    println(Function("f = -x"))
    //    println(Function("f = -(x)"))
    //    println(Function("f = (-x)"))
    //    println(Function("f = (((x)))"))

    //        println(Function("f = (4 * (x + 3))")(3))
    //        println(Function("f = 4 * (x + 3)")(3))
  }
}
