import java.util.NoSuchElementException

import scala.language.implicitConversions

object Function {
  private case class F(body: Seq[Double] => Double, name: String)

  private abstract sealed class Leaf extends Function(Seq())
  private class Constant(_value: Double) extends Leaf {
    override def copy: Function = new Constant(_value)
    override def value: Option[Double] = Some(_value)
  }
  private class Identity(val argument: String) extends Leaf {
    override def copy: Function = new Identity(argument)
  }

  private class Branch(val func: F, leaves: Seq[Function]) extends Function(leaves) {
    override def copy: Function = new Branch(func, leaves.map(_.copy))
  }
  private class Primitive(func: F, arguments: Seq[String]) extends Branch(func, arguments.map(a => new Identity(a)))

  /** Primitive predef functions */
  private object primitives {

    /** Primitive with a symbol */
    sealed trait Symbolic {
      val symbol: Char
    }

    object sin extends Primitive(F(args => Math.sin(args.head), "sin"), Seq("x"))
    object cos extends Primitive(F(args => Math.cos(args.head), "cos"), Seq("x"))
    object add extends Primitive(F(args => args.head + args(1), "add"), Seq("x", "y")) with Symbolic {
      override val symbol: Char = '+'
    }
    object neg extends Primitive(F(args => -args.head, "neg"), Seq("x")) with Symbolic {
      override val symbol: Char = '-'
    }
    object mul extends Primitive(F(args => args.head * args(1), "mul"), Seq("x", "y")) with Symbolic {
      override val symbol: Char = '*'
    }
    object div extends Primitive(F(args => args.head / args(1), "div"), Seq("x", "y")) with Symbolic {
      override val symbol: Char = '/'
    }
    object exp extends Primitive(F(args => Math.pow(args.head, args(1)), "exp"), Seq("x", "y")) with Symbolic {
      override val symbol: Char = '^'
    }

    val toSeq: Seq[Primitive] = Seq(neg, add, mul, div, exp, sin, cos)
    val symbolic: Seq[Primitive with Symbolic] = toSeq.flatMap { case s: Symbolic => Some(s) case _ => None }
  }

  /** Registered functions */
  private var registered: Map[String, Function] = primitives.toSeq.map(p => p.func.name -> p).toMap

  /** Register new function */
  def register(f: Function, name: String): Unit = registered = registered - name + (name -> f)

  /** Checks if symbol is an operand */
  private val isOperand = (char: Char) => primitives.symbolic.exists(_.symbol == char)

  /**
    * Searches for first enclosing scope of a string, regarding parenthesis pairs
    *
    * @param string    String to be searched within
    * @param direction Direction of search. Symbol which is either `'fromStart` or anything else.
    *                  If is `'fromStart`, then function searches for the first enclosing scope from
    *                  start of the string. Else the search is done from string's end
    * @return Returns a Tuple - pair of where the enclosing scope starts and where does it end.
    *         If no scopes were found, returns `(0, string.length)`
    */
  private def findEnclosingScope(string: String, direction: Symbol = 'fromStart): (Int, Int) = {
    var brackets = 0

    if (direction == 'fromStart) {
      for ((char, i) <- string.zipWithIndex) {
        if (char == '(') brackets += 1
        else if (char == ')') brackets -= 1

        if (brackets < 0 || (brackets == 0 && isOperand(char))) return (0, i)
      }

      (0, string.length)
    } else {
      for ((char, i) <- string.zipWithIndex.reverse) {
        if (char == ')') brackets += 1
        else if (char == '(') brackets -= 1

        if (brackets < 0 || (brackets == 0 && isOperand(char))) return (i + 1, string.length)
      }

      (0, string.length)
    }
  }

  /** Recursively replace unary operators */
  private def replaceUnary(string: String, operator: Char, replacement: String): String = {
    // Split by first occurrence of operand
    val split = string.split(operator)

    // If no operand found
    if (split.length == 1) return string

    // Find argument bounds
    val tmp = findEnclosingScope(string.substring(split(0).length + 1, string.length))
    // Find operand's bounds
    val bounds = (split(0).length + 1 + tmp._1, split(0).length + 1 + tmp._2) // offset by first occurrence

    // Operand as string
    val operand = string.substring(bounds._1, bounds._2)

    // Determine if should add +
    val addPlus = if ((split(0).last == ')' || !isOperand(split(0).last)) && split(0).last != '(') "+" else ""

    // Repeat recursively until no operands found
    replace(string.substring(0, split(0).length) + addPlus + replacement + '(' + operand + ')' + string.substring
    (bounds._2, string.length),
      operator, replacement)
  }

  /** Recursively replaces symbolic operators with text function: x + y -> add(x,y) to simplify parsing */
  private def replace(string: String, operator: Char, replacement: String): String = {
    // Split by first occurrence of operand
    val split = string.split(operator)

    // If no operand found
    if (split.length == 1) return string

    // Find left operand's bounds
    val left = findEnclosingScope(split(0), 'fromEnd)
    val tmp = findEnclosingScope(string.substring(split(0).length + 1, string.length))
    // Find right operand's bounds
    val right = (split(0).length + 1 + tmp._1, split(0).length + 1 + tmp._2) // offset by first occurrence

    // Operands as strings
    val leftOperand = string.substring(left._1, left._2)
    val rightOperand = string.substring(right._1, right._2)

    // String remainders
    val beforeLeft = string.substring(0, left._1)
    val afterRight = string.substring(right._2, string.length)

    // Repeat recursively until no operands found
    replace(beforeLeft + replacement + "(" + leftOperand + "," + rightOperand + ")" + afterRight,
      operator, replacement)
  }

  /**
    * Removes redundant parenthesis from the string. Returns a new string without redundancies
    *
    * @param string String to be optimized
    * @return New string without redundant parenthesis
    */
  private def removeRedundantParenthesis(string: String): String = {
    println(string)
    val indexed = string.zipWithIndex

    // Positions of opening brackets, indexed
    val opening = indexed.filter(_._1 == '(').map(_._2)

    // Seq of position of the bracket to be removed and it's index within all opening brackets
    val openingRemoved = opening.filter(el => el == 0 || string(el - 1) == '(' || string(el - 1) == ',')

    // For each bracket find enclosing scope
    val closingRemoved = openingRemoved.map(el => el + findEnclosingScope(string.substring(el + 1))._2)

    val toBeRemoved = openingRemoved ++ closingRemoved

    indexed.filter(x => !toBeRemoved.contains(x._2)).map(_._1).mkString("")
  }

  /** Transforms function string into easier-to-parse format */
  private def adjust(string: String): String = {
    // Replace symbols with function names
    val tmp = replace(replace(replace(replaceUnary(replace(
      s"(${string.filterNot(_.isWhitespace).trim})", // get rid of whitespaces
      primitives.exp.symbol, primitives.exp.func.name),
      primitives.neg.symbol, primitives.neg.func.name),
      primitives.mul.symbol, primitives.mul.func.name),
      primitives.div.symbol, primitives.div.func.name),
      primitives.add.symbol, primitives.add.func.name)

    removeRedundantParenthesis(tmp.substring(1, tmp.length - 1))
  }

  def bracketsOk(string: String): Boolean = string.count(_ == '(') == string.count(_ == ')')

  /**
    * Parses a function-like string, e.g.: add(mul(4, x), 3) and
    * splits it into its name: "add" and arguments Seq("mul(4, x)", "3")
    */
  @throws[NoSuchElementException]("If function was not yet registered. (No definitions found)")
  @throws[Error]("If arguments count exceeds function's arity")
  private def parseFunction(string: String): Option[(String, Seq[String])] = {
    println(string)
    val split = string.split('(')
    if (split.length == 1) return None

    val name = split.head
    val arity = registered.getOrElse(name, throw new NoSuchElementException(
      s"Could not find definition for function '$name'. Have you forgot to register it?")).arguments.length

    val scope = findEnclosingScope(string.substring(name.length + 1, string.length - 1))
    val arguments = string.substring(name.length + 1 + scope._1, name.length + 1 + scope._2)


    var brackets = 0
    var commas = Seq[Int]()
    for ((char, index) <- arguments.zipWithIndex) {
      if (char == '(') brackets += 1
      else if (char == ')') brackets -= 1

      if (brackets == 0 && char == ',') commas = commas :+ index
    }

    if (commas.length > arity - 1) {
      println(string)
      throw new Error(s"$name's arity exceeded. Found ${commas.length + 1} arguments:  $arguments   OR    ${
        (commas
         .map(c => arguments.substring(0, c).split(",")(1)) :+ arguments.substring(commas.last)).mkString(" and ")
      } " +
        s"split at $commas")
    }
    commas = -1 +: commas :+ arguments.length

    val args = commas.zipWithIndex.drop(1).map(pos => arguments.substring(commas(pos._2 - 1) + 1, pos._1))

    Some(name, args)
  }

  /** Recursively parses function-like strings and forms a call tree */
  private def formTree(string: String): Function = parseFunction(string) match {
    case Some(parsed) => registered(parsed._1).copy.substitute(parsed._2.map(formTree): _*)
    case None => try {
      new Constant(string.toDouble)
    } catch {
      case _: Throwable => new Identity(string)
    }
  }

  /**
    * Creates new function from string.
    *
    * @example Function("f = 2*x + 3^(5 + 3 * y)^"
    * @param string String to be parsed
    * @return
    */
  @throws[IllegalArgumentException]("If opening and closing brackets do not match")
  def apply(string: String): Function = if (!bracketsOk(string))
    throw new IllegalArgumentException("Error parsing function: wrong opening and closing brackets count")
  else formTree(adjust(string.trim)).simplified
}

/**
  * Create new function from call tree. If name is not specified, name is assumed "f" and current
  * "f" function gets replaced with new one.
  *
  * @param name     Function name. Example: f for f(x) = 2*x + sin(x)
  * @param callTree Function body. Example: 2*x + sin(x) for f(x) = 2*x + sin(x).
  *                 Call tree represents what functions in which order are getting executed.
  *                 Outputs from deeper-lying functions bubbles up to the starting nodes.
  */

sealed abstract class Function(val leaves: Seq[Function]) {

  /**
    * Tree of applications of functions. Represents the order of applying the functions,
    * with their outputs directed from lower leaves into higher branches. Every branch is
    * a function, arguments of which are also functions.<br>
    * <br>
    * Leaves always contain either identity function or constant function.<br>
    * <br>
    * Implicits are advised to construct the function tree.
    *
    * @example
    * @param func   Function, applied to leaves' result.
    * @param leaves Lower level branches
    * @param arg    Name of function argument. This does not matter for higher branches, only
    *               for the end leaves. e.g. add(mull(x, y), z) is transformed into
    *               add(mull(id(x), id(y)), id(z)), and only three id(x/y/z) functions matter
    */


  /** Substitute function's argument for another function */
  def substitute(functions: Function*): Function = substitute(functions.zipWithIndex.map(f => (arguments(f._2), f._1)
  ).toMap)

  /** Substitute variables for functions (branches) */
  def substitute(namedArgs: Map[String, Function]): Function = this match {
    case id: Function.Identity => (if (namedArgs.contains(id.argument)) namedArgs(id.argument) else id).copy
    case c: Function.Constant => c.copy
    case branch: Function.Branch => new Function.Branch(branch.func, leaves.map(_.substitute(namedArgs)))
  }


  /**
    * Substitutes arguments for values. Applying occurs in lexicographical order of arguments, used in a function.<br/>
    * Returns new [[Function]] with substituted arguments as constants.<br/>
    * Returned function is simplified.<br/>
    *
    * @param args Argument values
    * @example
    * {{{
    * val f = Function("x + 3 * y") // function
    *
    * // Substituting
    * f(1)    // 1 + 3 * y
    * f(1, 2) // 1 + 3 * 2, simplifying -> 7
    * }}}
    * @note
    * In example above, '''to substitute only "y" use Map:'''
    * {{{
    * f(Map("y" -> 1)) // x + 3 * 1, simplifying -> x + 3
    * }}}
    * @return new [[Function]] with substituted arguments, simplified.
    */
  def apply(args: Double*): Function = apply(args.zipWithIndex.map(f => (arguments(f._2), f._1)).toMap)

  /**
    * Substitutes arguments for values. Applying with allows to select variables for substituting.<br/>
    * Returns new [[Function]] with substituted arguments as constants.<br/>
    * Returned function is simplified.<br/>
    *
    * @param namedArgs Map of named arguments: name -> value
    * @example
    * {{{
    * val f = Function("x + 3 * y") // function
    *
    * // Substituting
    * val partially       = f(Map("x" -> 1)) // 1 + 3 * y
    * val differently     = f(Map("y" -> 1)) // x + 3 * 1, simplifying -> x + 3
    * val fully = f(Map("x" -> 1, "y" -> 2)) // 1 + 3 * 2, simplifying -> 7
    * }}}
    * @return new [[Function]] with substituted arguments, simplified.
    */
  def apply(namedArgs: Map[String, Double]): Function = this match {
    case id: Function.Identity => if (namedArgs.contains(id.argument)) new Function.Constant(namedArgs(id.argument))
    else id.copy
    case c: Function.Constant => c.copy
    case branch: Function.Branch => new Function.Branch(branch.func, leaves.map(_ (namedArgs))).simplified
  }

  /**
    * Computes constant expressions, returning a simplified [[Function]] instance.<br/>
    * This converts branches([[Function.Branch]]), with leaves ''only'' of [[Function.Constant]] to
    * [[Function.Constant]] themselves.<br/>
    *
    * @note Simplification is done automatically on applying arguments.
    * @example
    * {{{
    * Function("(3 + 6) * x").simplified     // 9 * x
    * Function("(y + 6) * x")(Map("y" -> 3)) // 9 * x
    * }}}
    * @return new [[Function]] instance with computed constants.
    */
  def simplified: Function = {
    val s = this match {
      case br: Function.Branch => if (br.leaves.forall(_.isInstanceOf[Function.Constant]))
        new Function.Constant(br.func.body(br.leaves.map(_.asInstanceOf[Function.Constant].value.get)))
      else new Function.Branch(br.func, br.leaves.map(_.simplified))
      case _ => copy
    }

    if (callsCount == s.callsCount) s else s.simplified
  }

  /**
    * Used to compare functions in [[simplified]] function above
    *
    * @return number of function calls (disregarding [[Function.Identity]] and [[Function.Constant]] - only those in
    *         [[Function.Branch]])
    */
  private def callsCount: Int = this match {
    case br: Function.Branch => 1 + br.leaves.map(_.callsCount).sum
    case _ => 0
  }

  /**
    * Variable names, ordered lexicographically, used in a function as sequence of strings.
    *
    * @return ordered sequence of variable names as strings
    */
  def arguments: Seq[String] = _arguments.distinct.sorted
  private def _arguments: Seq[String] = this match {
    case id: Function.Identity => Seq(id.argument)
    case br: Function.Branch => br.leaves.flatMap(_._arguments)
    case _ => Seq()
  }

  /**
    * Returns a deep copy of an instance.
    *
    * @return new [[Function]] instance with identical call tree, name and variables
    */
  def copy: Function

  /**
    * Computed value as [[Option[Double]].<br/>
    * Is present only if function is '''constant''' (has no free variables).
    *
    * @example
    * {{{
    * Function("3 + 5").value // Some(7)
    * Function("x + 5").value // None - as function has free (unsubstituted) variable 'x'
    * }}}
    * @return
    */
  def value: Option[Double] = None

  /**
    * Returns function body as string.<br/>
    * This shows how more complex functions break down into [[Function.Primitive]].
    *
    * @example
    * {{{
    * println(Function(3*x*cos(y + x)))
    * > mul(mul(3,x),cos(add(y,x)))
    * }}}
    * @return function body as string
    */
  override implicit def toString: String = this match {
    case id: Function.Identity => id.argument
    case c: Function.Constant => c.value.get.toString
    case br: Function.Branch => s"${br.func.name}(${br.leaves.map(_.toString).mkString(", ")})"
  }

  /**
    * Returns true if is instance of [[Function.Constant]] function.
    *
    * @return `true` if is instance of [[Function.Constant]] else `false`
    */
  def isComputable: Boolean = isInstanceOf[Function.Constant]

  /**
    * Register function with name to further use in expressions.<br/>
    * This is the same as [[Function.register]]<br/>
    *
    * @example
    * {{{
    * val sqrt = Function("x `^` (1/2)") as "sqrt"
    * Function.register(Function("sqrt(x) * 2"), "f")
    * }}}
    * @param name Name for the function to be referenced later with.
    * @return this
    */
  def as(name: String): Function = {
    Function.register(this, name)
    this
  }
}
