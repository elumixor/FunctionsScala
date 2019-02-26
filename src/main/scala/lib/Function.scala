package lib

import java.util.NoSuchElementException

import lib.Function.{Branch, Simplifiable}

import scala.language.implicitConversions

// todo documentation

//@ScalaJSDefined
//@JSExport("TutorialLib")

//@js.native
//@JSImport("bar.js", "Foo")
//class JSFoo(val x: Int) extends js.Object

sealed abstract class Function {
  /**
    * Substitutes arguments for values. Applying occurs in lexicographical order of arguments, used in a function.<br/>
    * Returns new [[Function]] with substituted arguments as constants.<br/>
    * Returned function is simplified.<br/>
    *
    * @param args Argument values
    * @example
    * {{{
    * val f = lib.Function("x + 3 * y") // function
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
    * val f = lib.Function("x + 3 * y") // function
    *
    * // Substituting
    * val partially       = f(Map("x" -> 1)) // 1 + 3 * y
    * val differently     = f(Map("y" -> 1)) // x + 3 * 1, simplifying -> x + 3
    * val fully = f(Map("x" -> 1, "y" -> 2)) // 1 + 3 * 2, simplifying -> 7
    * }}}
    * @return new [[Function]] with substituted arguments, simplified.
    */
  def apply(namedArgs: Map[String, Double]): Function

  // todo doc
  /** Substitute function's argument for another function */
  def substitute(functions: Function*): Function = substitute(functions.zipWithIndex.map(f => (arguments(f._2), f._1)
  ).toMap)

  // todo doc
  /** Substitute variables for functions (branches) */
  def substitute(namedArgs: Map[String, Function]): Function

  /**
    * Computes constant expressions, returning a simplified [[Function]] instance.<br/>
    * This converts branches([[Function.Branch]]), with leaves ''only'' of [[Function.Constant]] to
    * [[Function.Constant]] themselves.<br/>
    *
    * @note Simplification is done automatically on applying arguments.
    * @example
    * {{{
    * lib.Function("(3 + 6) * x").simplified     // 9 * x
    * lib.Function("(y + 6) * x")(Map("y" -> 3)) // 9 * x
    * }}}
    * @return new [[Function]] instance with computed constants.
    */
  def simplified: Function = {
    def callsCount(of: Function): Int = of match {
      case br: Function.Branch => 1 + br.leaves.map(callsCount).sum
      case _ => 0
    }

    val s = this match {
      case br: Function.Branch =>
        (br.func match {
          case s: Simplifiable => s.simplify(br.leaves)
          case _ => br
        }) match {
          case branch: Function.Branch => if (branch.leaves.forall(_.isInstanceOf[Function.Constant]))
            new Function.Constant(branch.func.body(branch.leaves.map(_.asInstanceOf[Function.Constant].value.get)))
          else Function.Branch(branch.func, branch.leaves.map(_.simplified))
          case x => x
        }
      case _ => copy
    }

    if (callsCount(this) == callsCount(s)) s else s.simplified
  }

  /** */
  def derivative(power: Int): Function = {
    val args = arguments
    if (args.isEmpty) new Function.Constant(0) else derivative(args.head, power)
  }

  /** Partial derivative by argument */
  def derivative(argument: String, power: Int = 1): Function = {
    if (power == 0) copy
    else _derivative(argument).simplified.derivative(argument, power - 1)
  }

  protected def _derivative(argument: String): Function

  /**
    * Variable names, ordered lexicographically, used in a function as sequence of strings.
    *
    * @example
    * {{{
    * lib.Function("x * (3 + y)").arguments
    * > Seq("x", "y")
    * }}}
    * @return ordered sequence of variable names as strings
    */
  def arguments: Seq[String] = {
    def _arguments(of: Function): Seq[String] = of match {
      case id: Function.Identity => Seq(id.argument)
      case br: Function.Branch => br.leaves.flatMap(_arguments)
      case _ => Seq()
    }

    _arguments(this).distinct.sorted
  }

  /**
    * Computed value as [[Option[Double]].<br/>
    * Is present only if function is '''constant''' (has no free variables).
    *
    * @example
    * {{{
    * lib.Function("3 + 5").value // Some(7)
    * lib.Function("x + 5").value // None - as function has free (unsubstituted) variable 'x'
    * }}}
    * @return
    */
  def value: Option[Double] = None

  /**
    * Returns a deep copy of an instance.
    *
    * @return new [[Function]] instance with identical call tree, name and variables
    */
  def copy: Function

  /**
    * Returns function body as string.<br/>
    * This shows how more complex functions break down into [[Function.Primitive]].
    *
    * @example
    * {{{
    * println(lib.Function(3*x*cos(y + x)))
    * > mul(mul(3,x),cos(add(y,x)))
    * }}}
    * @return function body as string
    */
  override implicit def toString: String

  /**
    * Register function with name to further use in expressions.<br/>
    * This is the same as [[Function.register]]<br/>
    *
    * @example
    * {{{
    * val sqrt = lib.Function("x `^` (1/2)") as "sqrt"
    * lib.Function.register(lib.Function("sqrt(x) * 2"), "f")
    * }}}
    * @param name Name for the function to be referenced later with.
    * @return this
    */
  def as(name: String): Function = {
    Function.register(this, name)
    this
  }
}

/**
  * Factory for [[Function]] instances.
  * todo doс
  */
object Function {
  private abstract sealed case class Primitive(body: Seq[Double] => Double, name: String, arguments: Seq[String]) {
    def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch
  }
  private trait Simplifiable {
    this: Primitive =>

    def simplify(leaves: Seq[Function]): Function
  }

  private abstract sealed class Leaf extends Function

  private class Identity(val argument: String) extends Leaf {
    override def copy: Function = new Identity(argument)
    override def apply(namedArgs: Map[String, Double]): Function = if (namedArgs.contains(argument))
      new Function.Constant(namedArgs(argument))
    else copy
    override def substitute(namedArgs: Map[String, Function]): Function = if (namedArgs.contains(argument))
      namedArgs(argument).copy
    else copy
    override def _derivative(argument: String): Function = if (this.argument == argument) 1.0 else 0.0
    implicit override def toString: String = argument
  }

  private implicit class Constant(_value: Double) extends Leaf {
    override def copy: Function = _value
    override def value: Option[Double] = Some(_value)
    override def apply(namedArgs: Map[String, Double]): Function = copy
    override def substitute(namedArgs: Map[String, Function]): Function = copy
    override def _derivative(arguments: String): Function = 0.0
    implicit override def toString: String = _value.toString
  }

  private sealed case class Branch(func: Primitive, leaves: Seq[Function]) extends Function {
    override def copy: Function = Branch(func, leaves.map(_.copy))
    override def apply(namedArgs: Map[String, Double]): Function = Function.Branch(func, leaves.map(_ (namedArgs)))
      .simplified
    override def substitute(namedArgs: Map[String, Function]): Function = Function.Branch(func, leaves.map(_
      .substitute(namedArgs)))
    implicit override def toString: String = s"${func.name}(${leaves.map(_.toString).mkString(", ")})"
    override protected def _derivative(argument: String): Function = {
      val derived = leaves.map(_._derivative(argument))
      func.derivative(leaves, derived)
    }
  }
  //  private class Primitive(func: F, arguments: Seq[String]) extends Branch(func, arguments.map(a => new Identity(a)))

  /** Primitive predef functions */
  private object primitives {

    /** Primitive with a symbol */
    sealed trait Symbolic {
      val symbol: Char
    }

    object sin extends Primitive(args => Math.sin(args.head), "sin", Seq("x")) {
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(mul, Seq(Branch(cos, Seq(leaves.head)), derived.head))
      }
    }

    object cos extends Primitive(args => Math.cos(args.head), "cos", Seq("x")) {
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(mul, Seq(Branch(neg, Seq(Branch(sin, Seq(leaves.head)))), derived.head))
      }
    }

    object add extends Primitive(args => args.head + args(1), "add", Seq("x", "y"))
      with Symbolic with Simplifiable {
      override val symbol: Char = '+'
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(add, Seq(derived.head, derived(1)))
      }
      def simplify(leaves: Seq[Function]): Function = {
        // 0 + x = x
        leaves.head match {
          case c: Constant if c.value.get == 0 => return leaves(1)
          case _ =>
        }

        // x + 0 = x
        leaves(1) match {
          case c: Constant if c.value.get == 0 => return leaves.head
          case _ =>
        }

        // x + y = x + y
        Branch(this, leaves)
      }
    }

    object neg extends Primitive(args => -args.head, "neg", Seq("x"))
      with Symbolic with Simplifiable {
      override val symbol: Char = '-'
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(neg, Seq(derived.head))
      }
      def simplify(leaves: Seq[Function]): Function = {
        leaves.head match {
          case c: Constant if c.value.get == 0 => leaves.head
          case _ => Branch(this, leaves)
        }
      }
    }

    object mul extends Primitive(args => args.head * args(1), "mul", Seq("x", "y"))
      with Symbolic with Simplifiable {
      override val symbol: Char = '*'
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(add, Seq(Branch(mul, Seq(derived.head, leaves(1))), Branch(mul, Seq(leaves.head, derived(1)))))
      }
      def simplify(leaves: Seq[Function]): Function = {
        leaves.head match {
          case c: Constant =>
            // 0 * x = 0
            if (c.value.get == 0) return 0.0
            // 1 * x = x
            if (c.value.get == 1) return leaves(1)
          case _ =>
        }

        leaves(1) match {
          case c: Constant =>
            // x * 0 = 0
            if (c.value.get == 0) return 0.0
            // x * 1 = x
            if (c.value.get == 1) return leaves.head
          case _ =>
        }

        // x * y = x * y
        Branch(this, leaves)
      }
    }

    object div extends Primitive(args => args.head / args(1), "div", Seq("x", "y"))
      with Symbolic with Simplifiable {
      override val symbol: Char = '/'
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(div,
          Seq(Branch(add,
            Seq(Branch(mul, Seq(derived.head, leaves(1))),
              Branch(neg, Seq(Branch(mul, Seq(derived(1), leaves.head)))))),
            Branch(mul, Seq(leaves(1), leaves(1)))))
      }
      def simplify(leaves: Seq[Function]): Function = {
        leaves.head match {
          // 0 / x = 0
          case c: Constant if c.value.get == 0 => return 0.0
          // x / x = 1
          case id: Identity => leaves(1) match {
            case id2: Identity => if (id.argument == id2.argument) return 1.0
            case _ =>
          }
          case _ =>
        }

        // x / 1 = x
        leaves(1) match {
          case c: Constant if c.value.get == 1 => return leaves.head
          case _ =>
        }

        // x / y = x / y
        Branch(this, leaves)
      }
    }

    object log extends Primitive(args => Math.log10(args(1)) / Math.log10(args.head), "log", Seq("x", "y"))
      with Simplifiable {
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(div, Seq(
          Branch(add, Seq(
            Branch(div, Seq(Branch(mul, Seq(derived(1), Branch(log, Seq(10.0, leaves.head)))), leaves(1)))
            , Branch(neg, Seq(
              Branch(div, Seq(Branch(mul, Seq(derived.head, Branch(log, Seq(10.0, leaves(1))))), leaves(1)))
            )))),
          Branch(mul, Seq(Branch(log, Seq(10.0, leaves.head)), Branch(log, Seq(10.0, leaves.head))))
        ))
      }
      def simplify(leaves: Seq[Function]): Function = {
        leaves.head match {
          // log(0, x) = 0
          case c: Constant if c.value.get == 0 => return 0.0
          // log(x, x) = 1
          case id: Identity => leaves(1) match {
            case id2: Identity => if (id.argument == id2.argument) return 1.0
            case _ =>
          }
          case _ =>
        }

        // log(x, 1) = 0
        leaves(1) match {
          case c: Constant if c.value.get == 1.0 => return 0.0
          case _ =>
        }

        // log(x, y) = log(x, y)
        Branch(this, leaves)
      }
    }

    object exp extends Primitive(args => Math.pow(args.head, args(1)), "exp", Seq("x", "y"))
      with Symbolic with Simplifiable {
      override val symbol: Char = '^'
      def derivative(leaves: Seq[Function], derived: Seq[Function]): Branch = {
        Branch(mul, Seq(
          Branch(exp, Seq(leaves.head, Branch(add, Seq(leaves(1), Branch(neg, Seq(1.0)))))),
          Branch(add, Seq(
            Branch(mul, Seq(derived.head, leaves(1))),
            Branch(mul, Seq(leaves.head, Branch(mul, Seq(derived(1), Branch(log, Seq(10.0, leaves.head))))))
          ))
        ))

      }
      def simplify(leaves: Seq[Function]): Function = {
        leaves.head match {
          case c: Constant =>
            // 0 ^ x = 0
            if (c.value.get == 0) return 0.0
            // 1 ^ x = x
            if (c.value.get == 1) return leaves(1)
          case _ =>
        }


        leaves(1) match {
          case c: Constant =>
            // x ^ 1 = x
            if (c.value.get == 1) return leaves.head
            // x ^ 0 = 1
            if (c.value.get == 0) return 1.0
          case _ =>
        }

        // x ^ y = x ^ y
        Branch(this, leaves)
      }
    }

    val toSeq: Seq[Primitive] = Seq(neg, add, mul, div, exp, sin, cos)
    val symbolic: Seq[Primitive with Symbolic] = toSeq.flatMap { case s: Symbolic => Some(s) case _ => None }
  }

  /** Registered functions */
  private var registered: Map[String, Function] = primitives.toSeq.map(p => p.name -> new Branch(p, p.arguments.map
  (new Identity(_)))).toMap


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
      primitives.exp.symbol, primitives.exp.name),
      primitives.neg.symbol, primitives.neg.name),
      primitives.mul.symbol, primitives.mul.name),
      primitives.div.symbol, primitives.div.name),
      primitives.add.symbol, primitives.add.name)

    removeRedundantParenthesis(tmp.substring(1, tmp.length - 1))
  }

  /** Checks if the '''number''' of opening brackets is same as closing */
  private def bracketsMatch(string: String): Boolean = string.count(_ == '(') == string.count(_ == ')')

  /**
    * Extracts function name and arguments from string.
    * {{{
    * parseFunction("add(mul(4, x), 3)")
    * > Some("add", Seq("mul(4, x)", "3"))
    * }}}
    */
  @throws[NoSuchElementException]("If function was not yet registered. (No definitions found)")
  @throws[Error]("If arguments count exceeds function's arity")
  private def parseFunction(string: String): Option[(String, Seq[String])] = {
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

    if (commas.length > arity - 1)
      throw new Error(s"$name's arity exceeded. Found ${commas.length + 1} arguments:  $arguments split at $commas")

    commas = -1 +: commas :+ arguments.length

    val args = commas.zipWithIndex.drop(1).map(pos => arguments.substring(commas(pos._2 - 1) + 1, pos._1))

    Some(name, args)
  }

  /** Recursively parses function-like strings and forms a call tree */
  private def formTree(string: String): Function = parseFunction(string) match {
    case Some(parsed) => registered(parsed._1).copy.substitute(parsed._2.map(formTree): _*)
    case None => try {
      string.toDouble
    } catch {
      case _: Throwable => new Identity(string)
    }
  }

  /**
    * Register function with name to further use in expressions.<br/>
    *
    * @example
    * {{{
    * lib.Function.register(lib.Function("sqrt(x) * 2"), "f")
    * val sqrt = lib.Function("x `^` (1/2)") as "sqrt"
    * }}}
    * @param name Name for the function to be referenced later with.
    * @return this
    */
  def register(f: Function, name: String): Unit = registered = registered - name + (name -> f)

  // todo doc
  /**
    * Creates new function from string.
    *
    * @example lib.Function("2*x + 3`^`(5 + 3 * y)"
    * @param string String to be parsed
    * @return
    */
  @throws[IllegalArgumentException]("If opening and closing brackets do not match")
  def apply(string: String): Function = if (!bracketsMatch(string))
    throw new IllegalArgumentException("Error parsing function: wrong opening and closing brackets count")
  else formTree(adjust(string.trim)).simplified
}
