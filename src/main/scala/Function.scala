import java.util.NoSuchElementException

import scala.language.implicitConversions
import scala.util.matching.Regex

object Function {
  private val constName = "const"
  private val identityName = "id"

  private type Func = Seq[Double] => Double

  private def constantFunc(v: Double): Func = {
    _: Seq[Double] => v
  }

  private def identityFunc: Func = {
    v: Seq[Double] => v.head
  }

  /**
    * Function with checkable type. Allows to check if the function is single-argument function,
    * if it's constant or identity
    */
  implicit private class ComparableFunction(private val func: Func) {
    /** Checks if function is constant: x => C */
    def isConst: Boolean = isSingleArg && func(Seq(1.0)) == func(Seq(2.0))

    /** Checks if function is identity: x => x */
    def isIdentity: Boolean = isSingleArg && (func(Seq(1.0)) == 1.0)

    /** Checks if function requires more than one argument */
    def isSingleArg: Boolean = try {
      func(Seq(1.0))
      true
    } catch {
      case _: IndexOutOfBoundsException => false
    }
  }

  /** Forms constant branch any -> C */
  implicit private def toBranch(v: Double): Branch = new Branch(constantFunc(v), constName)

  /** Forms constant branch any -> C */
  implicit private def toBranch(v: Int): Branch = new Branch(constantFunc(v), constName)

  /** Forms identity or constant branch x -> x */
  implicit private def toBranch(v: String): Branch = {
    try {
      v.toDouble
    } catch {
      case _: Throwable => new Branch(identityFunc, identityName, Seq(), v)
    }
  }

  /** Class for implicit branch creation */
  implicit private class implicit_branch(val name: String) {
    def o(leaves: Branch*): Branch = {
      val tree = functions(name).get.callTree.copy
      val args = tree.getArgs
      tree.substituteFunction(leaves.zipWithIndex.map(el => (args(el._2), el._1)).toMap)
      tree
    }
  }

  /** Primitive predef functions */
  private object functions {

    /** Primitive with a symbol */
    trait Symbolic {
      val symbol: Char
    }

    object sin extends Function(new Branch(args => Math.sin(args.head), "sin", Seq("x")), "sin")

    object cos extends Function(new Branch(args => Math.cos(args.head), "cos", Seq("x")), "cos")

    object add extends Function(new Branch(args => args.head + args(1), "add", Seq("x", "y")), "add") with Symbolic {
      override val symbol: Char = '+'
    }

    object neg extends Function(new Branch(args => -args.head, "neg", Seq("x")), "neg") with Symbolic {
      override val symbol: Char = '-'
    }

    object mul extends Function(new Branch(args => args.head * args(1), "mul", Seq("x", "y")), "mul") with Symbolic {
      override val symbol: Char = '*'
    }

    object div extends Function(new Branch(args => args.head / args(1), "div", Seq("x", "y")), "div") with Symbolic {
      override val symbol: Char = '/'
    }

    object exp extends Function(new Branch(args => Math.pow(args.head, args(1)), "exp", Seq("x", "y")), "exp") with Symbolic {
      override val symbol: Char = '^'
    }

    /** Returns all symbolic functions */
    def symbolicFunctions: Seq[Function with Symbolic] = registered.filter(_.isInstanceOf[Symbolic])
      .map(_.asInstanceOf[Function with Symbolic])

    /** Registered functions */
    private var registered = Seq[Function](neg, add, mul, div, exp, sin, cos)

    /** Register new function or replace with new definition */
    def register(f: Function): Unit = {
      val previous = registered.zipWithIndex.find(func => func._1.name == f.name)
      if (previous.isDefined) {
        registered = registered.patch(previous.get._2, Seq(f), 1)
      }
      else {
        registered = registered :+ f
      }
    }

    /** Get function definition by name */
    def apply(functionName: String): Option[Function] = registered.find(_.name == functionName)
  }

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
  class Branch(private var func: Func, var name: String, var leaves: Seq[Branch] = Seq(), var arg: String = "") {
    /** Checks if the branch has sub-branches
      *
      * @return true - if has no sub-branches, else false
      */
    def isLeaf: Boolean = leaves.isEmpty

    def isConst: Boolean = name == "const"

    def isIdentity: Boolean = name == "id"

    /**
      * Computes the branch (branch should already have all variables substituted).<br>
      * Check if all variables are substituted for constants via `canCompute`
      */
    def compute(): Double = if (isLeaf) func(Seq()) else func(leaves.map(_.compute()))

    /** Substitutes variables for constant values */
    def substitute(namedArgs: Map[String, Double]): Unit = {
      if (isLeaf && func.isIdentity && namedArgs.contains(arg))
        func = constantFunc(namedArgs(arg))
      else leaves.foreach(_.substitute(namedArgs))
    }

    /** Substitute variables for functions (branches) */
    def substituteFunction(namedArgs: Map[String, Branch]): Unit = {
      if (isLeaf && func.isIdentity && namedArgs.contains(arg)) leaves = Seq(namedArgs(arg))
      else leaves.foreach(_.substituteFunction(namedArgs))

      leaves = leaves.map(l => if (l.func.isIdentity) if (l.isLeaf) l else l.leaves.head else l)
    }

    /** Checks if the tree can be computed (i.e. has all variables substituted for constants) */
    def canCompute: Boolean = if (isLeaf) func.isConst else leaves.map(_.canCompute).reduce(_ && _)

    /** Gets all variables, used in a tree */
    def getArgs: Seq[String] = if (isLeaf) Seq(arg) else leaves.map(x => x.getArgs).reduce(_ ++ _).filter(!_.isEmpty).distinct

    /** Computes constant expressions, thus simplifying the tree */
    def simplify(): Unit = {
      if (leaves.nonEmpty) {
        leaves.foreach(_.simplify())
        if (leaves.forall(x => x.isLeaf && x.func.isConst)) {
          func = constantFunc(func(leaves.map(_.func(Seq()))))
          leaves = Seq()
        }
      }
    }

    /**
      * Custom toString implementation
      *
      * @return Double value as string, if is computed (all variables substituted),
      *         else prints a function representation.
      * @example f(x, y) = add(x, neg(y))
      */
    override implicit def toString: String = {
      if (isLeaf)
        if (func.isConst) s"const(${func(Seq())})" else s"id($arg)"
      else name + "(" + leaves.map(_.toString).mkString(", ") + ")"
    }

    /** Returns a deep copy of a branch */
    def copy: Branch = new Branch(func, name, leaves.map(_.copy), arg)
  }

  /** Checks if symbol is an operand */
  private val isOperand = (char: Char) => functions.symbolicFunctions.map(f => f.symbol).contains(char)

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
    replace(string.substring(0, split(0).length) + addPlus + replacement + '(' + operand + ')' + string.substring(bounds._2, string.length),
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

    // Positions of closing brackets
    val closing = indexed.filter(_._1 == ')').map(_._2)

    // For each bracket wind enclosing scope
    val closingRemoved = openingRemoved.map(el => el + findEnclosingScope(string.substring(el + 1))._2)

    val toBeRemoved = openingRemoved ++ closingRemoved

    indexed.filter(x => !toBeRemoved.contains(x._2)).map(_._1).mkString("")
  }

  /** Transforms function string into easier-to-parse format */
  private def adjust(string: String): String = {
    // Replace symbols with function names
    val tmp = replace(replace(replace(replaceUnary(replace(
      s"(${string.filterNot(_.isWhitespace).trim})", // get rid of whitespaces
      functions.exp.symbol, functions.exp.name),
      functions.neg.symbol, functions.neg.name),
      functions.mul.symbol, functions.mul.name),
      functions.div.symbol, functions.div.name),
      functions.add.symbol, functions.add.name)

    removeRedundantParenthesis(tmp.substring(1, tmp.length - 1))
  }

  /**
    * Parses a function-like string, e.g.: add(mul(4, x), 3) and
    * splits it into its name: "add" and arguments Seq("mul(4, x)", "3")
    */
  @throws[NoSuchElementException]("If function was not yet registered. (No definitions found)")
  @throws[Error]("If arguments count exceeds function's arity")
  private def parseFunction(string: String): Option[(String, Seq[String])] = {
    //    println("Parsing " + string)

    val split = string.split('(')
    if (split.length == 1) {
      //      println("nothing more")
      return None
    }

    val name = split.head
    val arity = functions(name).getOrElse(
      throw new NoSuchElementException(
        s"Could not find definition for function '$name'. Have you forgot to register it?"))
      .args.length

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
      throw new Error(s"$name's arity exceeded. Found ${commas.length + 1} arguments: $arguments split at $commas")
    }
    commas = -1 +: commas :+ arguments.length

    val args = commas.zipWithIndex.drop(1).map(pos => arguments.substring(commas(pos._2 - 1) + 1, pos._1))

    Some(name, args)
  }

  /** Recursively parses function-like strings and forms a call tree */
  private def formTree(string: String): Branch = {
    parseFunction(string) match {
      case Some(parsed) => parsed._1 o (parsed._2.map(formTree): _*)
      case None => string
    }
  }

  /**
    * Creates new function from string.
    *
    * @example Function("f = 2*x + 3^(5 + 3 * y)^"
    * @param string String to be parsed
    * @return
    */
  def apply(string: String): Function = {
    val initParser = new Regex("""^\ *([^\ ]*)\ *=\ *(.*)\ *$""", "name", "expr")
    val groups = initParser.findAllIn(string)
    val name = groups.group("name")
    val body = adjust(groups.group("expr").trim())
    val callTree = formTree(body)
    val f = new Function(callTree, name)
    f
  }

  /** Registers new function, adding it to defined functions */
  def register(function: Function): Unit = functions.register(function)
}

import Function._

/**
  * Create new function from call tree. If name is not specified, name is assumed "f" and current
  * "f" function gets replaced with new one.
  *
  * @param name     Function name. Example: f for f(x) = 2*x + sin(x)
  * @param callTree Function body. Example: 2*x + sin(x) for f(x) = 2*x + sin(x).
  *                 Call tree represents what functions in which order are getting executed.
  *                 Outputs from deeper-lying functions bubbles up to the starting nodes.
  */
class Function(val callTree: Branch, val name: String = "f") {
  /**
    * Function arguments. Example: x for f(x) = 2*x + sin(x).
    * These are parsed automatically from function's call tree
    */
  def args: Seq[String] = callTree.getArgs

  /**
    * Computed value.
    * Contains a value if the function is not partial (all arguments are fixed)
    */
  var value: Option[Double] = None

  /**
    * Returns value as double
    *
    * @throws java.util.NoSuchElementException if the function is partial (missing arguments values)
    */
  implicit def toDouble: Double = value.get

  /**
    * Returns value as string
    *
    * @throws java.util.NoSuchElementException if the function is partial (missing arguments values)
    */
  override implicit def toString: String = {
    if (value.isDefined) value.get.toString
    else s"$name(${args.mkString(", ")}) = $body"
  }

  /**
    * Checks if the function can be computed to a Double (all arguments are present)
    *
    * @return True if all arguments are present
    */
  def isComputable: Boolean = value.isDefined

  /**
    * Tries to simplify the function, computing constant expressions
    *
    * @return
    */
  def simplify(): Unit = callTree.simplify()

  /** Function's body */
  def body: String = callTree.toString

  /** Substitute function's argument for another function */
  def substitute(functions: Map[String, Function]): Function = {
    callTree.substituteFunction(functions.map(x => (x._1, x._2.callTree)))
    this
  }

  /** Substitute function's argument for another function */
  def substitute(functions: Function*): Function = {
    if (functions.isEmpty || args.isEmpty) this
    else substitute(functions.zipWithIndex.map(f => (args(f._2), f._1)).toMap)
  }

  /**
    * Assigns values to function arguments. This always produces a new Function object.
    * <br>
    * Map contains pairs of (argument name -> substituted value).
    * This also tries to simplify the function, substituting constant values for specified arguments.
    * If all arguments are specified, then the function calculates a value.
    * <br>
    * This can be checked with isComputable of value.isDefined
    *
    * @return New function with substituted arguments
    */
  def apply(namedArgs: Map[String, Double]): Function = {
    if (namedArgs.isEmpty) this
    else {
      callTree.substitute(namedArgs)
      if (callTree.canCompute) value = Some(callTree.compute())
      else simplify()
      this
    }
  }

  /**
    * Assigns values to function arguments. This always produces a new Function object.
    * <br>
    * This also tries to simplify the function, substituting constant values for specified arguments.
    * If all arguments are specified, then the function calculates a value.
    * <br>
    * This can be checked with isComputable of value.isDefined
    *
    * @return New function with substituted arguments
    */
  def apply(arguments: Double*): Function = {
    if (arguments.isEmpty || args.isEmpty) this
    else apply(arguments.zipWithIndex.map(f => (args(f._2), f._1)).toMap)
  }

  /** Returns a copy of a function */
  def copy: Function = new Function(callTree.copy)

  callTree.substitute(Map())
  if (callTree.canCompute) value = Some(callTree.compute())
  else simplify()
}
