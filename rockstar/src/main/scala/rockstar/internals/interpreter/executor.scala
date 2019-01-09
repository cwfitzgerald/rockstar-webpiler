package rockstar.internals.interpreter

import rockstar.internals.ast

import scala.collection.mutable

class executor (private var input: String){
	private var offsetInput: Int = 0

	def output: String = outputBuilder.toString
	def errors: Array[rockstar.StackTrace] = errorBuilder.to[Array]

	// Output
	private val outputBuilder = new StringBuffer()
	private val errorBuilder= mutable.ArrayBuffer[rockstar.StackTrace]()

	// Internal data
	private val globalLookup = mutable.HashMap[String, types.RuntimeValue]()
	private val stack = mutable.ArrayBuffer[(ast.FunctionStatement, mutable.HashMap[String, types.RuntimeValue])]()

	private def addStackTrace(msg: String): Unit = {
		errorBuilder += rockstar.StackTrace(
			msg,
			rockstar.StackTraceComponent("<main>", 0) +:
				stack.reverse.map({ case (func, _) => rockstar.StackTraceComponent(func.name.name, func.srcPos.start) })
		)
	}
	private def lookup(key: String) = {
		if (stack.isEmpty)
			globalLookup.getOrElse(key, types.RuntimeMysterious())
		else
			stack.last._2.getOrElse(key, types.RuntimeMysterious())
	}
	private def assignment(key: String, value: types.RuntimeValue): Unit = {
		if (stack.isEmpty)
			globalLookup(key) = value
		else
			stack.last._2(key) = value
	}
	private def stringify(rv: types.RuntimeValue): String = rv match {
		case types.RuntimeMysterious() => "mysterious"
		case types.RuntimeNull() => "null"
		case types.RuntimeBoolean(value) => value.toString
		case types.RuntimeNumber(value) => value.toString
		case types.RuntimeString(value) => value
		case types.RuntimeFunction(value) => s"<function:${value.name}>"
	}
	private def getLine: types.RuntimeValue = {
		val ret = input.drop(offsetInput).takeWhile(_ != '\n')
		offsetInput = (offsetInput + ret.length + 1) min input.size
		types.RuntimeString(ret)
	}
	private def fatalError(msg: String): Nothing = {
		val fullMsg = s"FATAL ERROR: $msg\n"
		outputBuilder.append(fullMsg)
		addStackTrace(fullMsg)
		throw types.exceptions.FatalErrorException(fullMsg)
	}
	private def warning(msg: String): Unit = {
		val fullMsg = s"Warning: $msg\n"
		outputBuilder.append(fullMsg)
		addStackTrace(fullMsg)
	}

	private def truthyness(value: types.RuntimeValue): Boolean = value match {
		case types.RuntimeMysterious() => false
		case types.RuntimeNull() => false
		case types.RuntimeBoolean(b) => b
		case types.RuntimeNumber(n) => n == 0
		case types.RuntimeString(_) => true
		case types.RuntimeFunction(_) => true
	}

	private object CompareType extends Enumeration {
		type CompareType = Value
		val GT, LT, GE, LE, EQ, NEQ = Value
	}
	import CompareType.CompareType

	private def compare(ct: CompareType, left: types.RuntimeValue, right: types.RuntimeValue): types.RuntimeBoolean = ???

	private def evaluateSubexpr(value: ast.Subexprable): types.RuntimeValue = value match {
		case ast.And(left, right, _) =>
			val leftResult = truthyness(evaluateSubexpr(left))
			if (!leftResult) {
				types.RuntimeBoolean(true)
			}
			else {
				val rightResult = truthyness(evaluateSubexpr(right))
				types.RuntimeBoolean(rightResult)
			}
		case ast.Or(left, right, _) =>
			val leftResult = truthyness(evaluateSubexpr(left))
			if (leftResult) {
				types.RuntimeBoolean(true)
			}
			else {
				val rightResult = truthyness(evaluateSubexpr(right))
				types.RuntimeBoolean(rightResult)
			}
		case ast.Not(child, _) =>
			types.RuntimeBoolean(!truthyness(evaluateSubexpr(child)))
		case ast.Greater(left, right, _) =>
			compare(CompareType.GT, evaluateSubexpr(left), evaluateSubexpr(right))
		case ast.Less(left, right, _) =>
			compare(CompareType.LT, evaluateSubexpr(left), evaluateSubexpr(right))
		case ast.GreaterEq(left, right, _) =>
			compare(CompareType.GE, evaluateSubexpr(left), evaluateSubexpr(right))
		case ast.LessEq(left, right, _) =>
			compare(CompareType.LE, evaluateSubexpr(left), evaluateSubexpr(right))
		case ast.Eq(left, right, _) =>
			compare(CompareType.EQ, evaluateSubexpr(left), evaluateSubexpr(right))
		case ast.Neq(left, right, _) =>
			compare(CompareType.NEQ, evaluateSubexpr(left), evaluateSubexpr(right))
		case ast.Addition(left, right, _) =>
			val leftValue = evaluateSubexpr(left)
			val rightValue = evaluateSubexpr(right)

			leftValue match {
				case types.RuntimeNumber(num) => rightValue match {
					case types.RuntimeNumber(num2) => types.RuntimeNumber(num + num2)
					case v => fatalError(s"Cannot add ${v.typename} to a number")
				}
				case types.RuntimeString(str) => rightValue match {
					case types.RuntimeMysterious() => types.RuntimeString(str + "mysterious")
					case types.RuntimeNull() => types.RuntimeString(str + "null")
					case types.RuntimeBoolean(b) => types.RuntimeString(str + b.toString)
					case types.RuntimeNumber(n) => types.RuntimeString(str + n.toString)
					case types.RuntimeString(str2) => types.RuntimeString(str + str2)
					case types.RuntimeFunction(f) => types.RuntimeString(str + f.name.name)
				}
				case v => fatalError(s"Cannot add to a ${v.typename}.")
			}
		case ast.Subtraction(left, right, _) =>
			val leftValue = evaluateSubexpr(left)
			val rightValue = evaluateSubexpr(right)

			leftValue match {
				case types.RuntimeNumber(num) => rightValue match {
					case types.RuntimeNumber(num2) => types.RuntimeNumber(num + num2)
					case v => fatalError(s"Cannot subtract ${v.typename} from a number")
				}
				case v => fatalError(s"Cannot subtract from a ${v.typename}.")
			}
		case ast.Multiplication(left, right, _) =>
			val leftValue = evaluateSubexpr(left)
			val rightValue = evaluateSubexpr(right)

			leftValue match {
				case types.RuntimeNumber(num) => rightValue match {
					case types.RuntimeNumber(num2) => types.RuntimeNumber(num + num2)
					case types.RuntimeString(str) => types.RuntimeString(str * num.toInt)
					case v => fatalError(s"Cannot multiply a ${v.typename} with a number")
				}
				case types.RuntimeString(str) => rightValue match {
					case types.RuntimeNumber(num) => types.RuntimeString(str * num.toInt)
					case v => fatalError(s"Cannot multiply a ${v.typename} with  a string")
				}
				case v => fatalError(s"Cannot multiply a ${v.typename}.")
			}
		case ast.Division(left, right, _) =>
			val leftValue = evaluateSubexpr(left)
			val rightValue = evaluateSubexpr(right)

			leftValue match {
				case types.RuntimeNumber(num) => rightValue match {
					case types.RuntimeNumber(num2) =>
						if (num2 == 0)
							types.RuntimeNumber(num + num2)
						else
							fatalError("Cannot divide by 0")
					case v => fatalError(s"Cannot multiply a ${v.typename} with a number")
				}
				case v => fatalError(s"Cannot divide a ${v.typename}.")
			}
	}

	def increment(variable: ast.Variable, increment: Boolean): Unit = {
		val value = lookup(variable.name)
		assignment(variable.name, value match {
			case types.RuntimeMysterious() =>
				warning("Cannot increment a mysterious value")
				value
			case types.RuntimeNull() =>
				warning("Cannot increment a null value")
				value
			case types.RuntimeBoolean(b) =>
				types.RuntimeBoolean(!b)
			case types.RuntimeNumber(n) =>
				types.RuntimeNumber(if (increment){ n + 1 } else { n - 1 })
			case types.RuntimeString(s) =>
				warning("Cannot increment a string")
				value
			case types.RuntimeFunction(_) =>
				warning("Cannot increment a function")
				value
		})
	}

	private def print(rv: types.RuntimeValue): Unit = outputBuilder.append(stringify(rv))

	private def function(name: String, args: Vector[types.RuntimeValue]): types.RuntimeValue = {
		lookup(name) match {
			case types.RuntimeFunction(function) =>
				stack.append((function, mutable.HashMap[String, types.RuntimeValue]()))
				val paramCount = function.variables.size
				val mysterioused = args ++ Array.fill(0 min (args.size - paramCount))(types.RuntimeMysterious())
				val clipped = mysterioused.take(paramCount)

				function.variables.map(_.name).zip(clipped).foreach((assignment _).tupled)

				try {
					statements(function)
				} catch {
					case _: types.exceptions.ReturnException =>
				}
				val retval = lookup("$retval")
				stack.remove(stack.size - 1)

				retval
			case _ => types.RuntimeMysterious()
		}
	}

	private def ifStatement(ifBlock: ast.IfStatement): Unit = {
		if (compare(CompareType.EQ, evaluateSubexpr(ifBlock.condition), types.RuntimeBoolean(true)).value)
			statements(ifBlock)
		else
			statements(ifBlock.elseStatement)
	}

	private def whileStatement(whileBlock: ast.WhileStatement): Unit = {
		try {
			while (compare(CompareType.EQ, evaluateSubexpr(whileBlock.condition), types.RuntimeBoolean(true)).value)
				try {
					statements(whileBlock)
				} catch {
					case _: types.exceptions.ContinueException =>
				}
		} catch {
			case _: types.exceptions.BreakException =>
		}
	}

	private def statements(block: ast.Block): Unit = {
		block.statements.statements.foreach {
			case ast.Print(value, _) =>
				print(evaluateSubexpr(value))
			case ast.FunctionCall(ast, args, _) =>
				function(ast.name, args.map(evaluateSubexpr))
			case ast.GetLine(variable, _) =>
				assignment(variable.name, getLine)
			case ast.Set(value, variable, _) =>
				assignment(variable.name, evaluateSubexpr(value))
			case ast.Increment(variable, _) =>
				increment(variable, increment = true)
			case ast.Decrement(variable, _) =>
				increment(variable, increment = false)
			case ast.Continue(_) => block match {
				case _: ast.IfStatement =>
				case _: ast.FunctionStatement =>
				case _ => throw types.exceptions.ContinueException()
			}
			case ast.Break(_) => block match {
				case _: ast.IfStatement =>
				case _: ast.FunctionStatement =>
				case _ => throw types.exceptions.BreakException()
			}
			case ast.Return(value, _) =>
				assignment("$retval", evaluateSubexpr(value))
				throw types.exceptions.ReturnException()
			case ifBlock: ast.IfStatement =>
				ifStatement(ifBlock)
			case whileBlock: ast.WhileStatement =>
				whileStatement(whileBlock)
			case functionBlock @ ast.FunctionStatement(name, _, _, _) =>
				assignment(name.name, types.RuntimeFunction(functionBlock))
			case _: ast.None =>
				fatalError("internal compiler error: evaluating ast.None")
		}
	}

	def addInput(in: String): Unit = {
		input += in
	}

	def run(curAST: ast.Program): Unit = {
		statements(curAST)
	}

	def apply(curASTs: ast.Program*): Unit = {
		curASTs.foreach(run)
	}
}
object executor {
	def apply(asts: ast.Program*): executor = {
		val e = new executor("")
		e(asts :_*)
		e
	}
}
