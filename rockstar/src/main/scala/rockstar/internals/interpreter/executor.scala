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

	private object CompareType extends Enumeration {
		type CompareType = Value
		val GT, LT, GE, LE, EQ, NEQ = Value
	}
	import CompareType._

	private def compare(ct: CompareType, left: types.RuntimeValue, right: types.RuntimeValue): types.RuntimeBoolean = ???

	private def evaluateSubexpr(value: ast.Subexprable): types.RuntimeValue = ???

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
		else ifBlock.elseStatements foreach {
			e: ast.ElseStatement => statements(e)
		}
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
			case ast.Increment(variable, srcPos) =>
				assignment(variable.name, evaluateSubexpr(ast.Addition(variable, variable, srcPos)))
			case ast.Decrement(variable, srcPos) =>
				assignment(variable.name, evaluateSubexpr(ast.Subtraction(variable, variable, srcPos)))
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
