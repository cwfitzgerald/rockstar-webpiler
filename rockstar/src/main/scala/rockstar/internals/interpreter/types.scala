package rockstar.internals.interpreter

import rockstar.internals.ast

object types {
	sealed trait RuntimeValue {
		val typename: String
	}
	sealed case class RuntimeMysterious() extends RuntimeValue {
		val typename: String = "mysterious"
	}
	sealed case class RuntimeNull() extends RuntimeValue {
		val typename: String = "null"
	}
	sealed trait RuntimeContainerValue[T] { val value: T }
	sealed case class RuntimeBoolean(value: Boolean)
		extends RuntimeContainerValue[Boolean]
			with RuntimeValue {
		val typename: String = "boolean"
	}
	sealed case class RuntimeNumber(value: Double)
		extends RuntimeContainerValue[Double]
			with RuntimeValue {
		val typename: String = "number"
	}
	sealed case class RuntimeString(value: String)
		extends RuntimeContainerValue[String]
			with RuntimeValue {
		val typename: String = "string"
	}
	sealed case class RuntimeFunction(value: ast.FunctionStatement)
		extends RuntimeContainerValue[ast.FunctionStatement]
			with RuntimeValue {
		val typename: String = "function"
	}

	object exceptions {
		sealed trait ControlFlowException {}

		sealed case class BreakException() extends Exception() with ControlFlowException
		sealed case class ContinueException() extends Exception() with ControlFlowException
		sealed case class ReturnException() extends Exception() with ControlFlowException

		sealed case class FatalErrorException(msg: String) extends Exception(msg)
	}
}
