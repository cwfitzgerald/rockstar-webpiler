package rockstar.internals.interpreter

import rockstar.internals.ast

object types {
	sealed trait RuntimeValue
	sealed case class RuntimeMysterious() extends RuntimeValue
	sealed case class RuntimeNull() extends RuntimeValue
	sealed trait RuntimeContainerValue[T] { val value: T }
	sealed case class RuntimeBoolean(value: Boolean)
		extends RuntimeContainerValue[Boolean]
			with RuntimeValue
	sealed case class RuntimeNumber(value: Double)
		extends RuntimeContainerValue[Double]
			with RuntimeValue
	sealed case class RuntimeString(value: String)
		extends RuntimeContainerValue[String]
			with RuntimeValue
	sealed case class RuntimeFunction(value: ast.FunctionStatement)
		extends RuntimeContainerValue[ast.FunctionStatement]
			with RuntimeValue

	object exceptions {
		sealed trait ControlFlowException {}

		sealed case class BreakException() extends Exception() with ControlFlowException
		sealed case class ContinueException() extends Exception() with ControlFlowException
		sealed case class ReturnException() extends Exception() with ControlFlowException

		sealed case class FatalErrorException(msg: String) extends Exception(msg)
	}
}
