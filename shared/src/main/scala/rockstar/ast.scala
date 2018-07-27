package rockstar

object ast {
	case class SourcePosition(start: Int, end: Int) {
		def expand(that: SourcePosition): SourcePosition = {
			SourcePosition(start.min(that.start), end.max(that.end))
		}
	}

	sealed trait Node {
		val srcPos: SourcePosition
	}

	case class None(srcPos: SourcePosition) extends Node

	/////////////////////////////
	// Variables and Constants //
	/////////////////////////////

	sealed trait Value extends Node

	sealed trait Variable extends Value {
		val name: String
	}

	case class CommonVariable(prefix: String, name: String, srcPos: SourcePosition) extends Variable
	case class ProperVariable(name: String, srcPos: SourcePosition) extends Variable
	case class Pronoun(name: String, srcPos: SourcePosition) extends Variable

	sealed trait Constant extends Value

	case class Mysterious(srcPos: SourcePosition) extends Constant
	case class Null(srcPos: SourcePosition) extends Constant
	case class BooleanC(value: Boolean, srcPos: SourcePosition) extends Constant
	case class NumberC(value: BigDecimal, srcPos: SourcePosition) extends Constant
	case class StringC(value: String, srcPos: SourcePosition) extends Constant

	////////////////////
	// Control Follow //
	////////////////////

	case class FunctionCall(function: Variable, args: Vector[Value], srcPos: SourcePosition) extends Node
	case class Break(srcPos: SourcePosition) extends Node
	case class Continue(srcPos: SourcePosition) extends Node
	case class Return(value: Node, srcPos: SourcePosition) extends Node

	/////////////////////
	// Math Operations //
	/////////////////////

	sealed trait BinaryOperator extends Node {
		val left: Node
		val right: Node
	}

	case class Addition(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Subtraction(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Multiplication(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Division(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator

	case class Greater(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Less(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class GreaterEq(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class LessEq(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Eq(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Neq(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator

	case class And(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Or(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator
	case class Nor(left: Node, right: Node, srcPos: SourcePosition) extends BinaryOperator

	sealed trait UnaryOperator extends Node {
		val value: Node
	}

	case class Increment(value: Variable, srcPos: SourcePosition) extends UnaryOperator
	case class Decrement(value: Variable, srcPos: SourcePosition) extends UnaryOperator

	///////////////////////
	// Variable Handling //
	///////////////////////

	case class Print(value: Node, srcPos: SourcePosition) extends UnaryOperator
	case class GetLine(variable: Variable, srcPos: SourcePosition) extends Node
	case class Set(value: Node, variable: Variable, srcPos: SourcePosition) extends Node

	//////////////////////
	// Block Statements //
	//////////////////////

	case class StatementList(srcPos: SourcePosition, statements: Vector[Node]) extends Node

	sealed trait Block extends Node {
		val statements: Node
	}

	case class IfStatement(condition: Node, statements: Node, elseStatements: Node, srcPos: SourcePosition) extends Block
	case class WhileStatement(condition: Node, statements: Node, srcPos: SourcePosition) extends Block
	case class UntilStatement(condition: Node, statements: Node, srcPos: SourcePosition) extends Block

	case class FunctionStatement(name: Variable, variables: Vector[Variable], statements: StatementList, srcPos: SourcePosition) extends Block

	case class Program(statements: Node, srcPos: SourcePosition) extends Block
}