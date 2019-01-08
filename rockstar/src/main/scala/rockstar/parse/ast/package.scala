package rockstar.parse

package object ast {
	case class SourcePosition(start: Int, end: Int) {
		def expand(that: SourcePosition): SourcePosition = {
			SourcePosition(start.min(that.start), end.max(that.end))
		}
	}

	sealed trait Node {
		val srcPos: SourcePosition
	}

	case class None(srcPos: SourcePosition) extends TopLevel

	sealed trait Subexprable extends Node
	sealed trait TopLevel extends Node

	/////////////////////////////
	// Variables and Constants //
	/////////////////////////////

	sealed trait Value extends Node with Subexprable

	sealed trait Variable extends Value {
		val name: String
	}

	case class CommonVariable(name: String, srcPos: SourcePosition) extends Variable
	case class ProperVariable(name: String, srcPos: SourcePosition) extends Variable
	case class Pronoun(name: String, srcPos: SourcePosition) extends Variable

	sealed trait Constant extends Value

	case class MysteriousConstant(srcPos: SourcePosition) extends Constant
	case class NullConstant(srcPos: SourcePosition) extends Constant
	case class BooleanConstant(value: Boolean, srcPos: SourcePosition) extends Constant
	case class NumberConstant(value: BigDecimal, srcPos: SourcePosition) extends Constant
	case class StringConstant(value: String, srcPos: SourcePosition) extends Constant

	////////////////////
	// Control Follow //
	////////////////////

	case class FunctionCall(function: Variable, args: Vector[Subexprable], srcPos: SourcePosition) extends Subexprable with TopLevel
	case class Break(srcPos: SourcePosition) extends Node with TopLevel
	case class Continue(srcPos: SourcePosition) extends Node with TopLevel
	case class Return(value: Node, srcPos: SourcePosition) extends Node with TopLevel

	/////////////////////
	// Math Operations //
	/////////////////////

	sealed trait BinaryOperator extends Subexprable {
		val left: Subexprable
		val right: Subexprable
	}

	case class Addition(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Subtraction(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Multiplication(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Division(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator

	case class Greater(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Less(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class GreaterEq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class LessEq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Eq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Neq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator

	case class And(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Or(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	case class Nor(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator

	sealed trait UnaryOperator extends Node {
		val value: Node
	}

	case class Increment(value: Variable, srcPos: SourcePosition) extends TopLevel
	case class Decrement(value: Variable, srcPos: SourcePosition) extends TopLevel

	///////////////////////
	// Variable Handling //
	///////////////////////

	case class Print(value: Subexprable, srcPos: SourcePosition) extends TopLevel
	case class GetLine(variable: Variable, srcPos: SourcePosition) extends TopLevel
	case class Set(value: Subexprable, variable: Variable, srcPos: SourcePosition) extends TopLevel

	//////////////////////
	// Block Statements //
	//////////////////////

	case class StatementList(srcPos: SourcePosition, statements: Vector[TopLevel]) extends Node

	sealed trait Block extends TopLevel {
		val statements: StatementList
	}

	case class IfStatement(condition: Subexprable, statements: StatementList, elseStatements: Node, srcPos: SourcePosition) extends Block
	case class WhileStatement(condition: Subexprable, statements: StatementList, srcPos: SourcePosition) extends Block
	case class UntilStatement(condition: Subexprable, statements: StatementList, srcPos: SourcePosition) extends Block

	case class FunctionStatement(name: Variable, variables: Vector[Variable], statements: StatementList, srcPos: SourcePosition) extends Block

	case class Program(statements: StatementList, srcPos: SourcePosition) extends Block
}
