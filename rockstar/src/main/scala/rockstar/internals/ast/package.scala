package rockstar.internals

package object ast {
	sealed case class SourcePosition(start: Int, end: Int) {
		def expand(that: SourcePosition): SourcePosition = {
			SourcePosition(start.min(that.start), end.max(that.end))
		}
	}

	sealed trait Node {
		val srcPos: SourcePosition
	}

	sealed case class None(srcPos: SourcePosition) extends TopLevel

	sealed trait Subexprable extends Node
	sealed trait TopLevel extends Node

	/////////////////////////////
	// Variables and Constants //
	/////////////////////////////

	sealed trait Value extends Node with Subexprable

	sealed trait Variable extends Value {
		val name: String
	}

	sealed case class CommonVariable(name: String, srcPos: SourcePosition) extends Variable
	sealed case class ProperVariable(name: String, srcPos: SourcePosition) extends Variable
	sealed case class Pronoun(name: String, srcPos: SourcePosition) extends Variable

	sealed trait Constant extends Value

	sealed case class MysteriousConstant(srcPos: SourcePosition) extends Constant
	sealed case class NullConstant(srcPos: SourcePosition) extends Constant
	sealed case class BooleanConstant(value: Boolean, srcPos: SourcePosition) extends Constant
	sealed case class NumberConstant(value: BigDecimal, srcPos: SourcePosition) extends Constant
	sealed case class StringConstant(value: String, srcPos: SourcePosition) extends Constant

	////////////////////
	// Control Follow //
	////////////////////

	sealed case class FunctionCall(function: Variable, args: Vector[Subexprable], srcPos: SourcePosition) extends Subexprable with TopLevel
	sealed case class Break(srcPos: SourcePosition) extends Node with TopLevel
	sealed case class Continue(srcPos: SourcePosition) extends Node with TopLevel
	sealed case class Return(value: Subexprable, srcPos: SourcePosition) extends Node with TopLevel

	/////////////////////
	// Math Operations //
	/////////////////////

	sealed trait BinaryOperator extends Subexprable {
		val left: Subexprable
		val right: Subexprable
	}

	sealed case class Addition(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class Subtraction(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class Multiplication(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class Division(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator

	sealed case class Greater(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class Less(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class GreaterEq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class LessEq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class Eq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class Neq(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator

	sealed case class And(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator
	sealed case class Or(left: Subexprable, right: Subexprable, srcPos: SourcePosition) extends BinaryOperator

	sealed trait UnaryOperator extends Node {
		val value: Node
	}

	sealed case class Not(value: Subexprable, srcPos: SourcePosition) extends UnaryOperator with Subexprable

	sealed case class Increment(value: Variable, srcPos: SourcePosition) extends TopLevel
	sealed case class Decrement(value: Variable, srcPos: SourcePosition) extends TopLevel

	///////////////////////
	// Variable Handling //
	///////////////////////

	sealed case class Print(value: Subexprable, srcPos: SourcePosition) extends TopLevel
	sealed case class GetLine(variable: Variable, srcPos: SourcePosition) extends TopLevel
	sealed case class Set(value: Subexprable, variable: Variable, srcPos: SourcePosition) extends TopLevel

	//////////////////////
	// Block Statements //
	//////////////////////

	sealed case class StatementList(srcPos: SourcePosition, statements: Vector[TopLevel]) extends Node

	sealed trait Block extends Node {
		val statements: StatementList
	}

	sealed case class IfStatement(condition: Subexprable,
	                              statements: StatementList,
	                              elseStatement: ElseStatement,
	                              srcPos: SourcePosition) extends Block with TopLevel
	sealed case class ElseStatement(statements: StatementList, srcPos: SourcePosition) extends Block
	sealed case class WhileStatement(condition: Subexprable, statements: StatementList, srcPos: SourcePosition) extends Block with TopLevel

	sealed case class FunctionStatement(name: Variable, variables: Vector[Variable], statements: StatementList, srcPos: SourcePosition) extends Block with TopLevel

	sealed case class Program(statements: StatementList, srcPos: SourcePosition) extends Block
}
