package rockstar

import scala.collection.mutable

package object ir {
	class Program(val blocks: BasicBlocks)

	case class Variable(names : Vector[String])

	private[ir] type CurrentDefines = mutable.HashMap[Variable, mutable.HashMap[BasicBlock, Value]]
	private[ir] type IncompletePhis = mutable.HashMap[BasicBlock, mutable.HashMap[Variable, Operations.Phi]]
	private[ir] type BasicBlocks = mutable.ArrayBuffer[BasicBlock]
	private[ir] type ValueList = mutable.LinkedHashSet[Value]

	sealed trait Value

	sealed trait Constant

	class NumberConstant(val value: BigDecimal) extends Value with Constant
	class StringConstant(val value: String) extends Value with Constant
	class BooleanConstant(val value: Boolean) extends Value with Constant
	class NullConstant(val value: Unit) extends Value with Constant
	class MysteriousConstant(val value: Unit) extends Value with Constant

	object NumberConstant {
		def apply(value: BigDecimal): NumberConstant = new NumberConstant(value)
		def unapply(arg: NumberConstant): Option[BigDecimal] = Some(arg.value)
	}
	object StringConstant {
		def apply(value: String): StringConstant = new StringConstant(value)
		def unapply(arg: StringConstant): Option[String] = Some(arg.value)
	}
	object BooleanConstant {
		def apply(value: Boolean): BooleanConstant = new BooleanConstant(value)
		def unapply(arg: BooleanConstant): Option[Boolean] = Some(arg.value)
	}
	object NullConstant {
		def apply(): NullConstant = new NullConstant(())
		def unapply(arg: BooleanConstant): Boolean = true
	}
	object MysteriousConstant {
		def apply(): MysteriousConstant = new MysteriousConstant(())
		def unapply(arg: MysteriousConstant): Boolean = true
	}

	class BasicBlock(val preds : BasicBlocks, val succs: BasicBlocks, var operations: ValueList) extends Value {}
	object BasicBlock {
		def apply(preds: BasicBlocks, succs: BasicBlocks, operations: ValueList): BasicBlock =
			new BasicBlock(preds, succs, operations)
	}

	object Operations {
		class Phi(val block: BasicBlock, val operands : mutable.MutableList[Value]) extends Value
		object Phi {
			def apply(block: BasicBlock, operands: mutable.MutableList[Value]): Phi = new Phi(block, operands)
			def unapply(arg: Phi): Option[(BasicBlock, mutable.MutableList[Value])] = Some(arg.block, arg.operands)
		}

		class Add(val lhs : Value, val rhs : Value) extends Value
		class Subtract(val lhs : Value, val rhs : Value) extends Value
		class Multiply(val lhs : Value, val rhs : Value) extends Value
		class Divide(val lhs : Value, val rhs : Value) extends Value
		class Greater(val lhs : Value, val rhs : Value) extends Value
		class Less(val lhs : Value, val rhs : Value) extends Value
		class GreaterEq(val lhs : Value, val rhs : Value) extends Value
		class LessEq(val lhs : Value, val rhs : Value) extends Value
		class Eq(val lhs : Value, val rhs : Value) extends Value    
		class Neq(val lhs : Value, val rhs : Value) extends Value
		class And(val lhs : Value, val rhs : Value) extends Value
		class Or(val lhs : Value, val rhs : Value) extends Value
		class Nor(val lhs : Value, val rhs : Value) extends Value

		object Add {
			def apply(lhs: Value, rhs: Value): Add = new Add(lhs, rhs)
			def unapply(arg: Add): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Subtract {
			def apply(lhs: Value, rhs: Value): Subtract = new Subtract(lhs, rhs)
			def unapply(arg: Subtract): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Multiply {
			def apply(lhs: Value, rhs: Value): Multiply = new Multiply(lhs, rhs)
			def unapply(arg: Multiply): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Divide {
			def apply(lhs: Value, rhs: Value): Divide = new Divide(lhs, rhs)
			def unapply(arg: Divide): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Greater {
			def apply(lhs: Value, rhs: Value): Greater = new Greater(lhs, rhs)
			def unapply(arg: Greater): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Less {
			def apply(lhs: Value, rhs: Value): Less = new Less(lhs, rhs)
			def unapply(arg: Less): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object GreaterEq {
			def apply(lhs: Value, rhs: Value): GreaterEq = new GreaterEq(lhs, rhs)
			def unapply(arg: GreaterEq): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object LessEq {
			def apply(lhs: Value, rhs: Value): LessEq = new LessEq(lhs, rhs)
			def unapply(arg: LessEq): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Eq {
			def apply(lhs: Value, rhs: Value): Eq = new Eq(lhs, rhs)
			def unapply(arg: Eq): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Neq {
			def apply(lhs: Value, rhs: Value): Neq = new Neq(lhs, rhs)
			def unapply(arg: Neq): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object And {
			def apply(lhs: Value, rhs: Value): And = new And(lhs, rhs)
			def unapply(arg: And): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Or {
			def apply(lhs: Value, rhs: Value): Or = new Or(lhs, rhs)
			def unapply(arg: Or): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}
		object Nor {
			def apply(lhs: Value, rhs: Value): Nor = new Nor(lhs, rhs)
			def unapply(arg: Nor): Option[(Value, Value)] = Some(arg.lhs, arg.rhs)
		}

		class Print(val value: Value) extends Value
		class GetLine() extends Value

		object Print { def apply(value: Value): Print = new Print(value) }
		object GetLine { def apply(): GetLine = new GetLine() }
	}
}
