package rockstar.ir

import rockstar.ast

import scala.collection.mutable

class FromAst {
	private val currentDefines = new CurrentDefines()
	private val incompletePhis = new IncompletePhis()
	private val basicBlocks = new BasicBlocks()

	private var tmpNum = 0
	private def GetTempVariable(scope: Vector[String]): Variable = {
		val res = scope :+ s"_tmp$tmpNum"
		tmpNum += 1
		Variable(res)
	}
	
	private def GetVariableName(scope: Vector[String], name: String): Variable = {
		Variable(scope :+ name)
	} 

	private def NewBasicBlock(pred: BasicBlocks = new BasicBlocks(), succs: BasicBlocks = new BasicBlocks()): BasicBlock = {
		val block = BasicBlock(pred, succs, new ValueList())
		basicBlocks += block
		block
	}

	private def WriteVariable(variable: Variable, block: BasicBlock, value: Value): Variable = {
		currentDefines.getOrElseUpdate(variable, mutable.HashMap[BasicBlock, Value]()).update(block, value)
		if (!value.isInstanceOf[Constant]) {
			block.operations += value
		}
		variable
	}

	private def ReadVariable(variable: Variable, block: BasicBlock): Value = {
		currentDefines.get(variable) match {
			case Some(blockMap) => blockMap(block)
			case None => MysteriousConstant()
		}
	}

	private def WalkAstSubexpr(block: BasicBlock, scope: Vector[String], subexpr: ast.Subexprable): Variable = subexpr match {
		case ast.StringConstant(value, srcPos) => WriteVariable(GetTempVariable(scope), block, StringConstant(value))
		case ast.NumberConstant(value, srcPos) => WriteVariable(GetTempVariable(scope), block, NumberConstant(value))
		case ast.BooleanConstant(value, srcPos) => WriteVariable(GetTempVariable(scope), block, BooleanConstant(value))
		case ast.NullConstant(srcPos) => WriteVariable(GetTempVariable(scope), block, NullConstant())
		case ast.MysteriousConstant(srcPos) => WriteVariable(GetTempVariable(scope), block, MysteriousConstant())
		case ast.CommonVariable(name, srcPos) => GetVariableName(scope, name)
		case ast.ProperVariable(name, srcPos) => GetVariableName(scope, name)

		// Rewrite this madness with type classes
		case ast.Addition(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Add(lhsVal, rhsVal))

		case ast.Subtraction(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Subtract(lhsVal, rhsVal))

		case ast.Multiplication(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Multiply(lhsVal, rhsVal))

		case ast.Division(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Divide(lhsVal, rhsVal))

		case ast.Greater(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Greater(lhsVal, rhsVal))

		case ast.Less(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Less(lhsVal, rhsVal))

		case ast.GreaterEq(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.GreaterEq(lhsVal, rhsVal))

		case ast.LessEq(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.LessEq(lhsVal, rhsVal))

		case ast.Eq(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Eq(lhsVal, rhsVal))

		case ast.Neq(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Neq(lhsVal, rhsVal))

		case ast.And(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.And(lhsVal, rhsVal))

		case ast.Or(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Or(lhsVal, rhsVal))

		case ast.Nor(lhs, rhs, srcPos) =>
			val lhsVar = WalkAstSubexpr(block, scope, lhs)
			val rhsVar = WalkAstSubexpr(block, scope, rhs)
			val lhsVal = ReadVariable(lhsVar, block)
			val rhsVal = ReadVariable(rhsVar, block)

			WriteVariable(GetTempVariable(scope), block, Operations.Nor(lhsVal, rhsVal))
	}

	private def WalkAstImpl(block: BasicBlock, scope: Vector[String], statement: ast.Node): Unit = {
		statement.asInstanceOf[ast.StatementList].statements.foreach {
			case ast.Set(value, variable, srcPos) =>
				val subexprVariable = WalkAstSubexpr(block, scope, value)
				val subexprValue = ReadVariable(subexprVariable, block)
				WriteVariable(GetVariableName(scope, variable.name), block, subexprValue)
			case ast.Print(value, srcPos) =>
				val subexprVariable = WalkAstSubexpr(block, scope, value)
				val subexprValue = ReadVariable(subexprVariable, block)
				block.operations += Operations.Print(subexprValue)
			case ast.GetLine(variable, srcPos) =>
				val op = Operations.GetLine()
				WriteVariable(GetVariableName(scope, variable.name), block, op)
				block.operations += op
			case ast.Increment(variable, srcPos) =>
				val inputVar = ReadVariable(GetVariableName(scope, variable.name), block)
				val addend1 = WriteVariable(GetTempVariable(scope), block, inputVar)
				val addend2 = WriteVariable(GetTempVariable(scope), block, NumberConstant(1))

				val value = Operations.Add(ReadVariable(addend1, block), ReadVariable(addend2, block))
				WriteVariable(GetVariableName(scope, variable.name), block, value)
			case ast.Decrement(variable, srcPos) =>
				val inputVar = ReadVariable(GetVariableName(scope, variable.name), block)
				val addend1 = WriteVariable(GetTempVariable(scope), block, inputVar)
				val addend2 = WriteVariable(GetTempVariable(scope), block, NumberConstant(-1))

				val value = Operations.Add(ReadVariable(addend1, block), ReadVariable(addend2, block))
				WriteVariable(GetVariableName(scope, variable.name), block, value)
		}
	}


	private def WalkAst(head : ast.Program): Unit = {
		WalkAstImpl(NewBasicBlock(), Vector[String](), head.statements)
	}
}

object FromAst {
	def apply(program: ast.Program): Program = {
		val fa = new FromAst()
		fa.WalkAst(program)
		new Program(fa.basicBlocks)
	}
}

