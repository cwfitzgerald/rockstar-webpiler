package rockstar.parse.ast

import rockstar.parse.util.lineBreaks
import rockstar.parse.{ast,util}

object PrettyPrinter {
	case class positionFormatting(breaks: lineBreaks, cn: util.charsNeeded)

	private def printAST(curNode: ast.Node, breaks: util.lineBreaks, cn: util.charsNeeded, indent: Int): Seq[String] = {
		val pos = curNode.srcPos

		implicit val pnl: positionFormatting = positionFormatting(breaks, cn)

		printASTImpl(curNode, indent)
	}

	// I'm not too proud of that implicit there, but ¯\_(ツ)_/¯
	private def printASTImpl(curNode: ast.Node, indent: Int)(implicit pnl: positionFormatting) : Seq[String] = {
		val pos = curNode.srcPos

		val posStartString = util.formatAsString(pnl.breaks, pnl.cn, pos.start)
		val posEndString = util.formatAsString(pnl.breaks, pnl.cn, pos.end)

		def printer(value: String, indent: Int = indent) = {
			Seq(s"$posStartString-$posEndString${(1 to indent).map(_ => " |").foldLeft("")(_ + _)} $value")
		}

		curNode match {
			case _: ast.None => printer("None")
			case n: ast.CommonVariable => printer(s"Variable: '${n.name}'")
			case n: ast.ProperVariable => printer(s"Variable: '${n.name}'")
			case n: ast.Pronoun => printer(s"Pronoun: '${n.name}'")
			case _: ast.MysteriousConstant => printer("MysteriousConstant")
			case _: ast.NullConstant => printer("NullConstant")
			case n: ast.BooleanConstant => printer(s"Constant: ${n.value}")
			case n: ast.NumberConstant => printer(s"Constant: ${n.value}")
			case n: ast.StringConstant => printer(s"Constant: \'${n.value}\'")
			case n: ast.FunctionCall =>
				printer(s"Function Call: ") ++
					printer("Name", indent + 1) ++
					printASTImpl(n.function, indent + 2) ++
					printer("Args", indent + 1) ++
					n.args.flatMap(x => printASTImpl(x, indent + 2))
			case _: ast.Break => printer("Break")
			case _: ast.Continue => printer("Continue")
			case n: ast.Return => printer("Return") ++ printASTImpl(n.value, indent + 1)
			case n: ast.Addition => printer("Addition") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Subtraction => printer("Subtraction") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Multiplication => printer("Multiplication") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Division => printer("Division") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Greater => printer("Greater") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Less => printer("Less") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.GreaterEq => printer("Greater or Equal") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.LessEq => printer("Lesser or Equal") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Eq => printer("Equal") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Neq => printer("Not Equal") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.And => printer("And") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Or => printer("Or") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Nor => printer("Nor") ++ printASTImpl(n.left, indent + 1) ++ printASTImpl(n.right, indent + 1)
			case n: ast.Increment => printer("Increment") ++ printASTImpl(n.value, indent + 1)
			case n: ast.Decrement => printer("Decrement") ++ printASTImpl(n.value, indent + 1)
			case n: ast.Print => printer("Print") ++ printASTImpl(n.value, indent + 1)
			case n: ast.GetLine => printer("GetLine") ++ printASTImpl(n.variable, indent + 1)
			case n: ast.Set => printer("Set") ++ printASTImpl(n.variable, indent + 1) ++ printASTImpl(n.value, indent + 1)
			case n: ast.StatementList => printer("Statement List") ++ n.statements.flatMap(x => printASTImpl(x, indent + 1))
			case n: ast.IfStatement =>
				printer("If") ++
					printer("Conditon", indent + 1) ++
					printASTImpl(n.condition, indent + 2) ++
					printASTImpl(n.statements, indent + 1) ++
					printer("Else") ++
					printASTImpl(n.elseStatements, indent + 1)
			case n: ast.WhileStatement =>
				printer("While") ++
					printer("Conditon", indent + 1) ++
					printASTImpl(n.condition, indent + 2) ++
					printASTImpl(n.statements, indent + 1)
			case n: ast.UntilStatement =>
				printer("Until") ++
					printer("Conditon", indent + 1) ++
					printASTImpl(n.condition, indent + 2) ++
					printASTImpl(n.statements, indent + 1)
			case n: ast.FunctionStatement =>
				printer("Function") ++
					printer("Name:", indent + 1) ++
					printASTImpl(n.name, indent + 2) ++
					printer("Variables", indent + 1) ++
					n.variables.flatMap(x => printASTImpl(x, indent + 2)) ++
					printASTImpl(n.statements, indent + 1)
			case n: ast.Program => printer("Program") ++ printASTImpl(n.statements, indent + 1)
		}
	}

	def apply(program: Program, breaks: util.lineBreaks, cn: util.charsNeeded, indent: Int = 0): Seq[String] = {
		printAST(program, breaks, cn, indent)
	}
}
