package rockstar

import fastparse.all
import fastparse.all._
import rockstar.ast.{SourcePosition => srcPos}

import scala.annotation.tailrec

object parser {
	/////////////////////
	// Core Primitives //
	/////////////////////

	private val CI = IgnoreCase

	private def I[T](p: P[T]): all.Parser[(Int, T, Int)] = P( Index ~ p ~ Index )

	private val W = P( (CharsWhileIn(Seq(' ', '\t', ','), 1) | commentStatement).rep )/*.log()*/
	private val MW = P( (CharsWhileIn(Seq(' ', '\t', ','), 1) | commentStatement).rep(1) )/*.log()*/
	private val nonCommaWhitespace = P( (CharsWhileIn(Seq(' ', '\t'), 1) | commentStatement).rep )
	private val newLine = P( "\n" | "\r\n" )/*.log()*/.opaque("Newline")
	private val lineEnd = P( newLine | End )/*.log()*/.opaque("End of Line")

	private val capitalLetter = P( CharIn('A' to 'Z') )
	private val lowerLetter = P( CharIn('a' to 'z') )
	private val number = P( CharIn('0' to '9') )

	private val space = P( CharIn(Seq(' ')) )
	private val fullLetter = P( capitalLetter | lowerLetter )
	private val word = P( CharsWhile(c => !c.isWhitespace && (c != '(' && c != '.')) )/*.log()*/.opaque("Word")

	private val commonVariableVariableName = P( lowerLetter.rep(1) )/*.log()*/
	private val commonVariableKeywords = P( StringInIgnoreCase("a", "an", "the", "my", "your") )/*.log()*/
	private val commonVariable = I( commonVariableKeywords.! ~ MW ~ commonVariableVariableName.! )/*.log*/
    	.map {case (si, (k, n), ei) => ast.CommonVariable(k, n, srcPos(si, ei)) }

	private val properVariableName = I( (capitalLetter ~ fullLetter.rep ~ (space ~ capitalLetter ~ fullLetter.rep).rep).! )/*.log()*/
    	.map {case (si, n, ei) => ast.ProperVariable(n, srcPos(si, ei)) }

	private val pronoun = P( Index ~ StringInIgnoreCase("it", "he", "she", "him", "her", "them", "they").! ~ Index )/*.log()*/
    	.map {case (si, n, ei) => ast.Pronoun(n, srcPos(si, ei)) }

	private val variable: all.Parser[ast.Variable] = P( commonVariable | pronoun | properVariableName )/*.log()*/
    	.opaque("Variable")

	// all the literals in the language
	private val tMysterious = I( CI("mysterious") )/*.log()*/
		.map { case (si, _, ei) => ast.Mysterious(srcPos(si, ei)) }

	private val tNull = I( StringInIgnoreCase("null", "nothing", "nowhere", "nobody", "gone", "empty" ) )/*.log()*/
    	.map { case (si, _, ei) => ast.Null(srcPos(si, ei)) }

	private val tBooleanTrue = I( StringInIgnoreCase("true", "right", "yes", "ok") )/*.log()*/
    	.map { case (si, _, ei) => ast.BooleanC(value = true, srcPos(si, ei)) }

	private val tBooleanFalse = I( StringInIgnoreCase("false", "wrong", "no", "lines") )/*.log()*/
		.map { case (si, _, ei) => ast.BooleanC(value = false, srcPos(si, ei)) }

	private val tNumber = I( ("-".?.! ~ W ~ number.rep(1).!) ~ ("." ~ number.rep).!.? )/*.log()*/
		.map { case (si, (sign, front, back), ei) => ast.NumberC(BigDecimal(sign + front + back.getOrElse(".0")), srcPos(si, ei)) }

	private val tString = I( "\"" ~/ (!CharIn(Seq('\"', '\n')) ~/ AnyChar).rep.! ~ "\"" )/*.log()*/
    	.opaque("String")
    	.map { case (si, v, ei) => ast.StringC(v, srcPos(si, ei)) }

	private val tLiteral: all.Parser[ast.Constant] =
		P( tMysterious | tNull | tBooleanTrue | tBooleanFalse | tNumber | tString )/*.log()*/


	///////////////////////////
	// Grammar / Expressions //
	///////////////////////////

	// Allows better modularization of .map functions by distinguishing them early
	private sealed trait OperatorEnumerator

	// arithmetic
	private sealed trait plusMinusEnumeratorClass extends OperatorEnumerator
	private case class plusEnumerator() extends plusMinusEnumeratorClass
	private case class minusEnumerator() extends plusMinusEnumeratorClass
	private sealed trait mulDivEnumeratorClass extends OperatorEnumerator
	private case class mulEnumerator() extends mulDivEnumeratorClass
	private case class divEnumerator() extends mulDivEnumeratorClass

	private val mulDivOperator = P( StringInIgnoreCase("times", "of", "over").! )
    	.map {
		    case "times" => mulEnumerator()
		    case "of" => mulEnumerator()
		    case "over" => divEnumerator()
	    }

	private val plusMinusOperator = P( StringInIgnoreCase("plus", "with", "minus", "without").! )
		.map {
			case "plus" => plusEnumerator()
			case "with" => plusEnumerator()
			case "minus" => minusEnumerator()
			case "without" => minusEnumerator()
		}

	// comparison operators
	private sealed trait DiffEnumerator extends OperatorEnumerator
	private case class gtEnumerator() extends DiffEnumerator
	private case class ltEnumerator() extends DiffEnumerator
	private case class geEnumerator() extends DiffEnumerator
	private case class leEnumerator() extends DiffEnumerator

	private sealed trait compEqEnumerator extends OperatorEnumerator
	private case class eqEnumerator() extends compEqEnumerator
	private case class neqEnumerator() extends compEqEnumerator

	private val greaterThanOperator = P( StringInIgnoreCase("higher", "greater", "bigger", "stronger") )
		.map {_ => gtEnumerator()}
	private val lessThanOperator = P( StringInIgnoreCase("lower", "less", "smaller", "weaker") )
		.map {_ => ltEnumerator()}
	private val greaterThanEqOperator = P( StringInIgnoreCase("high", "great", "big", "strong") )
		.map {_ => geEnumerator()}
	private val lessThanEqOperator = P( StringInIgnoreCase("low", "little", "small", "weak") )
		.map {_ => leEnumerator()}
	private val eqOperator = P( StringInIgnoreCase("is") )
		.map {_ => eqEnumerator()}
	private val neqOperator = P( StringInIgnoreCase("ain't", "aint") )
		.map {_ => neqEnumerator()}
	private val negationOperator = P( StringInIgnoreCase("not") )

	// compound comparison operators
	private val diffNeqOperator = P( CI("is") ~ W ~ (greaterThanOperator | lessThanOperator) ~ W ~/ CI("than") )
	private val diffEqOperator = P( CI("is") ~ W ~ CI("as") ~ W ~/ (greaterThanEqOperator | lessThanEqOperator) ~ W ~/ CI("as") )
	private val diffOperator = P( diffEqOperator | diffNeqOperator )/*.log()*/

	private val compEqOperator = P( (eqOperator ~ negationOperator.!.?) | neqOperator )
    	.map {
		    case (eqEnumerator(), None) => eqEnumerator()
		    case (eqEnumerator(), Some(_)) => neqEnumerator()
		    case neqEnumerator() => neqEnumerator()
	    }

	// boolean operators
	private sealed trait BooleanEnumerator extends OperatorEnumerator
	private case class andEnumerator() extends BooleanEnumerator
	private case class orEnumerator() extends BooleanEnumerator
	private case class norEnumerator() extends BooleanEnumerator
	private val booleanOperator = P( StringInIgnoreCase("and", "or", "nor").! )
    	.map {
		    case "and" => andEnumerator()
		    case "or" => orEnumerator()
		    case "nor" => norEnumerator()
	    }

	// represents anything that can give you a value
	private val valueProvider: all.Parser[ast.Value] = P( tLiteral | variable )/*.log()*/

	/////////////////////////
	// Order of Operations //
	/////////////////////////

	private val functionArgumentDelim = P( StringInIgnoreCase(",", "and") )
	private val functionCall =
		I(
			(variable ~ (W ~ CI("taking") ~ W ~/ valueProvider ~ (nonCommaWhitespace ~ functionArgumentDelim ~ W ~/ valueProvider).rep ).? )
			| tLiteral
		)/*.log()*/
    	.map{
			case (si, v, ei) => v match {
				case lit: ast.Constant => lit
				case (variable: ast.Variable, None) => variable
				case (funcName: ast.Variable, Some((firstArg: ast.Value, nArgs: Object))) =>
					val nArgsCast = nArgs.asInstanceOf[Seq[ast.Value]]

					ast.FunctionCall(funcName, (firstArg +: nArgsCast).toVector, srcPos(si, ei))
			}
		}

	@tailrec
	private def mulDivOperationGen(lhs: ast.Node, rhs: List[(mulDivEnumeratorClass, ast.Node)]): ast.Node = rhs match {
		case Nil => lhs
		case (mulEnumerator(), node) :: rest =>
			mulDivOperationGen(ast.Multiplication(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (divEnumerator(), node) :: rest =>
			mulDivOperationGen(ast.Division(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private val mulDivOperation =
		P( functionCall ~
			(W ~ mulDivOperator ~ W ~/ functionCall).rep
		)/*.log()*/
    	.map {
			case (lhs, rhs) => mulDivOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def plusMinusOperationGen(lhs: ast.Node, rhs: List[(plusMinusEnumeratorClass, ast.Node)]): ast.Node = rhs match {
		case Nil => lhs
		case (plusEnumerator(), node) :: rest =>
			plusMinusOperationGen(ast.Addition(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (minusEnumerator(), node) :: rest =>
			plusMinusOperationGen(ast.Subtraction(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private val plusMinusOperation =
		P( mulDivOperation ~
			(W ~ plusMinusOperator ~ W ~/ mulDivOperation).rep
		)/*.log()*/
		.map {
			case (lhs, rhs) => plusMinusOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def diffComparisonOperationGen(lhs: ast.Node, rhs: List[(DiffEnumerator, ast.Node)]): ast.Node = rhs match {
		case Nil => lhs
		case (gtEnumerator(), node) :: rest =>
			diffComparisonOperationGen(ast.Greater(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (ltEnumerator(), node) :: rest =>
			diffComparisonOperationGen(ast.Less(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (geEnumerator(), node) :: rest =>
			diffComparisonOperationGen(ast.GreaterEq(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (leEnumerator(), node) :: rest =>
			diffComparisonOperationGen(ast.LessEq(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private val diffComparisonOperation =
		P( plusMinusOperation ~
			(W ~ diffOperator ~ W ~/ plusMinusOperation).rep
		)/*.log()*/
    	.map {
			case (lhs, rhs) => diffComparisonOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def compEqComparisonOperationGen(lhs: ast.Node, rhs: List[(compEqEnumerator, ast.Node)]): ast.Node = rhs match {
		case Nil => lhs
		case (eqEnumerator(), node) :: rest =>
			compEqComparisonOperationGen(ast.Eq(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (neqEnumerator(), node) :: rest =>
			compEqComparisonOperationGen(ast.Neq(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private val compEqComparisonOperation =
		P( diffComparisonOperation ~
			(W ~ compEqOperator ~ W ~/ diffComparisonOperation).rep
		)/*.log()*/
    	.map {
			case (lhs, rhs) => compEqComparisonOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def boolOperationGen(lhs: ast.Node, rhs: List[(BooleanEnumerator, ast.Node)]): ast.Node = rhs match {
		case Nil => lhs
		case (andEnumerator(), node) :: rest =>
			boolOperationGen(ast.And(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (orEnumerator(), node) :: rest =>
			boolOperationGen(ast.Or(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (norEnumerator(), node) :: rest =>
			boolOperationGen(ast.Nor(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private val booleanOperation =
		P( compEqComparisonOperation ~
			(W ~ booleanOperator ~ W ~/ compEqComparisonOperation).rep
		)/*.log()*/
    	.map {
			case (lhs, rhs) => boolOperationGen(lhs, rhs.toList)
		}

	private val mathExpr = P( booleanOperation )/*.log()*/
    	.opaque("Math Expression")

	///////////////////////////
	// Standalone statements //
	///////////////////////////

	private val incrementStatement = I( CI("build") ~ W ~/ variable ~ W ~ CI("up") )/*.log()*/
    	.map { case (si, v, ei) => ast.Increment(v, srcPos(si, ei)) }
	private val decrementStatement = I( CI("knock") ~ W ~/ variable ~ W ~ CI("down") )/*.log()*/
		.map { case (si, v, ei) => ast.Decrement(v, srcPos(si, ei)) }

	private val breakStatement = I( CI("break") ~/ (W ~ CI("it") ~ W ~/ CI("down")).? ).opaque("Break Statement")
    	.map { case (si, _, ei) => ast.Break(srcPos(si, ei)) }
	private val continueStatement = I( CI("continue") | (CI("take") ~ W ~/ CI("it") ~ W ~/ CI("to") ~ W ~/ CI("the") ~ W ~/ CI("top")) ).opaque("Continue Statement")
		.map { case (si, _, ei) => ast.Continue(srcPos(si, ei)) }
	private val putStatement = I( CI("put") ~ W ~/ mathExpr ~ W ~ CI("into") ~ W ~/ variable )/*.log()*/
    	.map { case (si, (value, vari), ei) => ast.Set(value, vari, srcPos(si, ei)) }
	private val listenStatement = I( CI("listen") ~ W ~/ CI("to") ~ W ~/ variable )/*.log()*/
    	.map { case (si, vari, ei) => ast.GetLine(vari, srcPos(si, ei)) }
	private val sayCommands = P( StringInIgnoreCase("say", "shout", "whisper", "scream") )
	private val sayStatement = I( sayCommands ~ W ~/ mathExpr )/*.log()*/
    	.map { case (si, expr, ei) => ast.Print(expr, srcPos(si, ei)) }
	private val giveBackStatement = I( CI("give") ~ W ~/ CI("back") ~ W ~/ mathExpr )/*.log()*/
    	.map { case (si, expr, ei) => ast.Return(expr, srcPos(si, ei)) }

	private def resolvePoeticLiteral(string: Seq[String]) = {
		string.map(word => (word.filter( c => c.isLetter ).length % 10).toString).foldLeft(""){_ + _}
	}

	private val variableAssignmentOperator = P( StringInIgnoreCase("is", "was", "were", "'s") )/*.log()*/
    	.opaque("Variable Assignment Operator")
	private val variablePoeticTypeLiteral = P( tBooleanTrue | tBooleanFalse | tNull | tMysterious )/*.log()*/
    	.opaque("Poetic Type Literal")
	private val variablePoeticNumberLiteral = I( (word.! ~ W).rep(1) ~/ ("." ~ (W ~ word.!).rep).? ~ &(lineEnd) )/*.log()*/
    	.opaque("Poetic Number Literal")
    	.map {
			case (si, (wordLeftSeq, wordRightSeq), ei) =>
				ast.NumberC(
					BigDecimal(resolvePoeticLiteral(wordLeftSeq) + "." + resolvePoeticLiteral(wordRightSeq.getOrElse(Seq("abcdefghij")))),
					srcPos(si, ei)
				)
		}
	private val variableTypeAssignment =
		I(
			variable ~ W ~ variableAssignmentOperator ~ W ~/
				((tNull | tBooleanFalse | tBooleanTrue) | variablePoeticTypeLiteral | variablePoeticNumberLiteral)
		) /*.log()*/
		.map {
			case (si, (vari, value), ei) => ast.Set(value, vari, srcPos(si, ei))
		}
	private val variableStrLiteralAssignment =
		I(
			variable ~ W ~ CI("says") ~ W ~/ I((!newLine ~ AnyChar).rep.!)
		) /*.log()*/
		.map {
			case (si, (vari, (strSI, value, strEI)), ei) =>
				ast.Set(ast.StringC(value, srcPos(strSI, strEI)), vari, srcPos(si, ei))
		}

	private val variableAssignmentStatement = P( variableTypeAssignment | variableStrLiteralAssignment )/*.log()*/

	private val ifConditionExpression = P( CI("if") ~ W ~/ mathExpr )/*.log()*/
	private val elseConditionExpression = P( CI("else") ~/ W ) /*.log()*/
	private val whileConditionExpression = P( CI("while") ~ W ~/ mathExpr )/*.log()*/
	private val untilConditionExpression = P( CI("until") ~ W ~/ mathExpr )/*.log()*/
	private val functionSignatureExpression = P( variable ~ W ~ CI("takes") ~ W ~/ variable ~ (W ~ CI("and")/*.log()*/ ~ W ~ variable).rep )/*.log()*/
    	.map {
			case (funcVar, arg1, argN) => (funcVar, arg1 +: argN)
		}


	private def blockGenerator[T](parser: P[T]): P[(Int, (T, Seq[ast.Node]), Int)] = {
		I(
			parser ~ W ~ newLine ~
				(!(W ~ lineEnd) ~/ expression).rep()
		)
	}

	private val elseStatement = blockGenerator(elseConditionExpression)
		.map {
			case (si, (_, statements), ei) =>
				ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
		}/*.log()*/

	private val ifStatement =
		I(
			ifConditionExpression ~ W ~ newLine ~
				(!(W ~ lineEnd) ~ expression ).rep() ~ (W ~ elseStatement).?
		)/*.log()*/
    	.map {
			case (si, (condition, statements, elseBlock), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the if condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.IfStatement(condition, statementList, elseBlock.getOrElse(ast.None(srcPos(ei, ei))), srcPos(si, ei))
		}

	private val whileStatement = blockGenerator(whileConditionExpression)/*.log()*/
		.map {
			case (si, (condition, statements), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the while condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.WhileStatement(condition, statementList, srcPos(si, ei))
		}
	private val untilStatement = blockGenerator(untilConditionExpression)/*.log()*/
		.map {
			case (si, (condition, statements), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the util condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.UntilStatement(condition, statementList, srcPos(si, ei))
		}
	private val functionStatement = blockGenerator(functionSignatureExpression)/*.log()*/
		.map {
			case (si, ((funcVar, argVars), statements), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the util condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.FunctionStatement(funcVar, argVars.toVector, statementList, srcPos(si, ei))
		}

	private lazy val commentStatement = P( "(" ~ (!")" ~ AnyChar).rep ~ ")" )/*.log()*/

	// All statements that start with a terminal must come first, as things like
	// "While Number is as high as Divisor" can start to be parsed as an assignment to
	// "While Number" causing hell to be raised
	private lazy val expression: P[ast.Node] = P(
		W ~ (
			putStatement |
				listenStatement |
				sayStatement |
				giveBackStatement |
				ifStatement |
				whileStatement |
				untilStatement |
				functionStatement |
				incrementStatement |
				decrementStatement |
				continueStatement |
				breakStatement |
				variableAssignmentStatement |
				functionCall |
				I("").map{ case (si, _, ei) => ast.None(srcPos(si, ei))}
		) ~ W ~ lineEnd
	)/*.log()*/

	private lazy val program = I( (!End ~/ expression).rep ~ End ).map {
		case (si, statements, ei) =>
			val statementList =
			// Create a statement list with a source mapping at of at least the end of the if condition
				ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
			ast.Program(statementList, srcPos(si, ei))
	}

	def apply() = program
	def apply(input: String) = program.parse(input)
}
