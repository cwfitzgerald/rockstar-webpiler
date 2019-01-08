package rockstar.parse

import fastparse._, NoWhitespace._
import rockstar.parse.ast.{SourcePosition => srcPos}

import scala.annotation.tailrec

object parser {
	/////////////////////
	// Core Primitives //
	/////////////////////

	@inline
	private def CI[_: P](string: String) = IgnoreCase(string)

	private def I[_: P, T](p: => P[T]) = P( Index ~ p ~ Index )

	private def W[_: P] = P( (CharsWhileIn(" \t,", 1) | commentStatement).rep )/*.log*/
	private def MW[_: P] = P( (CharsWhileIn(" \t,", 1) | commentStatement).rep(1) )/*.log*/
	private def nonCommaWhitespace[_: P] = P( (CharsWhileIn(" \t", 1) | commentStatement).rep )
	private def newLine[_: P] = P( "\n" | "\r\n" )/*.log*/.opaque("Newline")
	private def lineEnd[_: P] = P( newLine | End )/*.log*/.opaque("End of Line")

	private def capitalLetter[_: P] = P( CharIn("A-Z") )
	private def lowerLetter[_: P] = P( CharIn("a-z") )
	private def number[_: P] = P( CharIn("0-9") )

	private def space[_: P] = P( " " )
	private def fullLetter[_: P] = P( capitalLetter | lowerLetter )
	private def word[_: P] = P( CharsWhile(c => !c.isWhitespace && (c != '(' && c != '.')) )/*.log*/.opaque("Word")

	private def commonVariableVariableName[_: P] = P( lowerLetter.rep(1) )/*.log*/
	private def commonVariableKeywords[_: P] = P( StringInIgnoreCase("a", "an", "the", "my", "your") )/*.log*/
	private def commonVariable[_: P] = I( commonVariableKeywords.! ~ MW ~ commonVariableVariableName.! )/*.log*/
    	.map {case (si, (k, n), ei) => ast.CommonVariable(s"${k}_$n", srcPos(si, ei)) }

	private def properVariableName[_: P] = I( (capitalLetter ~ fullLetter.rep ~ (space ~ capitalLetter ~ fullLetter.rep).rep).! )/*.log*/
    	.map {case (si, n, ei) => ast.ProperVariable(n, srcPos(si, ei)) }

	private def pronoun[_: P] = I( StringInIgnoreCase("it", "he", "she", "him", "her", "they", "them", "ze", "hir", "zie", "zir", "xe", "xem", "ve", "ver").! )/*.log*/
    	.map {case (si, n, ei) => ast.Pronoun(n, srcPos(si, ei)) }

	private def variable[_: P] = P( commonVariable | properVariableName | pronoun )/*.log*/
    	.opaque("Variable")

	// all the literals in the language
	private def tMysterious[_: P] = I( CI("mysterious") )/*.log*/
		.map { case (si, _, ei) => ast.MysteriousConstant(srcPos(si, ei)) }

	private def tNull[_: P] = I( StringInIgnoreCase("null", "nothing", "nowhere", "nobody", "gone", "empty" ) )/*.log*/
    	.map { case (si, _, ei) => ast.NullConstant(srcPos(si, ei)) }

	private def tBooleanTrue[_: P] = I( StringInIgnoreCase("true", "right", "yes", "ok") )/*.log*/
    	.map { case (si, _, ei) => ast.BooleanConstant(value = true, srcPos(si, ei)) }

	private def tBooleanFalse[_: P] = I( StringInIgnoreCase("false", "wrong", "no", "lines") )/*.log*/
		.map { case (si, _, ei) => ast.BooleanConstant(value = false, srcPos(si, ei)) }

	private def tNumber[_: P] = I( ("-".?.! ~ W ~ number.rep(1).!) ~ ("." ~ number.rep).!.? )/*.log*/
		.map { case (si, (sign, front, back), ei) => ast.NumberConstant(BigDecimal(sign + front + back.getOrElse(".0")), srcPos(si, ei)) }

	private def tString[_: P] = I( "\"" ~/ (!CharIn("\"\n") ~/ AnyChar).rep.! ~ "\"" )/*.log*/
    	.opaque("String")
    	.map { case (si, v, ei) => ast.StringConstant(v, srcPos(si, ei)) }

	private def tLiteral[_: P] =
		P( tMysterious | tNull | tBooleanTrue | tBooleanFalse | tNumber | tString )/*.log*/


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

	private def mulDivOperator[_: P] = P( StringInIgnoreCase("times", "of", "over").! )
    	.map {
		    case "times" => mulEnumerator()
		    case "of" => mulEnumerator()
		    case "over" => divEnumerator()
	    }

	private def plusMinusOperator[_: P] = P( StringInIgnoreCase("plus", "with", "minus", "without").! )
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

	private def greaterThanOperator[_: P] = P( StringInIgnoreCase("higher", "greater", "bigger", "stronger") )
		.map {_ => gtEnumerator()}
	private def lessThanOperator[_: P] = P( StringInIgnoreCase("lower", "less", "smaller", "weaker") )
		.map {_ => ltEnumerator()}
	private def greaterThanEqOperator[_: P] = P( StringInIgnoreCase("high", "great", "big", "strong") )
		.map {_ => geEnumerator()}
	private def lessThanEqOperator[_: P] = P( StringInIgnoreCase("low", "little", "small", "weak") )
		.map {_ => leEnumerator()}
	private def eqOperator[_: P] = P( StringInIgnoreCase("is") )
		.map {_ => eqEnumerator()}
	private def neqOperator[_: P] = P( StringInIgnoreCase("ain't", "aint") )
		.map {_ => neqEnumerator()}
	private def negationOperator[_: P] = P( StringInIgnoreCase("not") )

	// compound comparison operators
	private def diffNeqOperator[_: P] = P( CI("is") ~ W ~ (greaterThanOperator | lessThanOperator) ~ W ~/ CI("than") )
	private def diffEqOperator[_: P] = P( CI("is") ~ W ~ CI("as") ~ W ~/ (greaterThanEqOperator | lessThanEqOperator) ~ W ~/ CI("as") )
	private def diffOperator[_: P] = P( diffEqOperator | diffNeqOperator )/*.log*/

	private def compEqOperator[_: P] = P( (eqOperator ~ (W ~ negationOperator.! ~ &(MW)).?) | neqOperator )
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
	private def booleanOperator[_: P] = P( StringInIgnoreCase("and", "or", "nor").! )
    	.map {
		    case "and" => andEnumerator()
		    case "or" => orEnumerator()
		    case "nor" => norEnumerator()
	    }

	// represents anything that can give you a value
	private def valueProvider[_: P]: P[ast.Subexprable] = P( tLiteral | variable )/*.log*/.opaque("Value provider")

	/////////////////////////
	// Order of Operations //
	/////////////////////////

	private def functionArgumentDelim[_: P] = P( StringInIgnoreCase(",", "and") )
	private def functionCall[_: P] =
		I(
			(variable ~ (W ~ CI("taking") ~ W ~/ valueProvider ~ (nonCommaWhitespace ~ functionArgumentDelim ~ W ~/ valueProvider).rep ).? )
			 | tLiteral

		)/*.log*/
    	.map{
			case (si, v, ei) => v match {
				case lit: ast.Constant => lit
				case (variable: ast.Variable, None) => variable
				case (funcName: ast.Variable, Some((firstArg: ast.Value, nArgs: Object))) =>
					val nArgsCast = nArgs.asInstanceOf[Seq[ast.Value]]

					ast.FunctionCall(funcName, (firstArg +: nArgsCast).toVector, srcPos(si, ei))
			}
		}

	private def topLevelFunctionCall[_: P] =
		I(
			variable ~ W ~ CI("taking") ~ W ~/ valueProvider ~ (nonCommaWhitespace ~ functionArgumentDelim ~ W ~/ valueProvider).rep

		)/*.log*/
			.map{
				case (si, v, ei) => v match {
					case (funcName: ast.Variable, firstArg: ast.Value, nArgs: Object) =>
						val nArgsCast = nArgs.asInstanceOf[Seq[ast.Subexprable]]

						ast.FunctionCall(funcName, (firstArg +: nArgsCast).toVector, srcPos(si, ei))
			}
		}

	@tailrec
	private def mulDivOperationGen(lhs: ast.Subexprable, rhs: List[(mulDivEnumeratorClass, ast.Subexprable)]): ast.Subexprable = rhs match {
		case Nil => lhs
		case (mulEnumerator(), node) :: rest =>
			mulDivOperationGen(ast.Multiplication(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (divEnumerator(), node) :: rest =>
			mulDivOperationGen(ast.Division(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private def mulDivOperation[_: P] =
		P( functionCall ~
			(W ~ mulDivOperator ~ W ~/ functionCall).rep
		)/*.log*/
    	.map {
			case (lhs, rhs) => mulDivOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def plusMinusOperationGen(lhs: ast.Subexprable, rhs: List[(plusMinusEnumeratorClass, ast.Subexprable)]): ast.Subexprable = rhs match {
		case Nil => lhs
		case (plusEnumerator(), node) :: rest =>
			plusMinusOperationGen(ast.Addition(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (minusEnumerator(), node) :: rest =>
			plusMinusOperationGen(ast.Subtraction(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private def plusMinusOperation[_: P] =
		P( mulDivOperation ~
			(W ~ plusMinusOperator ~ W ~/ mulDivOperation).rep
		)/*.log*/
		.map {
			case (lhs, rhs) => plusMinusOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def diffComparisonOperationGen(lhs: ast.Subexprable, rhs: List[(DiffEnumerator, ast.Subexprable)]): ast.Subexprable = rhs match {
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

	private def diffComparisonOperation[_: P] =
		P( plusMinusOperation ~
			(W ~ diffOperator ~ W ~/ plusMinusOperation).rep
		)/*.log*/
    	.map {
			case (lhs, rhs) => diffComparisonOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def compEqComparisonOperationGen(lhs: ast.Subexprable, rhs: List[(compEqEnumerator, ast.Subexprable)]): ast.Subexprable = rhs match {
		case Nil => lhs
		case (eqEnumerator(), node) :: rest =>
			compEqComparisonOperationGen(ast.Eq(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (neqEnumerator(), node) :: rest =>
			compEqComparisonOperationGen(ast.Neq(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private def compEqComparisonOperation[_: P] =
		P( diffComparisonOperation ~
			(W ~ compEqOperator ~ W ~/ diffComparisonOperation).rep
		)/*.log*/
    	.map {
			case (lhs, rhs) => compEqComparisonOperationGen(lhs, rhs.toList)
		}

	@tailrec
	private def boolOperationGen(lhs: ast.Subexprable, rhs: List[(BooleanEnumerator, ast.Subexprable)]): ast.Subexprable = rhs match {
		case Nil => lhs
		case (andEnumerator(), node) :: rest =>
			boolOperationGen(ast.And(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (orEnumerator(), node) :: rest =>
			boolOperationGen(ast.Or(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
		case (norEnumerator(), node) :: rest =>
			boolOperationGen(ast.Nor(lhs, node, lhs.srcPos.expand(node.srcPos)), rest)
	}

	private def booleanOperation[_: P] =
		P( compEqComparisonOperation ~
			(W ~ booleanOperator ~ W ~/ compEqComparisonOperation).rep
		)/*.log*/
    	.map {
			case (lhs, rhs) => boolOperationGen(lhs, rhs.toList)
		}

	private def mathExpr[_: P] = P( booleanOperation )/*.log*/
    	.opaque("Math Expression")

	///////////////////////////
	// Standalone statements //
	///////////////////////////

	private def incrementStatement[_: P] = I( CI("build") ~ W ~/ variable ~ W ~ CI("up") )/*.log*/
    	.map { case (si, v, ei) => ast.Increment(v, srcPos(si, ei)) }
	private def decrementStatement[_: P] = I( CI("knock") ~ W ~/ variable ~ W ~ CI("down") )/*.log*/
		.map { case (si, v, ei) => ast.Decrement(v, srcPos(si, ei)) }

	private def breakStatement[_: P] = I( CI("break") ~/ (W ~ CI("it") ~ W ~/ CI("down")).? ).opaque("Break Statement")
    	.map { case (si, _, ei) => ast.Break(srcPos(si, ei)) }
	private def continueStatement[_: P] = I( CI("continue") | (CI("take") ~ W ~/ CI("it") ~ W ~/ CI("to") ~ W ~/ CI("the") ~ W ~/ CI("top")) ).opaque("Continue Statement")
		.map { case (si, _, ei) => ast.Continue(srcPos(si, ei)) }
	private def putStatement[_: P] = I( CI("put") ~ W ~ mathExpr ~ W ~ CI("into") ~ W ~ variable )/*.log*/
    	.map { case (si, (value, vari), ei) => ast.Set(value, vari, srcPos(si, ei)) }
	private def listenStatement[_: P] = I( CI("listen") ~ W ~/ CI("to") ~ W ~/ variable )/*.log*/
    	.map { case (si, vari, ei) => ast.GetLine(vari, srcPos(si, ei)) }
	private def sayCommands[_: P] = P( StringInIgnoreCase("say", "shout", "whisper", "scream") )
	private def sayStatement[_: P] = I( sayCommands ~ W ~/ mathExpr )/*.log*/
    	.map { case (si, expr, ei) => ast.Print(expr, srcPos(si, ei)) }
	private def giveBackStatement[_: P] = I( CI("give") ~ W ~/ CI("back") ~ W ~/ mathExpr )/*.log*/
    	.map { case (si, expr, ei) => ast.Return(expr, srcPos(si, ei)) }

	private def resolvePoeticLiteral(string: Seq[String]) = {
		string.map(word => (word.filter( c => c.isLetter ).length % 10).toString).foldLeft(""){_ + _}
	}

	private def variableAssignmentOperator[_: P] = P( StringInIgnoreCase("is", "was", "were", "'s") )/*.log*/
    	.opaque("Variable Assignment Operator")
	private def variablePoeticTypeLiteral[_: P] = P( tBooleanTrue | tBooleanFalse | tNull | tMysterious )/*.log*/
    	.opaque("Poetic Type Literal")
	private def variablePoeticNumberLiteral[_: P] = I( (word.! ~ W).rep(1) ~/ ("." ~ (W ~ word.!).rep).? ~ &(lineEnd) )/*.log*/
    	.opaque("Poetic Number Literal")
    	.map {
			case (si, (wordLeftSeq, wordRightSeq), ei) =>
				ast.NumberConstant(
					BigDecimal(resolvePoeticLiteral(wordLeftSeq) + "." + resolvePoeticLiteral(wordRightSeq.getOrElse(Seq("abcdefghij")))),
					srcPos(si, ei)
				)
		}
	private def variableTypeAssignment[_: P] =
		I(
			variable ~ W ~ variableAssignmentOperator ~ W ~/
				((tNull | tBooleanFalse | tBooleanTrue) | variablePoeticTypeLiteral | variablePoeticNumberLiteral)

		) /*.log*/
		.map {
			case (si, (vari, value), ei) => ast.Set(value, vari, srcPos(si, ei))
		}
	private def variableStrLiteralAssignment[_: P] =
		I(
			variable ~ W ~ CI("says") ~ W ~/ I( (!newLine ~ AnyChar).rep.! )
		) /*.log*/
		.map {
			case (si, (vari, (strSI, value, strEI)), ei) =>
				ast.Set(ast.StringConstant(value, srcPos(strSI, strEI)), vari, srcPos(si, ei))
		}

	private def variableAssignmentStatement[_: P] = P( variableTypeAssignment | variableStrLiteralAssignment )/*.log*/

	private def ifConditionExpression[_: P] = P( CI("if") ~ W ~/ mathExpr )/*.log*/
	private def elseConditionExpression[_: P] = P( CI("else") ~/ W ) /*.log*/
	private def whileConditionExpression[_: P] = P( CI("while") ~ W ~/ mathExpr )/*.log*/
	private def untilConditionExpression[_: P] = P( CI("until") ~ W ~/ mathExpr )/*.log*/
	private def functionSignatureExpression[_: P] = P( variable ~ W ~ CI("takes") ~ W ~/ variable ~ (W ~ CI("and")/*.log*/ ~ W ~ variable).rep )/*.log*/
    	.map {
			case (funcVar, arg1, argN) => (funcVar, arg1 +: argN)
		}


	private def blockGenerator[_: P, T](parser: => P[T]): P[(Int, (T, Seq[ast.TopLevel]), Int)] = {
		I(
			parser ~ W ~ newLine ~
				(!(W ~ lineEnd) ~/ expression).rep()
		)
	}

	private def elseStatement[_: P]= blockGenerator(elseConditionExpression)
		.map {
			case (si, (_, statements), ei) =>
				ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
		}/*.log*/

	private def ifStatement[_: P] =
		I(
			ifConditionExpression ~ W ~ newLine ~
				(!(W ~ lineEnd) ~/ expression ).rep() ~ (W ~ elseStatement).?
		)/*.log()*/
    	.map {
			case (si, (condition, statements, elseBlock), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the if condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.IfStatement(condition, statementList, elseBlock.getOrElse(ast.None(srcPos(ei, ei))), srcPos(si, ei))
		}

	private def whileStatement[_: P] = blockGenerator(whileConditionExpression)
		.map {
			case (si, (condition, statements), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the while condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.WhileStatement(condition, statementList, srcPos(si, ei))
		}
	private def untilStatement[_: P] = blockGenerator(untilConditionExpression)
		.map {
			case (si, (condition, statements), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the util condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.UntilStatement(condition, statementList, srcPos(si, ei))
		}
	private def functionStatement[_: P] = blockGenerator(functionSignatureExpression)
		.map {
			case (si, ((funcVar, argVars), statements), ei) =>
				val statementList =
					// Create a statement list with a source mapping at of at least the end of the util condition
					ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
				ast.FunctionStatement(funcVar, argVars.toVector, statementList, srcPos(si, ei))
		}

	private def commentStatement[_: P] = P( "(" ~ (!")" ~ AnyChar).rep ~ ")" )/*.log*/

	// All statements that start with a terminal must come first, as things like
	// "While Number is as high as Divisor" can start to be parsed as an assignment to
	// "While Number" causing hell to be raised
	private def expression[_: P]: P[ast.TopLevel] = P(
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
				topLevelFunctionCall |
				I("").map{ case (si, _, ei) => ast.None(srcPos(si, ei))}
		) ~ W ~ lineEnd
	)/*.log*/

	private def program[_: P] = I( (!End ~/ expression).rep ~ End ).map {
		case (si, statements, ei) =>
			val statementList =
			// Create a statement list with a source mapping at of at least the end of the if condition
				ast.StatementList(statements.map(_.srcPos).foldLeft(srcPos(ei, ei))(_.expand(_)), statements.toVector)
			ast.Program(statementList, srcPos(si, ei))
	}

	def apply(input: String): Parsed[ast.Program] = parse(input, program(_))
}
