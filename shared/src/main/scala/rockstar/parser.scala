package rockstar

import fastparse.all
import fastparse.all._

object parser {
	/////////////////////
	// Core Primitives //
	/////////////////////

	private val CI = IgnoreCase

	private val W = P( (CharsWhileIn(Seq(' ', '\t', ','), 1) | commentStatement).rep )
	private val nonCommaWhitespace = P( (CharsWhileIn(Seq(' ', '\t'), 1) | commentStatement).rep )
	private val newLine = P( "\n" | "\r\n" )/*.log()*/
	private val lineEnd = P( newLine | End )/*.log()*/

	private val capitalLetter = P( CharIn('A' to 'Z') )
	private val lowerLetter = P( CharIn('a' to 'z') )
	private val allowedSymbols = P( CharIn(Seq('\'' )) )
	private val number = P( CharIn('0' to '9') )

	private val space = P( CharIn(Seq(' ')) )
	private val fullLetter = P( capitalLetter | lowerLetter | allowedSymbols )
	private val word = P( fullLetter.rep(1) )

	private val commonVariableVariableName = P( lowerLetter.rep(1).! )/*.log()*/
	private val commonVariableKeywords = P( StringInIgnoreCase("a", "an", "the", "my", "your") )/*.log()*/

	private val properVariableName = P( (capitalLetter ~ fullLetter.rep ~ (space ~ capitalLetter ~ fullLetter.rep).rep).! )/*.log()*/

	private val pronoun = P( StringInIgnoreCase("it", "he", "she", "him", "her", "them", "they") )/*.log()*/

	private val variable = P( (commonVariableKeywords ~ W ~ commonVariableVariableName) | pronoun | properVariableName )/*.log()*/

	// all the literals in the language
	private val tMysterious = P( CI("mysterious") )/*.log()*/
	private val tNull = P( StringInIgnoreCase("null", "nothing", "nowhere", "nobody" ) )/*.log()*/
	private val tBooleanTrue = P( StringInIgnoreCase("true", "right", "yes", "ok") )/*.log()*/
	private val tBooleanFalse = P( StringInIgnoreCase("false", "wrong", "no", "lines") )/*.log()*/
	private val tNumber = P( (number.rep(1) ~ ("." ~ number.rep).?).! )/*.log()*/
	private val tString = P( "\"" ~ (!"\"" ~ AnyChar).rep.! ~ "\"" )/*.log()*/

	private val tLiteral = P( tMysterious | tNull | tBooleanTrue | tBooleanFalse | tNumber | tString )/*.log()*/

	///////////////////////////
	// Grammar / Expressions //
	///////////////////////////

	// arithmetic
	private val mulDivOperator = P( StringInIgnoreCase("times", "of", "over", "by") )
	private val plusMinusOperator = P( StringInIgnoreCase("plus", "with", "minus", "without") )

	// comparison operators
	private val greaterThanOperator = P( StringInIgnoreCase("higher", "greater", "bigger", "stronger") )
	private val lessThanOperator = P( StringInIgnoreCase("lower", "less", "smaller", "weaker") )
	private val greaterThanEqOperator = P( StringInIgnoreCase("high", "great", "big", "strong") )
	private val lessThanEqOperator = P( StringInIgnoreCase("low", "little", "small", "weak") )
	private val eqOperator = P( StringInIgnoreCase("is") )
	private val neqOperator = P( StringInIgnoreCase("ain't") )
	private val negationOperator = P( StringInIgnoreCase("not") )

	// compound comparison operators
	private val diffOperator = P( CI("is") ~ W ~ (greaterThanOperator | lessThanOperator) ~ W ~ "than" )
	private val diffEqOperator = P( CI("is") ~ W ~ CI("as") ~ W ~ (greaterThanEqOperator | lessThanEqOperator) ~ W ~ "as" )
	private val compEqOperator = P( (eqOperator ~ negationOperator.?) | neqOperator )

	private val comparisonOperator = P( diffEqOperator | diffOperator | compEqOperator )/*.log()*/

	// boolean operators
	private val booleanOperator = P( StringInIgnoreCase("and", "or", "nor") )

	// represents anything that can give you a value
	private val valueProvider = P( tLiteral | variable )/*.log()*/

	/////////////////////////
	// Order of Operations //
	/////////////////////////

	private val functionArgumentDelim = P( StringInIgnoreCase(", ", "and") )
	private val functionCall =
		P(
			(variable ~ ( W ~ CI("taking") ~ W ~ valueProvider ~ nonCommaWhitespace ~ (functionArgumentDelim ~ W ~ valueProvider).rep ).? )
			| tLiteral
		)/*.log()*/

	private val mulDivOperation =
		P( functionCall ~
			(W ~ mulDivOperator ~ W ~ functionCall).rep
		)/*.log()*/

	private val plusMinusOperation =
		P( mulDivOperation ~
			(W ~ plusMinusOperator ~ W ~ mulDivOperation).rep
		)/*.log()*/

	private val comparisonOperation =
		P( plusMinusOperation ~
			(W ~ comparisonOperator ~ W ~ plusMinusOperation).rep
		)/*.log()*/

	private val booleanOperation =
		P( comparisonOperation ~
			(W ~ booleanOperator ~ W ~ comparisonOperation).rep
		)/*.log()*/

	private val mathExpr = P( booleanOperation )/*.log()*/

	///////////////////////////
	// Standalone statements //
	///////////////////////////

	private val incrementStatement = P( CI("build") ~ W ~ variable ~ W ~ CI("up") )/*.log()*/
	private val decrementStatement = P( CI("knock") ~ W ~ variable ~ W ~ CI("down") )/*.log()*/

	private val breakStatement = P( CI("break") ~ (W ~ CI("it") ~ W ~ CI("down")).? )
	private val continueStatement = P( CI("continue") | (CI("take") ~ W ~ CI("it") ~ W ~ CI("to") ~ W ~ CI("the") ~ W ~ CI("top")) )
	private val putStatement = P( CI("put") ~ W ~ plusMinusOperation ~ W ~ "into" ~ W ~variable )/*.log()*/
	private val listenStatement = P( CI("listen") ~ W ~ CI("to") ~ W ~ variable )/*.log()*/
	private val sayCommands = P( StringInIgnoreCase("say", "shout", "whisper", "scream") )
	private val sayStatement = P( sayCommands ~ W ~ mathExpr )/*.log()*/
	private val giveBackStatement = P( CI("give") ~ W ~ CI("back") ~ W ~ mathExpr )/*.log()*/

	private val variableAssignmentOperator = P( StringInIgnoreCase("is", "was") )/*.log()*/
	private val variablePoeticTypeLiteral = P( tBooleanTrue | tBooleanFalse | tNull | tMysterious )/*.log()*/
	private val variablePoeticNumberLiteral = P( (word ~ W).rep(1) ~ &(lineEnd) )/*.log()*/
	private val variableTypeAssignment =
		P(
			variable ~ W ~ variableAssignmentOperator ~ W ~
				( (mathExpr ~ &(W ~ lineEnd)) | variablePoeticTypeLiteral | variablePoeticNumberLiteral )
		)/*.log()*/
	private val variableStrLiteralAssignment =
		P(
			variable ~ W ~ CI("says") ~ W ~ (!newLine ~ AnyChar)
		)/*.log()*/

	private val variableAssignmentStatement = P( variableTypeAssignment | variableStrLiteralAssignment )/*.log()*/

	private val ifConditionExpression = P( CI("if") ~ W ~ mathExpr )/*.log()*/
	private val whileConditionExpression = P( CI("while") ~ W ~ mathExpr )/*.log()*/
	private val untilConditionExpression = P( CI("until") ~ W ~ mathExpr )/*.log()*/
	private val functionSignatureExpression = P( variable ~ W ~ CI("takes") ~ W ~ variable ~ (W ~ CI("and") ~ W ~ variable).rep )/*.log()*/

	private def blockGenerator[T](parser: P[T]): fastparse.core.Parser[(T, Seq[Any]),Char,String] = {
		P(
			parser ~ W ~ newLine ~
				(!(W ~ lineEnd) ~ expression).rep()
		)
	}

	private val ifStatement = blockGenerator(ifConditionExpression)/*.log()*/
	private val whileStatement = blockGenerator(whileConditionExpression)/*.log()*/
	private val untilStatement = blockGenerator(untilConditionExpression)/*.log()*/
	private val functionStatement = blockGenerator(functionSignatureExpression)/*.log()*/

	private lazy val commentStatement = P( "(" ~ (!")" ~ AnyChar).rep ~ ")" )

	// All statements that start with a terminal must come first, as things like
	// "While Number is as high as Divisor" can start to be parsed as an assignment to
	// "While Number" causing hell to be raised
	private lazy val expression = P(
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
				""
		) ~ W ~ lineEnd
	)/*.log()*/

	private lazy val program = P( (!End ~ expression).rep ~ End )

	def apply() = program
	def apply(input: String) = program.parse(input)
}
