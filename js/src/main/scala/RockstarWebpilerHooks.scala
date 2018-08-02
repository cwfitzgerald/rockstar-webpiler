import fastparse.core.Parsed.{Failure, Success}

import scala.util.Try
import scala.concurrent.duration._
import scala.concurrent.Future

import org.querki.jquery._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.ext.Ajax.InputData
import org.scalajs.dom.{html => htmlDom}
import rockstar.{ast, util}
import scalatags.JsDom.all._

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object RockstarWebpilerHooks {
	case class compileError(expected: String, idx: Int)
	case class compileResult(error: Option[compileError], output: Option[ast.Program], time: Long)

	def compile(input: String): compileResult = {
		println("compiling...")

		val startTime = java.time.Instant.now()
		val parsed = Try{ rockstar.parser(input) }.toEither
		val endTime = java.time.Instant.now()

		val totalTime = endTime.toEpochMilli - startTime.toEpochMilli

		parsed match {
			case Right(Success(ast, _)) => compileResult(None, Some(ast), totalTime)
			case Right(Failure(ast, idx, traceback)) => compileResult(Some(compileError(s"Expecting: ${ast.toString}", idx)), None, totalTime)
			case Left(exception) => compileResult(Some(compileError(s"Exception: ${exception.toString}", 0)), None, totalTime)
		}
	}

	case class positionFormatting(breaks: util.lineBreaks, cn: util.charsNeeded)

	def walkAst(curNode: ast.Node, breaks: util.lineBreaks, cn: util.charsNeeded, indent: Int = 0): Seq[dom.Element] = {
		val pos = curNode.srcPos

		implicit val pnl: positionFormatting = positionFormatting(breaks, cn)

		walkAstImpl(curNode, indent)
	}

	// I'm not too proud of that implicit there, but ¯\_(ツ)_/¯
	def walkAstImpl(curNode: ast.Node, indent: Int)(implicit pnl: positionFormatting) : Seq[htmlDom.Element] = {
		val pos = curNode.srcPos

		val posStartString = util.formatAsString(pnl.breaks, pnl.cn, pos.start)
		val posEndString = util.formatAsString(pnl.breaks, pnl.cn, pos.end)

		def printer(value: String, indent: Int = indent) = {
			Seq(
				div(
					s"$posStartString-$posEndString",
					(1 to indent).map(_ => raw("&nbsp;|")),
					" ",
					value
				).render
			)
		}

		curNode match {
			case _: ast.None => printer("None")
			case n: ast.CommonVariable => printer(s"Variable: '${n.prefix}:${n.name}'")
			case n: ast.ProperVariable => printer(s"Variable: '${n.name}'")
			case n: ast.Pronoun => printer(s"Pronoun: '${n.name}'")
			case _: ast.Mysterious => printer("Mysterious")
			case _: ast.Null => printer("Null")
			case n: ast.BooleanC => printer(s"Constant: ${n.value}")
			case n: ast.NumberC => printer(s"Constant: ${n.value}")
			case n: ast.StringC => printer(s"Constant: \'${n.value}\'")
			case n: ast.FunctionCall =>
				printer(s"Function Call: ") ++
					printer("Name", indent + 1) ++
					walkAstImpl(n.function, indent + 2) ++
					printer("Args", indent + 1) ++
					n.args.flatMap(x => walkAstImpl(x, indent + 2))
			case _: ast.Break => printer("Break")
			case _: ast.Continue => printer("Continue")
			case n: ast.Return => printer("Return") ++ walkAstImpl(n.value, indent + 1)
			case n: ast.Addition => printer("Addition") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Subtraction => printer("Subtraction") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Multiplication => printer("Multiplication") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Division => printer("Division") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Greater => printer("Greater") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Less => printer("Less") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.GreaterEq => printer("Greater or Equal") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.LessEq => printer("Lesser or Equal") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Eq => printer("Equal") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Neq => printer("Not Equal") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.And => printer("And") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Or => printer("Or") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Nor => printer("Nor") ++ walkAstImpl(n.left, indent + 1) ++ walkAstImpl(n.right, indent + 1)
			case n: ast.Increment => printer("Increment") ++ walkAstImpl(n.value, indent + 1)
			case n: ast.Decrement => printer("Decrement") ++ walkAstImpl(n.value, indent + 1)
			case n: ast.Print => printer("Print") ++ walkAstImpl(n.value, indent + 1)
			case n: ast.GetLine => printer("GetLine") ++ walkAstImpl(n.variable, indent + 1)
			case n: ast.Set => printer("Set") ++ walkAstImpl(n.variable, indent + 1) ++ walkAstImpl(n.value, indent + 1)
			case n: ast.StatementList => printer("Statement List") ++ n.statements.flatMap(x => walkAstImpl(x, indent + 1))
			case n: ast.IfStatement =>
				printer("If") ++
					printer("Conditon", indent + 1) ++
					walkAstImpl(n.condition, indent + 2) ++
					walkAstImpl(n.statements, indent + 1) ++
					printer("Else") ++
					walkAstImpl(n.elseStatements, indent + 1)
			case n: ast.WhileStatement =>
				printer("While") ++
					printer("Conditon", indent + 1) ++
					walkAstImpl(n.condition, indent + 2) ++
					walkAstImpl(n.statements, indent + 1)
			case n: ast.UntilStatement =>
				printer("Until") ++
					printer("Conditon", indent + 1) ++
					walkAstImpl(n.condition, indent + 2) ++
					walkAstImpl(n.statements, indent + 1)
			case n: ast.FunctionStatement =>
				printer("Function") ++
					printer("Name:", indent + 1) ++
					walkAstImpl(n.name, indent + 2) ++
					printer("Variables", indent + 1) ++
					n.variables.flatMap(x => walkAstImpl(x, indent + 2)) ++
					walkAstImpl(n.statements, indent + 1)
			case n: ast.Program => printer("Program") ++ walkAstImpl(n.statements, indent + 1)
		}
	}

	def getShortlink(content: String): Future[String] = {
		val fdDyn = js.Dynamic.newInstance(js.Dynamic.global.FormData)()

		val blobDyn = js.Dynamic.newInstance(js.Dynamic.global.Blob)(Seq(content).toJSArray, js.Dynamic.literal("type" -> "application/x.rockstar"))

		fdDyn.append("file", blobDyn)

		val fd = fdDyn.asInstanceOf[dom.FormData]

		val future = Ajax.post( "/api/gen_shortlink", Ajax.InputData.formdata2ajax(fd))

		future.map{ xhr => ujson.read(xhr.responseText)("link").str }
	}

	@JSExportTopLevel("init")
	def init(): Unit = {
		// jquery selectors
		val input = $("#input").get(0).get.asInstanceOf[htmlDom.TextArea]
		val output = $("#output").get(0).get

		println("init")

		input.onkeyup = { e: dom.KeyboardEvent => sourceModified(input) }
		input.onclick = { e: dom.MouseEvent => sourceModified(input) }

		sourceModified(input)
	}

	private var lastCompile = new String

	def sourceModified(element: htmlDom.TextArea): Unit = {
		println("sm")
		val currentText = element.value

		getShortlink(currentText).foreach(x => println(x))

		val lineMap = util.createLineBreaks(currentText)
		val charsNeeded = util.findCharsNeeded(lineMap)

		$("#position").text(s"${util.formatAsString(lineMap, charsNeeded, element.selectionStart)}")

		if (currentText != lastCompile) {
			val (res, time) = compile(currentText) match {
				case compileResult(None, Some(program), compTime) => (walkAst(program, lineMap, charsNeeded), compTime)
				case compileResult(Some(compileError(message, index)), None, compTime) => {
					val errorLoc = util.findLinePair(lineMap, index)
					(Seq(p(s"Error. $message at pos ${errorLoc.line}:${errorLoc.char}").render), compTime)
				}
			}

			$("#output").empty()
			$("#output").append(res)

			$("#time-to-compile").text(s"${time}ms")

			lastCompile = currentText
		}
	}
}