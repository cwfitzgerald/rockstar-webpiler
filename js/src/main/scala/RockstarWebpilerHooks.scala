import fastparse.core.Parsed.{Failure, Success}
import org.querki.jquery._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.{html => htmlDom}
import rockstar.{ast, util}
import scalatags.JsDom.all._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._
import scala.util.Try

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

	def printAST(curNode: ast.Node, breaks: util.lineBreaks, cn: util.charsNeeded, indent: Int = 0): Seq[dom.Element] = {
		val pos = curNode.srcPos

		implicit val pnl: positionFormatting = positionFormatting(breaks, cn)

		walkASTImpl(curNode, indent)
	}

	// I'm not too proud of that implicit there, but ¯\_(ツ)_/¯
	def walkASTImpl(curNode: ast.Node, indent: Int)(implicit pnl: positionFormatting) : Seq[htmlDom.Element] = {
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
					walkASTImpl(n.function, indent + 2) ++
					printer("Args", indent + 1) ++
					n.args.flatMap(x => walkASTImpl(x, indent + 2))
			case _: ast.Break => printer("Break")
			case _: ast.Continue => printer("Continue")
			case n: ast.Return => printer("Return") ++ walkASTImpl(n.value, indent + 1)
			case n: ast.Addition => printer("Addition") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Subtraction => printer("Subtraction") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Multiplication => printer("Multiplication") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Division => printer("Division") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Greater => printer("Greater") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Less => printer("Less") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.GreaterEq => printer("Greater or Equal") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.LessEq => printer("Lesser or Equal") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Eq => printer("Equal") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Neq => printer("Not Equal") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.And => printer("And") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Or => printer("Or") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Nor => printer("Nor") ++ walkASTImpl(n.left, indent + 1) ++ walkASTImpl(n.right, indent + 1)
			case n: ast.Increment => printer("Increment") ++ walkASTImpl(n.value, indent + 1)
			case n: ast.Decrement => printer("Decrement") ++ walkASTImpl(n.value, indent + 1)
			case n: ast.Print => printer("Print") ++ walkASTImpl(n.value, indent + 1)
			case n: ast.GetLine => printer("GetLine") ++ walkASTImpl(n.variable, indent + 1)
			case n: ast.Set => printer("Set") ++ walkASTImpl(n.variable, indent + 1) ++ walkASTImpl(n.value, indent + 1)
			case n: ast.StatementList => printer("Statement List") ++ n.statements.flatMap(x => walkASTImpl(x, indent + 1))
			case n: ast.IfStatement =>
				printer("If") ++
					printer("Conditon", indent + 1) ++
					walkASTImpl(n.condition, indent + 2) ++
					walkASTImpl(n.statements, indent + 1) ++
					printer("Else") ++
					walkASTImpl(n.elseStatements, indent + 1)
			case n: ast.WhileStatement =>
				printer("While") ++
					printer("Conditon", indent + 1) ++
					walkASTImpl(n.condition, indent + 2) ++
					walkASTImpl(n.statements, indent + 1)
			case n: ast.UntilStatement =>
				printer("Until") ++
					printer("Conditon", indent + 1) ++
					walkASTImpl(n.condition, indent + 2) ++
					walkASTImpl(n.statements, indent + 1)
			case n: ast.FunctionStatement =>
				printer("Function") ++
					printer("Name:", indent + 1) ++
					walkASTImpl(n.name, indent + 2) ++
					printer("Variables", indent + 1) ++
					n.variables.flatMap(x => walkASTImpl(x, indent + 2)) ++
					walkASTImpl(n.statements, indent + 1)
			case n: ast.Program => printer("Program") ++ walkASTImpl(n.statements, indent + 1)
		}
	}

	def applyShortlink(shortlink: String): Unit = {
		val origin = dom.window.location.origin.get

		val shortlinkUrl = (origin :+ '/') ++ shortlink
		$("#shortLinkValue").value(shortlinkUrl)
		dom.window.history.replaceState("", dom.document.title, shortlinkUrl)
	}

	def getShortlink(content: String): Future[String] = {
		val fdDyn = js.Dynamic.newInstance(js.Dynamic.global.FormData)()

		val blobDyn = js.Dynamic.newInstance(js.Dynamic.global.Blob)(Seq(content).toJSArray, js.Dynamic.literal("type" -> "application/x.rockstar"))

		fdDyn.append("file", blobDyn)

		val fd = fdDyn.asInstanceOf[dom.FormData]

		val future = Ajax.post( "/api/gen_shortlink", Ajax.InputData.formdata2ajax(fd))

		future.map{ xhr => ujson.read(xhr.responseText)("link").str }.map(s => {println(s); s} )
	}

	@JSExportTopLevel("init")
	def init(): Unit = {
		// jquery selectors
		val input = $("#input").get(0).get.asInstanceOf[htmlDom.TextArea]
		val output = $("#output").get(0).get

		println("init")

		input.onkeyup = { e: dom.KeyboardEvent => sourceModified(input) }
		input.onclick = { e: dom.MouseEvent => sourceModified(input) }

		def modelCallback(j: JQueryEventObject, a: Any): Any = {
			getShortlink(input.value).map(applyShortlink)
		}

		$("#shortLinkModel").on("shown.bs.modal", modelCallback _)

		sourceModified(input)
	}

	private var lastCompile = new String

	def sourceModified(element: htmlDom.TextArea): Unit = {
		println("sm")
		val currentText = element.value

//		getShortlink(currentText).foreach(x => println(x))

		val lineMap = util.createLineBreaks(currentText)
		val charsNeeded = util.findCharsNeeded(lineMap)

		$("#position").text(s"${util.formatAsString(lineMap, charsNeeded, element.selectionStart)}")

		if (currentText != lastCompile) {
			val (res, time) = compile(currentText) match {
				case compileResult(None, Some(program), compTime) => (printAST(program, lineMap, charsNeeded), compTime)
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