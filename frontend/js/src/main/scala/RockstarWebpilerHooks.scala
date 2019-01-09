import fastparse.Parsed.{Failure, Success}
import org.querki.jquery._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html.Div
import org.scalajs.dom.{html => htmlDom}
import rockstar.{generateAST, internals}
import rockstar.internals.{ast, ir, parser}
import scalatags.JsDom.all._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._
import scala.util.Try

object RockstarWebpilerHooks {
	// Internal Methods

	object CurrentStatus extends Enumeration {
		type CurrentStatus = Value
		val AST, IR = Value
	}

	case class CompileError(expected: String, idx: Int)
	case class CompileResult(result: Either[CompileError, ast.Program], time: Long)

	def compile(input: String): CompileResult = {
		println("compiling...")

		val startTime = java.time.Instant.now()
		val parsed = Try {
			internals.parser(input)
		}.toEither
		val endTime = java.time.Instant.now()

		val totalTime = endTime.toEpochMilli - startTime.toEpochMilli

		val compileResult = parsed match {
			case Right(Success(ast, _)) => Right(ast)
			case Right(Failure(ast, idx, _)) => Left(CompileError(s"Expecting: ${ast.toString}", idx))
			case Left(exception) => Left(CompileError(s"Exception: ${exception.toString}", 0))
		}

//		val irResult = compileResult.flatMap (prog => {
//			val IRd = Try {
//				ir.FromAst(prog)
//			}.toEither
//
//			IRd match {
//				case Right(ir) => Right((prog, ir))
//				case Left(exception) => Left(CompileError(s"Exception in IR generation: ${exception.toString}", 0))
//			}
//		})

		CompileResult(compileResult, totalTime)
	}

	// Callbacks for user functionality

	def applyShortlink(shortlink: String): Unit = {
		val origin = dom.window.location.origin.get

		val shortlinkUrl = (origin :+ '/') ++ shortlink
		$("#shortLinkValue").value(shortlinkUrl)
		dom.window.history.replaceState("", dom.document.title, shortlinkUrl)
	}

	def getShortlink(content: String): Future[String] = {
		// TODO: Fix with anon subclasses
		val fdDyn = js.Dynamic.newInstance(js.Dynamic.global.FormData)()

		val blobDyn = js.Dynamic.newInstance(js.Dynamic.global.Blob)(Seq(content).toJSArray, js.Dynamic.literal("type" -> "application/x.rockstar"))

		fdDyn.append("file", blobDyn)

		val fd = fdDyn.asInstanceOf[dom.FormData]

		val future = Ajax.post("/api/gen_shortlink", Ajax.InputData.formdata2ajax(fd))

		future.map { xhr => ujson.read(xhr.responseText)("link").str }.map(s => {
			println(s); s
		})
	}

	def printAST(program: ast.Program, breaks: rockstar.util.lineBreaks, needed: rockstar.util.charsNeeded): Seq[Div] = {
		val stringSeq = ast.PrettyPrinter(program, breaks, needed)

		stringSeq.map(s => div(s, br()).render)
	}

	def setActive(value: CurrentStatus.Value): Unit = {
		currentStatus = value
		updateDisplay()
	}

	@JSExportTopLevel("init")
	def init(): Unit = {
		// jquery selectors
		val input = $("#input").get(0).get.asInstanceOf[htmlDom.TextArea]
		val ASTButton = $("#astbutton").get(0).get.asInstanceOf[htmlDom.Anchor]
		val IRButton = $("#irbutton").get(0).get.asInstanceOf[htmlDom.Anchor]

		println("init")

		input.onkeyup = { _: dom.KeyboardEvent => sourceModified(input) }
		input.onclick = { _: dom.MouseEvent => sourceModified(input) }

		def modelCallback(j: JQueryEventObject, a: Any): Any = {
			getShortlink(input.value).map(applyShortlink)
		}

		$("#shortLinkModel").on("shown.bs.modal", modelCallback _)

		ASTButton.onclick = { _: dom.MouseEvent => setActive(CurrentStatus.AST) }
		IRButton.onclick = { _: dom.MouseEvent => setActive(CurrentStatus.IR) }

		sourceModified(input)
	}

	private var currentStatus = CurrentStatus.AST
	private var lastInput = new String
	private var lastLineMap: rockstar.util.lineBreaks = _
	private var lastCharsNeeded: rockstar.util.charsNeeded = _
	private var lastError: Option[String] = None
	private var lastAst: Option[ast.Program] = None
	private var lastIR: Option[ir.Program] = None

	def updateDisplay(): Unit = {
		val output = $("#output")

		output.empty()
		if (lastError.isDefined) {
			output.append(lastError.get)
		}
		else {
			output.append(currentStatus match {
				case CurrentStatus.AST => printAST(lastAst.get, lastLineMap, lastCharsNeeded)
				case CurrentStatus.IR => Seq(div().render)
			})
		}
	}

	def sourceModified(element: htmlDom.TextArea): Unit = {
		println("sm")
		val currentText = element.value

		lastLineMap = rockstar.util.createLineBreaks(currentText)
		lastCharsNeeded = rockstar.util.findCharsNeeded(lastLineMap)

		$("#position").text(s"${rockstar.util.formatAsString(lastLineMap, lastCharsNeeded, element.selectionStart)}")

		if (currentText != lastInput) {
			lastAst = None
			lastIR = None
			lastError = None

			val time = compile(currentText) match {
				case CompileResult(Right(program), compTime) =>
					lastAst = Some(program)

					compTime

				case CompileResult(Left(CompileError(message, index)), compTime) => {
					val errorLoc = rockstar.util.findLinePair(lastLineMap, index)

					lastError = Some(s"Error. $message at pos ${errorLoc.line}:${errorLoc.char}")
					lastAst = None
					lastIR = None

					compTime
				}
			}

			$("#time-to-compile").text(s"${time}ms")
		}

		updateDisplay()

		lastInput = currentText
	}
}