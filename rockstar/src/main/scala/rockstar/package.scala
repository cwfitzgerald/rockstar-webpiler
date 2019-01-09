import fastparse.Parsed

package object rockstar {
	sealed case class StackTraceComponent(location: String, offset: Int)
	sealed case class StackTrace(msg: String, stackTrace: Seq[StackTraceComponent])

	sealed case class CompileResult(ast: Either[String, internals.ast.Program], duration: Int)
	sealed case class RunResult(output: String, errors: Seq[StackTrace])

	def generateAST(input: String, lineMap: util.lineBreaks, charsNeeded: util.charsNeeded): CompileResult = {
		val start = java.time.Instant.now().getNano
		val parseResult = internals.parser(input)
		val duration = (java.time.Instant.now().getNano - start) / 1000000

		parseResult match {
			case Parsed.Success(ast, _) =>
				CompileResult(Right(ast), duration)
			case Parsed.Failure(label, index, _) =>
				val error = s"Parse failed at ${util.formatAsString(lineMap, charsNeeded, index)}:\n\t$label"
				CompileResult(Left(error), duration)
		}
	}
}
