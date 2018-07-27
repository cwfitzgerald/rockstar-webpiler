package rockstar

import scala.annotation.tailrec

object util {
	case class linePair(line: Int, char: Int)
	case class lineBreaks(breakIndexes: Vector[Int])
	case class charsNeeded(line: Int, char: Int)

	def createLineBreaks(input: String): lineBreaks = {
		@tailrec
		def impl(retVal: Vector[Int], inputVector: Vector[Char], idx: Int): Vector[Int] = {
			if (inputVector.nonEmpty)
				impl(if (inputVector.head == '\n') retVal :+ (idx + 1) else retVal, inputVector.tail, idx + 1 )
			else
				retVal
		}

		lineBreaks(0 +: impl(Vector[Int](), input.toVector, 0) :+ input.length - 1)
	}

	def findLinePair(breaks: lineBreaks, idx: Int): linePair = {
		val line = breaks.breakIndexes.zipWithIndex.reverse.find({ case (i, _) => i <= idx }).map(_._2).get

		linePair(line, idx - breaks.breakIndexes(line))
	}

	def findCharsNeeded(breaks: lineBreaks): charsNeeded = {
		val lbList = breaks.breakIndexes

		val lineCount = lbList.size
		val longestLine = lbList.zip(lbList.drop(1)).map( {case (a, b) => b - a} ).max

		charsNeeded(math.ceil(math.log10(lineCount)).toInt, math.ceil(math.log10(longestLine)).toInt)
	}

	def formatAsString(breaks: lineBreaks, cn: charsNeeded, idx: Int): String = {
		val lp = findLinePair(breaks, idx)

		val line = lp.line.toString
		val char = lp.char.toString

		val lineString =
			if (line.length < cn.line)
				line.reverse.padTo(cn.line, '0').reverse
			else
				line
		val charString =
			if (char.length < cn.char)
				char.reverse.padTo(cn.char, '0').reverse
			else
				char

		s"$lineString:$charString"
	}
}
