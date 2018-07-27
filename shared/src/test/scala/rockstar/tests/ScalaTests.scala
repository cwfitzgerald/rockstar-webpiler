package rockstar.tests

import fastparse.core.Parsed.{Failure, Success}
import utest._

object ScalaTests extends TestSuite {
	val tests = Tests {
		'construct - {
			rockstar.parser()
		}

		'random - {
			rockstar.parser("Val is 3") match {
				case Success(value, index) => println(index); assert(true)
				case Failure(value, index, bt) => println(bt.traced.fullStack); println(index); assert(false)
			}
		}

		'unidiomatic - {
			val input =
				"""|Modulus takes Number and Divisor
				   |While Number is as high as Divisor
				   |Put Number minus Divisor into Number
				   |    (blank line ending While block)
				   |Give back Number
				   |    (blank line ending function declaration)
				   |Limit is 100
				   |Counter is 0
				   |Fizz is 3
				   |Buzz is 5
				   |Until Counter is Limit
				   |Build Counter up
				   |If Modulus taking Counter, Fizz is 0 and Modulus taking Counter, Buzz is 0
				   |Say "FizzBuzz!"
				   |Continue
				   |    (blank line ending 'If' Block)
				   |If Modulus taking Counter and Fizz is 0
				   |Say "Fizz!"
				   |Continue
				   |    (blank line ending 'If' Block)
				   |If Modulus taking Counter and Buzz is 0
				   |Say "Buzz!"
				   |Continue
				   |    (blank line ending 'If' Block)
				   |Say Counter
				   |    (EOL ending Until block)
				""".stripMargin
			rockstar.parser(input) match {
				case Success(value, index) => println(input.length); println(index); assert(true)
				case Failure(value, index, bt) => println(input.length); println(bt.traced.fullStack); println(index); assert(false)
			}
		}

		'idiomatic - {
			val input =
				"""|Midnight takes your heart and your soul
				   |While your heart is as high as your soul
				   |Put your heart without your soul into your heart
				   |
				   |Give back your heart
				   |
				   |
				   |Desire is a lovestruck ladykiller
				   |My world is nothing
				   |Fire is ice
				   |Hate is water
				   |Until my world is Desire,
				   |Build my world up
				   |If Midnight taking my world, Fire is nothing and Midnight taking my world, Hate is nothing
				   |Shout "FizzBuzz!"
				   |Take it to the top
				   |
				   |If Midnight taking my world, Fire is nothing
				   |Shout "Fizz!"
				   |Take it to the top
				   |
				   |If Midnight taking my world, Hate is nothing
				   |Say "Buzz!"
				   |Take it to the top
				   |
				   |Whisper my world
				""".stripMargin
			rockstar.parser(input) match {
				case Success(value, index) => println(input.length); println(index); assert(true)
				case Failure(value, index, bt) => println(input.length); println(bt.traced.fullStack); println(index); assert(false)
			}
		}
	}
}
