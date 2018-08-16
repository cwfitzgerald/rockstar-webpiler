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

		'algorithm - {
			val input = """
			  |
			  |my heart is inadequate
			  |my desire is overcomplex
			  |my thirst is unquenchable
			  |
			  |Scottie was all mysterious
			  |Life is on fire
			  |
			  |Liftin High takes the spirit and Greatness
			  |Davy is without
			  |Gina is outdazzling
			  |While Davy ain't Greatness
			  |Put the spirit of Gina into Gina
			  |Build Davy up
			  |
			  |Give back Gina
			  |
			  |
			  |Show Me The Way takes my love
			  |Existence is inadequate
			  |If my love is less than my thirst
			  |Give back Existence
			  |
			  |While Liftin High taking my thirst,Existence is weaker than my love with my desire
			  |Build Existence up
			  |
			  |Knock Existence down
			  |Give back Existence
			  |
			  |
			  |Truth Of The Now takes my time and your place
			  |If your place is lower than my heart
			  |Give back my heart
			  |
			  |While my time is higher than my heart
			  |Truth is mysterious
			  |Put Show Me The Way taking my time into Truth
			  |If your place is higher than Truth
			  |Give back my heart
			  |
			  |If your place is Truth
			  |Give back my desire
			  |
			  |Put my time without Liftin High taking my thirst,Truth into my time
			  |
			  |Give back my heart
			  |
			  |Truth Of The World takes the soul and the singer
			  |Put "" into the song
			  |Put the singer without my desire into the stage
			  |While the stage is higher than my heart without my desire
			  |If Truth Of The Now taking the soul,the stage
			  |Put the song with "*" into the song
			  |
			  |If Truth Of The Now taking the soul, the stage ain't true
			  |Put the song with "_" into the song
			  |
			  |Knock the stage down
			  |
			  |Scream the song
			  |
			  |Livin Life takes my cool and my calm and my need and my breath
			  |your voice is jeopardizing
			  |your lovin is stupendiafying
			  |Put my cool with your voice of my calm with your lovin of my need into a moment
			  |If Truth Of The Now taking my breath, a moment
			  |Give back my desire
			  |
			  |Give back my heart
			  |
			  |Livin Large takes my time and my money and the way
			  |Life is mysterious
			  |While my money ain't my heart without my desire
			  |Knock my money down
			  |Put Truth Of The Now taking my time, my money into my cool
			  |Build my money up
			  |Put Truth Of The Now taking my time, my money into my calm
			  |Build my money up
			  |Put Truth Of The Now taking my time, my money into my need
			  |Knock my money down
			  |Put Livin Life taking my cool, my calm, my need, the way into your life
			  |Put Life with your life of Liftin High taking my thirst, my money into Life
			  |Knock my money down
			  |
			  |Give back Life
			  |
			  |Rock Remembrance takes a man and a lifetime
			  |a chance is up
			  |the soul is volcanizing
			  |Put a chance of a lifetime with the soul into the beat
			  |Put Liftin High taking a chance,a lifetime into a story
			  |While a lifetime ain't nothing
			  |Truth Of The World taking a story,the beat
			  |Put Livin Large taking a story,the beat,a man into a story
			  |Knock a lifetime down
			  |
			  |
			  |Rock Remembrance taking Scottie, Life
			""".stripMargin

			rockstar.parser(input) match {
				case Success(value, index) => println(input.length); println(index); assert(true)
				case Failure(value, index, bt) => println(input.length); println(bt.traced.fullStack); println(index); assert(false)
			}
		}

		'IRtests - {
			val input =
				"""Put "x" into my x
				  |Shout my x with my x
				  |Build my x up
				  |Build my x up""".stripMargin

			val parsed = rockstar.parser(input).get.value

			val FromAst = rockstar.ir.FromAst(parsed)
		}
	}
}
