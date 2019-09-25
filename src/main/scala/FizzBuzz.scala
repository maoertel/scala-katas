class FizzBuzz {

  def isDividable(dividend: Int)(divisor: Int): Boolean = dividend % divisor == 0

  def fizzBuzz(end: Int)(implicit rules: Seq[Int => String]): IndexedSeq[String] =
    (1 to end).map { index =>
      val step = rules.map(func => func(index)).mkString
      if (step.isEmpty) index.toString else step
    }

  def fizzBuzz2(end: Int = 100)(implicit rules: Seq[Int => String]): IndexedSeq[String] = for {
    index <- 1 to end
    step <- rules.map(rule => rule(index)).mkString :: Nil
  } yield if (step.isEmpty) index.toString else step

  def inCaseOf(cond: Boolean)(elem: String): String = if (cond) elem else ""

}

object FizzBuzz extends FizzBuzz {

  implicit val fizzBuzzRules: Seq[Int => String] = Seq(
    { num => inCaseOf(isDividable(dividend = num)(divisor = 3))("Fizz") },
    { num => inCaseOf(isDividable(dividend = num)(divisor = 5))("Buzz") },
  )

  implicit val fizzBuzzRulesExtended: Seq[Int => String] = fizzBuzzRules ++ Seq(
    { num => (num.toString map { case digit: Char => inCaseOf(digit == '3')("Fizz") }).mkString },
    { num => (num.toString map { case digit: Char => inCaseOf(digit == '5')("Buzz") }).mkString },
  )

}

object Main extends App {

  import FizzBuzz.fizzBuzzRulesExtended

  print(FizzBuzz fizzBuzz 100)
  print(FizzBuzz fizzBuzz2 200)

}