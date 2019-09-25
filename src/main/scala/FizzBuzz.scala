

class FizzBuzz {

  def fizzBuzz(numbers: Seq[Int])(implicit rules: Seq[Int => String]): Seq[String] = for {
    index <- numbers
    step <- rules.map(rule => rule(index)).mkString :: Nil
  } yield if (step.isEmpty) index.toString else step

  def dividableBy(divisor: Int)(put: String): Int => String = {
    num: Int => inCaseOf(num)(_ % divisor == 0)(put = put)
  }

  def digitIs(digitC: Char)(put: String): Int => String = {
    num: Int => (num.toString map { case digit: Char => inCaseOf(digit)(_ == digitC)(put = put) }).mkString
  }

  private def inCaseOf[T](elem: T)(cond: T => Boolean)(put: => String): String = if (cond(elem)) put else ""

}

object FizzBuzz extends FizzBuzz {

  implicit val fizzBuzzRules: Seq[Int => String] = Seq(
    dividableBy(3)(put = "Fizz"),
    dividableBy(5)(put = "Buzz")
  )

  implicit val fizzBuzzRulesExtended: Seq[Int => String] = fizzBuzzRules ++ Seq(
    digitIs('3')("Fizz"),
    digitIs('5')("Buzz")
  )

}

object Main extends App {

  import FizzBuzz.fizzBuzzRulesExtended

  print(FizzBuzz fizzBuzz (1 to 100))

}