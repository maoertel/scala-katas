class FizzBuzz {

  def fizzBuzz(numbers: Seq[Int])(implicit rules: Seq[Int => String]): Seq[String] = for {
    index <- numbers
    step <- rules.map(rule => rule(index)).mkString :: Nil
  } yield if (step.isEmpty) index.toString else step

  def inCaseOf[T](elem: T)(cond: T => Boolean)(put: String): String = if (cond(elem)) put else ""

}

object FizzBuzz extends FizzBuzz {

  implicit val fizzBuzzRules: Seq[Int => String] = Seq(
    { num => inCaseOf(num)(_ % 3 == 0)(put = "Fizz") },
    { num => inCaseOf(num)(_ % 5 == 0)(put = "Buzz") },
  )

  implicit val fizzBuzzRulesExtended: Seq[Int => String] = fizzBuzzRules ++ Seq(
    { num => (num.toString map { case digit: Char => inCaseOf(digit)(_ == '3')(put = "Fizz") }).mkString },
    { num => (num.toString map { case digit: Char => inCaseOf(digit)(_ == '5')(put = "Buzz") }).mkString },
  )

}

object Main extends App {

  import FizzBuzz.fizzBuzzRulesExtended

  print(FizzBuzz fizzBuzz (1 to 100))
  print(FizzBuzz fizzBuzz (1 to 200))

}