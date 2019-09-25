import org.scalatest.{MustMatchers, WordSpec}

class FizzBuzzTest extends WordSpec with MustMatchers {

  "FizzBuzz.fizzBuzz" must {

    "match FizzBuzz base rules" in {
      import FizzBuzz.fizzBuzzRules
      val result = FizzBuzz.fizzBuzz(1 to 15)
      result.head must be("1")
      result(2) must be("Fizz")
      result(4) must be("Buzz")
      result(14) must be("FizzBuzz")
    }

    "match FizzBuzz extended rules" in {
      import FizzBuzz.fizzBuzzRulesExtended
      val result = FizzBuzz.fizzBuzz(1 to 100)
      result.head must be("1")
      result(2) must be("FizzFizz")
      result(4) must be("BuzzBuzz")
      result(14) must be("FizzBuzzBuzz")
      result(29) must be("FizzBuzzFizz")
      result(32) must be("FizzFizzFizz")
      result(34) must be("BuzzFizzBuzz")
      result(74) must be("FizzBuzzBuzz")
    }

  }

}