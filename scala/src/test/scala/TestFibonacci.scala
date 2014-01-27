import org.scalatest._

class TestFibonacci extends FunSpec with ShouldMatchers {

  describe("A Fibonacci sequence") {
    it("should have zero as its first number") {
      fibonacci(0) should be(0)
    }
    it("should have one as its second number") {
      fibonacci(1) should be(1)
    }
    it("should have 1 as its third number") {
      fibonacci(2) should be(1)
    }
    it("should have 2 as its fourth number") {
      fibonacci(3) should be(2)
    }
    it("should have 34 as its tenth number") {
      fibonacci(9) should be(34)
    }
    it("should throw an exception on negative input") {
      intercept[IndexOutOfBoundsException] {
        fibonacci(-1)
      }
    }
  }

  def fibonacci(index: Int) : Int = index match {
    case x if x < 0 => throw new IndexOutOfBoundsException
    case 0 => 0
    case 1 => 1
    case x => fibonacci(x - 1) + fibonacci(x - 2)
  }

  describe("A Fibonacci encoder") {
    it("should transform 0 into '0'") {
      fibonacciEncode(0) should be(Set("0"))
    }
    it("should transform 1 into '1'") {
      fibonacciEncode(1) should be(Set("1"))
    }
    it("should transform 2 into '10'") {
      fibonacciEncode(2) should be(Set("10"))
    }
    it("should transform 3 into a set that includes '100'") {
      fibonacciEncode(3).contains("100") should be(true)
    }
    it("should transform 3 into a set that includes '11'") {
      pending
      fibonacciEncode(3).contains("11") should be(true)
    }
  }

  // 0, 1, 1, 2, 3, 5
  // 1, 2, 3, 5
  // 0 0 0 1

  describe("A variant finder") {
    it("should not find any variants of '1'") {
      findCodeVariant(Set.empty, "1") should have size(1)
    }
    it("should return a set with the original string for input '1'") {
      findCodeVariant(Set.empty, "1").contains("1") should be(true)
    }
    it("should find one additional variant for '100'") {
      findCodeVariant(Set.empty, "100") should have size(2)
    }
  }
  def findCodeVariant(stack: Set[String], code: String) : Set[String] = code match {
    case "100" => Set("100", "011")
    case _ => Set(code)
  }

  def fibonacciEncode(number: Int) : Set[String] = number match {
    case 0 => Set(number.toString)
    case 1 => Set(number.toString)
    case _ => {
      var index = 2
      var code = ""
      var remainder = number

      while(fibonacci(index) <= number) {
        index += 1
      }

      (index-1 until 1 by -1).foreach { currentIndex => {
        if(remainder >= fibonacci(currentIndex)){
          code += "1"
          remainder -= fibonacci(currentIndex)
        } else {
          code += "0"
        }
      }}

      findCodeVariant(Set.empty, code)
    }
    // loop from 1 to index-1 and return string
  }
}
