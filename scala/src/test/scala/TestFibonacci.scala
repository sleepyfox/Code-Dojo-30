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
      fibonacciEncode(3).contains("11") should be(true)
    }
  }

  // 0, 1, 1, 2, 3, 5
  // 1, 2, 3, 5, 8
  // 0 0 0 1

  describe("A variant finder") {
    it("should not find any variants of '1'") {
      findCodeVariants(Set.empty, "1") should have size(1)
    }
    it("should return a set with the original string for input '1'") {
      findCodeVariants(Set.empty, "1").contains("1") should be(true)
    }
    it("should find one additional variant for '100'") {
      findCodeVariants(Set.empty, "100") should have size(2)
    }
    it("should find no additional variants for '101'") {
      findCodeVariants(Set.empty, "101") should have size(1)
    }
    it("should find one additional variants for '1000'") {
      findCodeVariants(Set.empty, "1000") should have size(2)
    }
    it("should find additional variant '110' for '1000'") {
      findCodeVariants(Set.empty, "1000").contains("110") should be(true)
    }
    it("should find 2 additional variants for '10000'") {
      findCodeVariants(Set.empty, "10000") should have size(3)
    }
    it("should find variant for '10000' of '1100'") {
      findCodeVariants(Set.empty, "10000").contains("1100") should be(true)
    }
    it("should find variant for '10000' of '1011'") {
      findCodeVariants(Set.empty, "10000").contains("1011") should be(true)
    }
  }

  def findCodeVariants(stack: Set[String], code: String) : Set[String] = stack match {
    case x if x.isEmpty => findCodeVariants(Set(code), code)
    case _ => {
      def produceSingleVariant(code: String) : String = {
        if (code.startsWith("100")) {
          code.replaceFirst("100", "11")
        } else {
          code.replaceFirst("100", "011")
        }
      }

      def hasVariant(code : String) : Boolean = code.contains("100")

      if (hasVariant(code)) {
        val newCode = produceSingleVariant(code)
        findCodeVariants(stack + newCode, newCode)
      } else {
        stack
      }
    }
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

      findCodeVariants(Set.empty, code)
    }
    // loop from 1 to index-1 and return string
  }

  describe("A Zeckendorf filter") {
    it("should not filter out 1") {
      isZeckendorf("1") should be(true)
    }
    it("should not filter out 0") {
      isZeckendorf("0") should be(true)
    }

    describe("with an empty string") {
      it("should throw IllegalArgumentException") {
        intercept[IllegalArgumentException] {
          isZeckendorf("")
        }
      }
    }
    describe("with a non-binary string") {
      it("'Hi!' should throw IllegalArgumentException") {
        intercept[IllegalArgumentException] {
          isZeckendorf("Hi!010101")
        }
      }
    }
  }

  def isZeckendorf(code : String) : Boolean = {
    if (code.isEmpty || !code.matches("[01]+"))
      throw new IllegalArgumentException
    else
      true
  }
}
