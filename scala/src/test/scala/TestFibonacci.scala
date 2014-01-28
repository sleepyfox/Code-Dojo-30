import org.scalatest._
import codedojo.Fibonacci._

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

  describe("A Zeckendorf filter") {
    it("should not filter out '1'") {
      isZeckendorf("1") should be(true)
    }
    it("should not filter out '0'") {
      isZeckendorf("0") should be(true)
    }
    it("should filter out '11'") {
      isZeckendorf("11") should be(false)
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
          isZeckendorf("Hi!")
        }
      }
      it("'Hi!0101' should throw IllegalArgumentException") {
        intercept[IllegalArgumentException] {
          isZeckendorf("Hi!0101")
        }
      }
    }
  }

  describe("The Zeckendorf representations for") {
    it("1 should be '1' only") {
      fibonacciEncode(1).filter(isZeckendorf) should be(Set("1"))
    }
    it("2 should be '10' only") {
      fibonacciEncode(2).filter(isZeckendorf) should be(Set("10"))
    }
    it("3 should be '100' only and not '11'") {
      fibonacciEncode(3).filter(isZeckendorf) should be(Set("100"))
    }
    it("8 should be '10000' only and not '1100' or '1011'") {
      fibonacciEncode(8).filter(isZeckendorf) should be(Set("10000"))
    }
  }

  (0 to 20).map( x => {
    println(x, fibonacciEncode(x).filter(isZeckendorf))
  })

}
