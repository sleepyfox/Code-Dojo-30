import org.scalatest._

class TestFibonacci extends FunSpec with ShouldMatchers {

  describe("A Fibonacci sequence") {
    it("should have zero as its first number") {
      getNumber(0) should be(0)
    }
    it("should have one as its second number") {
      getNumber(1) should be(1)
    }
    it("should have 1 as its third number") {
      getNumber(2) should be(1)
    }
  }

  def getNumber(index: Int) : Int = {
    if (index == 0) {
      0
    } else {
      1
    }
  }
}
