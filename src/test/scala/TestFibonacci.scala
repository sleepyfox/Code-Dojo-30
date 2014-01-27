import org.scalatest._

class TestFibonacci extends FunSpec with ShouldMatchers {

  describe("A Fibonacci sequence") {
    it("should have zero as its first number") {
      val fibonacci = new Fibonacci
      fibonacci.getNumber(0) should be(0)
    }
    it("should have one as its second number") {
      val fibonacci = new Fibonacci
      fibonacci.getNumber(1) should be(1)
    }
  }
}

class Fibonacci {
  def getNumber(index: Int) : Int = {
    0
  }
}
