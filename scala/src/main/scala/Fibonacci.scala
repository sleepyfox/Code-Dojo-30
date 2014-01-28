package codedojo {

  object Fibonacci {
    def fibonacci(index: Int) : Int = index match {
      case x if x < 0 => throw new IndexOutOfBoundsException
      case 0 => 0
      case 1 => 1
      case x => fibonacci(x - 1) + fibonacci(x - 2)
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

    private def biggestFibonacciIndex(number: Int) : Int = {
      var index = 2
      while(fibonacci(index) <= number) index += 1
      index - 2
    }

    private def makeCodeList(rep : List[Int], remainder : Int, digit : Int) : List[Int] = remainder match {
      case 0 => return rep
      case _ => {
        var newRemainder = remainder
        var newRep = rep
        if (remainder >= fibonacci(digit)) {
          newRemainder = newRemainder - fibonacci(digit)
          newRep = rep.updated((rep.length - 1 - digit), 1)
        }
        makeCodeList(newRep, newRemainder, digit - 1)
      }
    }

    private def principalCode(number: Int) : String = {
      val index = biggestFibonacciIndex(number)
      makeCodeList(List.fill(index)(0), number, index).mkString
    }

    def fibonacciEncode(number: Int) : Set[String] = number match {
      case 0 => Set(number.toString)
      case 1 => Set(number.toString)
      case _ => {
        findCodeVariants(Set.empty, principalCode(number))
      }
    }

    def isZeckendorf(code : String) : Boolean = code match {
      case ""                       => throw new IllegalArgumentException
      case x if !x.matches("[01]+") => throw new IllegalArgumentException
      case x if x.contains("11")    => false
      case _                        => true
    }
  }
}
