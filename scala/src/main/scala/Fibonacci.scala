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

    def isZeckendorf(code : String) : Boolean = code match {
      case ""                       => throw new IllegalArgumentException
      case x if !x.matches("[01]+") => throw new IllegalArgumentException
      case x if x.contains("11")    => false
      case _                        => true
    }
  }
}
