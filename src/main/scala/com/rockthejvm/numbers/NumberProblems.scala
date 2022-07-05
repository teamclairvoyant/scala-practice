package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberOps {

  implicit class RichInt(n: Int) {

    /*
            isPrime(11) = isPrimeHelper(2)
              Math.sqrt(Math.abs(11)) = 3.3
            = if(2 > 3.3) -> false
            = if(11 % 2 == 0) -> false
            = isPrimeHelper(3)
            = if(3 > 3.3) -> false
            = if(11 % 3 == 0) -> false
            = isPrimeHelper(4)
            = if(4 > 3.3) -> true
            = true

            isPrime(15) = isPrimeHelper(2)
              Math.sqrt(Math.abs(11)) = 3.8
            = if(2 > 3.8) -> false
            = if(15 % 2 == 0) -> false
            = isPrimeHelper(3)
            = if(3 > 3.8) -> false
            = if(15 % 3 == 0) -> true
            = false

            Complexity: O(sqrt(N))
     */
    def isPrime: Boolean = {
      @tailrec
      def isPrimeHelper(currentDivisor: Int): Boolean = {
        if (currentDivisor > Math.sqrt(Math.abs(n)))
          true
        else if (n % currentDivisor == 0)
          false
        else
          isPrimeHelper(currentDivisor + 1)
      }

      if (n == -1 || n == 0 || n == 1)
        false
      else
        isPrimeHelper(2)
    }

    /*
                      decomposeIntoPrimeFactors(11)
                            = decomposeIntoPrimeFactorsHelper(11, 2, [])
                              Math.sqrt(11) = 3.3
                              if(2 > 3.3) -> false
                              if(11 % 2 == 0) -> false
                            = decomposeIntoPrimeFactorsHelper(11, 3, [])
                              if(3 > 3.3) -> false
                              if(11 % 3 == 0) -> false
                            = decomposeIntoPrimeFactorsHelper(11, 4, [])
                              if(4 > 3.3) -> true
                            = 11 :: []
                            = [11]

                      decomposeIntoPrimeFactors(15)
                            = decomposeIntoPrimeFactorsHelper(15, 2, [])
                              Math.sqrt(15) = 3.8
                              if(2 > 3.8) -> false
                              if(15 % 2 == 0) -> false
                            = decomposeIntoPrimeFactorsHelper(15, 3, [])
                              if(3 > 3.8) -> false
                              if(15 % 3 == 0) -> true
                            = decomposeIntoPrimeFactorsHelper(5, 3, [3])
                              Math.sqrt(5) = 2.2
                              if(3 > 2.2) -> true
                            = 5 :: [3]
                            = [5, 3]

                      decomposeIntoPrimeFactors(16)
                            = decomposeIntoPrimeFactorsHelper(16, 2, [])
                              Math.sqrt(16) = 4
                              if(2 > 4) -> false
                              if(16 % 2 == 0) -> true
                            = decomposeIntoPrimeFactorsHelper(8, 2, [2])
                              Math.sqrt(8) = 2.8
                              if(2 > 2.8) -> false
                              if(8 % 2 == 0) -> true
                            = decomposeIntoPrimeFactorsHelper(4, 2, [2,2])
                              Math.sqrt(4) = 2
                              if(2 > 2) -> false
                              if(4 % 2 == 0) -> true
                            = decomposeIntoPrimeFactorsHelper(2, 2, [2,2,2])
                              Math.sqrt(4) = 1.4
                              if(2 > 1.4) -> true
                            = 2 :: [2,2,2]
                            = [2,2,2,2]

                      Complexity: O(sqrt(N)); can be as low as: O(log(N))
     */
    def decomposeIntoPrimeFactors: List[Int] = {
      assert(n >= 0)

      @tailrec
      def decomposeIntoPrimeFactorsHelper(remaining: Int, currentDivisor: Int, accumulator: List[Int]): List[Int] = {
        if (currentDivisor > Math.sqrt(remaining))
          remaining :: accumulator
        else if (remaining % currentDivisor == 0)
          decomposeIntoPrimeFactorsHelper(remaining / currentDivisor, currentDivisor, currentDivisor :: accumulator)
        else
          decomposeIntoPrimeFactorsHelper(remaining, currentDivisor + 1, accumulator)
      }

      decomposeIntoPrimeFactorsHelper(n, 2, List())
    }

  }

}

object NumberProblems extends App {
  import NumberOps.*

  println(2.isPrime) // true
  println(15.isPrime) // false
  println(2003.isPrime) // true
  println(2731189.isPrime) // true
  println(517935871.isPrime) // false
  println(1.isPrime) // false
  println(0.isPrime) // false
  println(-2003.isPrime) // true

  println(2.decomposeIntoPrimeFactors) // List(2)
  println(15.decomposeIntoPrimeFactors) // List(5, 3)
  println(2003.decomposeIntoPrimeFactors) // List(2003)
  println(2731189.decomposeIntoPrimeFactors) // List(2731189)
  println(517935871.decomposeIntoPrimeFactors) // List(53611, 9661)
  println(1.decomposeIntoPrimeFactors) // List(1)
  println(0.decomposeIntoPrimeFactors) // List(0)
  println(16.decomposeIntoPrimeFactors) // List(2, 2, 2, 2)
}
