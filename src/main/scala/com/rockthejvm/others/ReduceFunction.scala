package com.rockthejvm.others

object ReduceFunction extends App {
  private val listOfNumbers = List(1, 2, 3)

  private val maxNumber = listOfNumbers.reduce(_ max _)
  println(maxNumber) // 3

  private val reduceLeftValue = listOfNumbers.reduceLeft(_ - _)
  println(reduceLeftValue) // -4
  /*
    For reduceLeft the sequence is built up starting from the left,
    ((1 - 2) - 3)
    ((-1) - 3)
    (-4)
   */

  private val reduceRightValue = listOfNumbers.reduceRight(_ - _)
  println(reduceRightValue) // 2
  /*
    For reduceLeft the sequence is built up starting from the left,
    (1 - (2 - 3))
    (1 - (-1))
    2
   */

  private val emptyList = List[Int]()

  private val reduceOptionValue: Option[Int] = emptyList.reduceOption(_ + _)
  println(reduceOptionValue)

  private val reduceLeftOptionValue: Option[Int] = emptyList.reduceLeftOption(_ + _)
  println(reduceLeftOptionValue)
}
