package com.rockthejvm.lists

object LazyListExample extends App {

  // Fibonacci
  def fibs(current: Int = 0, next: Int = 1): LazyList[Int] = LazyList.cons(current, fibs(next, current + next))

  def calculateFibsFor(position: Int): Int = fibs().take(position + 1).last

  val position: Int = 12
  println(s"The fibonacci for f($position) = ${calculateFibsFor(position)}")
}
