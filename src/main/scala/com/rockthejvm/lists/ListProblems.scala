package com.rockthejvm.lists

import scala.annotation.tailrec

abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()

  override def tail: RList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringHelper(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty)
        result
      else if (remaining.tail.isEmpty)
        s"$result${remaining.head}"
      else
        toStringHelper(remaining.tail, s"$result${remaining.head}, ")
    }

    s"[${toStringHelper(this, "")}]"
  }

  // Complexity: O(min(N, index)
  override def apply(index: Int): T = {
    @tailrec
    def applyHelper(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index)
        remaining.head
      else
        applyHelper(remaining.tail, currentIndex + 1)
    }

    if (index < 0)
      throw new NoSuchElementException()
    else
      applyHelper(this, 0)
  }

  // Complexity: O(N)
  override def length: Int = {
    @tailrec
    def lengthHelper(list: RList[T], accumulator: Int): Int = {
      if (list.isEmpty)
        accumulator
      else
        lengthHelper(list.tail, accumulator + 1)
    }

    lengthHelper(this, 0)
  }

  // Complexity: O(N)
  override def reverse: RList[T] = {
    @tailrec
    def reverseListHelper(list: RList[T], accumulator: RList[T]): RList[T] = {
      if (list.isEmpty)
        accumulator
      else
        reverseListHelper(list.tail, list.head :: accumulator)
    }

    reverseListHelper(this, RNil)
  }

  // Complexity: O(M + N), where M and N are the length of respective lists
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatHelper(remainingList: RList[S], accumulator: RList[S]): RList[S] = {
      if (remainingList.isEmpty)
        accumulator
      else
        concatHelper(remainingList.tail, remainingList.head :: accumulator)
    }

    concatHelper(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtHelper(currentIndex: Int, left: RList[T], right: RList[T]): RList[T] = {
      if (currentIndex == index)
        left.reverse ++ right.tail
      else
        removeAtHelper(currentIndex + 1, right.head :: left, right.tail)
    }

    if (index < 0)
      this
    else
      removeAtHelper(0, RNil, this)
  }

  // Complexity: O(N)
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapHelper(remainingList: RList[T], accumulator: RList[S]): RList[S] = {
      if (remainingList.isEmpty)
        accumulator.reverse
      else
        mapHelper(remainingList.tail, f(remainingList.head) :: accumulator)
    }

    mapHelper(this, RNil)
  }

}

object ListProblems extends App {
  val list = 1 :: 2 :: 3 :: 9 :: 8 :: 7 :: RNil

  println(list) // [1, 2, 3, 9, 8, 7]

  println(list(0)) // 1
  println(list(1)) // 2
  println(list(2)) // 3

  println(list.length) // 6

  println(list.reverse) // [7, 8, 9, 3, 2, 1]

  val list1 = 1 :: 2 :: 3 :: RNil
  val list2 = 4 :: 5 :: 6 :: RNil

  println(list1 ++ list2) // [1, 2, 3, 4, 5, 6]

  println(list.removeAt(0)) // [2, 3, 9, 8, 7]
  println(list.removeAt(1)) // [1, 3, 9, 8, 7]
  println(list.removeAt(2)) // [1, 2, 9, 8, 7]

  println(list.map(_ * 2)) // [2, 4, 6, 18, 16, 14]
}
