package com.rockthejvm.others

object ZipFunction extends App {
  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6, 7, 8)
  val list3 = List()

  private val zippedList = list1.zip(list2)
  println(zippedList) // List((1,4), (2,5), (3,6))

  println(list3.zip(list2)) // List()

  private val unzippedList = zippedList.unzip
  println(unzippedList._1) // List(1, 2, 3)
  println(unzippedList._2) // List(4, 5, 6)
}
