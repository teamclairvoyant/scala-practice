package com.rockthejvm.others

object EitherLeftRightExample extends App {

  private def divideXByY(x: Int, y: Int): Either[String, Int] = {
    if (y == 0)
      Left("Dude, can't divide by 0")
    else
      Right(x / y)
  }

  divideXByY(1, 0) match {
    case Left(error) =>
      println(s"Error: $error")
    case Right(d) =>
      println(s"Answer: $d")
  }
  // Output --> Error: Dude, can't divide by 0

}
