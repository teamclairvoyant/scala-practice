package com.rockthejvm.others

object FoldFunction extends App {

  val listOfNumbers = List(1, 2, 3, 4, 5)

  // fold
  private val foldValue = listOfNumbers.fold(0)((accumulator, operand) => accumulator + operand)
  println(foldValue) // 15

  // foldLeft
  private case class Person(name: String, sex: String)

  private val persons = List(Person("Thomas", "male"), Person("Sowell", "male"), Person("Liz", "female"))

  private val foldLeftList =
    persons.foldLeft(List[String]()) { (accumulator, person) =>
      val title =
        person.sex match {
          case "male" =>
            "Mr."
          case "female" =>
            "Ms."
        }
      accumulator :+ s"$title ${person.name}"
    }

  println(foldLeftList) // List(Mr. Thomas, Mr. Sowell, Ms. Liz)

  // foldRight
  private val foldRightList =
    persons.foldRight(List[String]()) { (person, accumulator) =>
      val title =
        person.sex match {
          case "male" =>
            "Mr."
          case "female" =>
            "Ms."
        }
      accumulator :+ s"$title ${person.name}"
    }

  println(foldRightList) // List(Ms. Liz, Mr. Sowell, Mr. Thomas)
}
