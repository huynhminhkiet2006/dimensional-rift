import scala.io.StdIn.*
import scala.util.{Failure, Success, Try}

class GameAction {

  def pressEnter() =
    println("\nPress Enter to continue...")
    readLine()
    println("")

  def getInputNumber(prompt: String, validChoices: Vector[Int]): Int =

    var valid = false
    var choice = -1

    while (!valid) do
      print(prompt)

      val input = Try(readInt()) match
        case Success(num) => num
        case Failure(_) =>
          println("Invalid input. Please enter a valid number.")
          -1

      if (validChoices.contains(input)) then
        choice = input
        valid = true
      else
        println(s"Invalid choice. Please select from: ${validChoices.mkString(", ")}")

    choice

  def getInputNumberNoPrinting(validChoices: Vector[Int]): Int =

    var valid = false
    var choice = -1
    print("Enter number: ")

    while (!valid) do

      val input = Try(readInt()) match
        case Success(num) => num
        case Failure(_) =>
          -1

      if (validChoices.contains(input)) then
        choice = input
        valid = true
      else
        println(s"Invalid choice. Please select from: ${validChoices.mkString(", ")}")

    choice
}
