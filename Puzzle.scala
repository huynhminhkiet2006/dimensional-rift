abstract class Puzzle

class LibraryPuzzle(
            val question: String,
            val answerList: Vector[String],
            val correctSequence: Vector[Int],
            val reward: Item) extends Puzzle:

  var checkIfSolved = false

  def displayPuzzle() =
      println(question)
      for i <- answerList.indices do
        println(s"${i + 1}. ${answerList(i)}")
      println("Enter the correct sequence of numbers separated by spaces (e.g., 1 3 2):")

  def checkAnswer(playerInput: String) =
    val inputSequence = playerInput.split(" ").map(_.toInt).toVector
    val correct = inputSequence == correctSequence
    if correct then checkIfSolved = true
    correct

val archivePuzzle = new LibraryPuzzle(
  "A story is told in whispers between the first breath and the final sigh. " +
    "\nWhat speaks first in the silence?\n" +
    "\nHint: The start is not always the first step; endings may come before understanding.",
  Vector(
  "The Tome of Beginnings - Where all things rise, though not all things start.",
  "The Book of Silence - In the quiet before time stirs.",
  "The Final Record - A last mark upon a fading page."
  ),
  Vector(2, 3, 1),
  archiveKeyItem
)

val chamberPuzzle = new LibraryPuzzle(
  "Time coils around itself, memory unlocks what is forgotten, and only then can the past slip away. " +
    "\nWhich hand moves first?\n" +
    "\nHint: What is held close must loosen before time flows, and only then may one leave.",
  Vector(
    "The Locket of Memory - What is sealed must be freed.",
    "The Hourglass of Time - It turns, but only after something is set in motion.",
    "The Key of Departure - The gate is opened when the forgotten is remembered.",
  ),
  Vector(1, 2, 3),
  chamberKeyItem
)

val sanctumPuzzle = new LibraryPuzzle(
  " Echoes ripple through time, carried by voices unknown. Fear stirs before sorrow speaks, yet the final word remains unheard. " +
    "\nWhich voice fades last?\n" +
    "\nHint: What is held close must loosen before time flows, and only then may one leave.",
  Vector(
    "The Whisper of Fear - That which trembles in the darkness.",
    "The Cry of Sorrow - A sound born of loss, it follows where fire once burned.",
    "The Echo of Acceptance - In the end, all echoes are stilled."
  ),
  Vector(1, 2, 3),
  sanctumKeyItem
)
