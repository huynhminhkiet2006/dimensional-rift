abstract class Puzzle

//Library
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
      println("\nEnter the correct sequence of numbers separated by spaces (e.g., 1 3 2):")

  def checkAnswer(playerInput: String) =
    val inputSequence = playerInput.split(" ").map(_.toInt).toVector
    val correct = inputSequence == correctSequence
    if correct then checkIfSolved = true
    correct

val archivePuzzle = new LibraryPuzzle(
  "A story is told in whispers between the first breath and the final sigh. " +
    "\nWhat speaks first in the silence?\n" +
    "\nHint: The start is not always the first step; endings may come before understanding.\n",
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
    "\nHint: What is held close must loosen before time flows, and only then may one leave.\n",
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
    "\nHint: What trembles comes first, though sorrow often lingers before all fades to quiet.\n",
  Vector(
    "The Whisper of Fear - That which trembles in the darkness.",
    "The Cry of Sorrow - A sound born of loss, it follows where fire once burned.",
    "The Echo of Acceptance - In the end, all echoes are stilled."
  ),
  Vector(1, 2, 3),
  sanctumKeyItem
)

//Cavern
class CavernPuzzle(
                  val question: String,
                  val answerList: Vector[String],
                  val correctAnswer: Int,
                  val reward: Item) extends Puzzle:

  var checkIfSolved = false

  def displayPuzzle() =
      println(question)
      for i <- answerList.indices do
        println(s"${i + 1}. ${answerList(i)}")
      println("\nEnter the correct answer:")

  def checkAnswer(input: Int) =
    val correct = input == correctAnswer
    if correct then checkIfSolved = true
    correct


val abyssPuzzle = new CavernPuzzle(
  "The path is lit, but the light itself is in a trance. Which shadow follows the pulse of the truth?\n" +
    "Hint: Reflection is more than an image.",
  Vector(
    "Shadow of Time",
    "Shadow of Silence",
    "Shadow of Reflection",
  ),
  3,
  abyssFragment
)

val grottoPuzzle = new CavernPuzzle(
  "A puzzle fragmented by time, yet bound by eternal threads. Which piece finds its true place?\n" +
    "Hint: The past endures beyond time.",
  Vector(
    "Piece of Time",
    "Piece of Eternity",
    "Piece of Oblivion"
  ),
  2,
  grottoFragment
)

val hallPuzzle = new CavernPuzzle(
  "Which light splits the truth from its illusion?\n" +
    "Hint: Clarity shines brightest in blue.",
  Vector(
    "Blue Beam",
    "Red Beam",
    "Green Beam"
  ),
  1,
  hallFragment
)

val veinPuzzle = new CavernPuzzle(
  "Where cold is no longer seen, what heat is left behind?\n" +
    "Hint: Memory outlasts the chill.",
  Vector(
    "Heat of Ages",
    "Heat of Fire",
    "Heat of Memory"
  ),
  3,
  veinFragment
)

class ForestPuzzle(
                  val question: String,
                  val answerList: Vector[String],
                  val correctAnswer: Int,
                  val reward: Item) extends Puzzle:

  var checkIfSolved = false

  def displayPuzzle() =
      println(question)
      for i <- answerList.indices do
        println(s"${i + 1}. ${answerList(i)}")
      println("\nEnter the correct answer:")

  def checkAnswer(input: Int) =
    val correct = input == correctAnswer
    if correct then checkIfSolved = true
    correct

val groovePuzzle = new ForestPuzzle(
  "You enter the Duskbloom Grove, where the fading light plays tricks on the eye.\n" +
  "In the center of the grove, you see a girl standing between two mythical creatures, a lion and a unicorn.\n" +
  "They speak to her, and their words echo in the twilight.\n\n" +
  "The lion says, \"Yesterday, I was lying.\"\nThe unicorn adds, \"So was I.\"\n\n" +
  "The creatures' nature is peculiar:\n\nThe lion lies on Monday, Tuesday, and Wednesday, but tells the truth on the other days.\n" +
  "The unicorn lies on Thursday, Friday, and Saturday, but speaks the truth on the other days.\n" +
  "The forest grows still, waiting for you to answer: What day is it?",
  Vector(
    "Monday",
    "Tuesday",
    "Thurday",
    "Saturday"
  ),
  3,
  grooveEssence
)

val thicketPuzzle = new ForestPuzzle(
  "In the heart of the Whispering Thicket, a group of adventurers must cross a rickety wooden bridge at night.\n" +
  "They only have one enchanted lantern that illuminates the path for only 15 minutes.\n" +
  "To make the crossing, they must work together, but the whispers of the thicket often confuse them.\n" +
  "\n\nCharacters:\n\nNina: Crosses the bridge in 1 minute.\nRufus: Crosses the bridge in 2 minutes.\nEleanor: Crosses the bridge in 5 minutes.\nGideon: Crosses the bridge in 8 minutes.\n" +
  "The adventurers must cross the bridge with the lantern before the light goes out.\n" +
  "They can only cross two at a time, and when they do, they must go at the pace of the slower individual.\n" +
  "What is the shortest time possible for all of them to cross the bridge before the torch goes out?",
  Vector(
    "14 minutes",
    "15 minutes",
    "17 minutes",
    "18 minutes"
  ),
  2,
  thicketEssence
)

val springPuzzle = new ForestPuzzle(
  "Four guardians named Naiad, Sylvan, Aether, and Thorn were competing in a friendly contest.\n " +
    "However, one of them told a lie.\n\n" +
    "Naiad said, \"The second guardian is Aether.\"\n" +
    "Sylvan said, \"I am not Thorn!\"\n" +
    "Aether said, \"Naiad? That is the fourth guardian.\"\n" +
    "Thorn remained silent.\n" +
    "Question: \"Which one of the guardians is Aether?\"",
  Vector(
    "The 1st one",
    "The 2nd one",
    "The 3rd one",
    "The 4th one"
  ),
  3,
  springEssence
)

val gladePuzzle = new ForestPuzzle(
  "Among the whispers, five words do stay,\n" +
    "RACECAR and MURDRUM dance in a play.\n" +
    "SAGAS tell tales of time’s gentle tread,\n" +
    "ATTACK rushes forth, leaving others in dread.\n\n" +
    "But one word is different, it doesn’t belong,\n" +
    "Can you find the fake in this wordy song?\n",
  Vector(
    "ATTACK",
    "SAGAS",
    "MURDUM",
    "RACECAR"
  ),
  4,
  gladeEssence
)