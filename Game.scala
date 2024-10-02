import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.io.StdIn.*
import scala.util.*

class Game {

  private var playerName: String = ""
  val action = new GameAction

  var isGameOver = false

  def randomEncounterChance(area: Area): Boolean =
    val encounterChance = 0
    val randomValue = Random.nextDouble()
    randomValue < encounterChance

  def spawnRandomEnemy(area: Area): Enemy =
    val enemyList = area match
      //Library
      case `hallway` => Vector(shadowLibrarian)
      case `archive` => Vector(ancientTome)
      case `chamber` => Vector(curatorGuardian)
      case `sanctum` => Vector(echoWrath)
      case _         => Vector()

    enemyList(Random.nextInt(enemyList.length))


  entrance.setNeighbors(Vector("north" -> hallway                                                        ))

  hallway. setNeighbors(Vector("north" -> throne, "east" -> archive,                    "west" -> chamber))
  chamber. setNeighbors(Vector(                   "east" -> hallway,                    "west" -> sanctum))
  sanctum. setNeighbors(Vector("north" -> throne, "east" -> chamber                                      ))
  archive. setNeighbors(Vector("north" -> throne,                                       "west" -> hallway))
  throne.  setNeighbors(Vector(                                      "south" -> hallway                  ))

  def getStringCommand(actor: Player, input: String) =

    val lowerCaseInput = input.toLowerCase
    lowerCaseInput match
      case "solve puzzle" =>
        actor.currentArea.containsPuzzle match
          case Some(puzzle) =>
            puzzle match
            case libPuzzle: LibraryPuzzle =>
              if libPuzzle.checkIfSolved then
                println("The puzzle here has already been solved.")
              else
                libPuzzle.displayPuzzle()
                val input = readLine()
                if libPuzzle.checkAnswer(input) then
                  println("The puzzle shifts, revealing the truth hidden beneath layers of ancient secrets.")
                  println("A hidden force stirs, revealing an ancient relic that pulses with a strange energy.")
                  println(s"You have acquired a key item: ${libPuzzle.reward}")
                  actor.acquireItem(libPuzzle.reward)
                else
                  println("The answer you seek eludes you... the echoes of your failure fade into the shadows.")
                  println("The walls close in, and you feel a cold hand guiding you back to the hallway.")
                  println("You are drawn away from the puzzle, leaving the forgotten knowledge behind for now.")
                  actor.travelTo(forgottenLibrary, hallway)
                  println(s"You are now in ${actor.currentArea}")
          case None => println("There is no puzzle here to solve.")

      case "use" =>
        if (actor.getUsableItem.isEmpty) then
          println("You don't have any items to use now.")
        else
          actor.getUsableItem.zipWithIndex.foreach { case ((item, count), index) =>
            println(s"${index + 1}. ${item} x$count")
          }
          val validChoiceList = (1 to actor.getUsableItem.size).toVector
          val inputInt = action.getInputNumber("Choose item to use: ", validChoiceList)
          actor.getItemByNumber(actor.getUsableItem, inputInt) match
            case Some(item) => actor.useItem(item)
            case None => println("Item not found.")
      case "switch" =>
        actor.displayWeaponList()
        val validChoiceList = (1 to actor.weaponList.size).toVector
        val inputInt = action.getInputNumber("Choose weapon to switch: ", validChoiceList)
        actor.switchWeapon(actor.weaponList(inputInt - 1))
      case "go" =>
        actor.currentArea.displayExitsAvailable()
        val validChoiceList = (1 to actor.currentArea.exitsAvailable.size).toVector
        val inputInt = action.getInputNumber("Choose directon: ", validChoiceList)
        val traveledDirection = actor.currentArea.exitsAvailable(inputInt - 1)
        println(s"You go ${traveledDirection}\n\n")
        actor.currentArea.neighbor(traveledDirection) match
          case Some(area) =>
            if area.keyArea then
              if area == throne then
                if !actor.itemList.contains(libraryKeyItem) then
                  println("The door is tightly shut.")
                  println("What will you do now?")
                else
                  actor.go(traveledDirection)
                  println(s"You use the ${libraryKeyItem.name}.")
            else
              actor.go(traveledDirection)
              if (!actor.currentLocation.areaList.contains(actor.currentArea)) then
                actor.addArea(actor.currentLocation, actor.currentArea)
                println(s"You are now in ${actor.currentArea}.")
                actor.currentArea.containsPuzzle match
                  case Some(puzzle) =>
                    puzzle match
                      case libPuzzle: LibraryPuzzle =>
                        if !libPuzzle.checkIfSolved then
                          println("A puzzle awaits you here.\n")
                  case None =>
                println(s"${actor.currentArea.name}: ${actor.currentArea.description}.\n")
                println(s"You can now fast travel to ${actor.currentArea}.\n")
              else
                println(s"You are now in ${actor.currentArea}.\n")
                actor.currentArea.containsPuzzle match
                  case Some(puzzle) =>
                    puzzle match
                      case libPuzzle: LibraryPuzzle =>
                        if !libPuzzle.checkIfSolved then
                          println("A puzzle awaits you here.\n")
                  case None =>
              if (randomEncounterChance(actor.currentArea)) then
                val enemy = spawnRandomEnemy(actor.currentArea)
                val encounter = new Encounter(actor, enemy)
                if (!encounter.start()) then
                  isGameOver = true
              else
                println("The area seems quiet... for now.")
                println("What will you do now?")


          case None =>

      case "fast travel" =>
        actor.displayAvailableLocation()
        val validChoiceList = (1 to actor.unlockedLocation.size).toVector
        val inputInt = action.getInputNumber("Choose location: ", validChoiceList)
        if inputInt == 1 then
          actor.travelTo(dimensionalNexus, entrance)
          println("You returned to entrance.")
        else
          actor.displayAvailableArea(actor.unlockedLocation(inputInt - 1))
          val validChoiceList2 = (1 to actor.unlockedLocation(inputInt - 1).areas.size).toVector
          val inputInt2 = action.getInputNumber("Choose area: ", validChoiceList2)
          actor.travelTo(actor.unlockedLocation(inputInt - 1), actor.unlockedLocation(inputInt - 1).areas(inputInt2 - 1))
          println(s"You traveled to ${actor.unlockedLocation(inputInt - 1).areas(inputInt2 - 1)}.")
          actor.currentArea.containsPuzzle match
                  case Some(puzzle) =>
                    puzzle match
                      case libPuzzle: LibraryPuzzle =>
                        if !libPuzzle.checkIfSolved then
                          println("A puzzle awaits you here.\n")
                  case None =>
          println(s"${actor.unlockedLocation(inputInt - 1).areas(inputInt2 - 1).description}")
          if (randomEncounterChance(actor.currentArea)) then
            val enemy = spawnRandomEnemy(actor.currentArea)
            val encounter = new Encounter(actor, enemy)
            if (!encounter.start()) then
              isGameOver = true
          else
            println("The area seems quiet... for now.")
            println("What will you do now?")

      case "inventory" =>
        actor.displayInventory()
      case "display" =>
        actor.displayFullStat()

      case "return" =>
        actor.travelTo(dimensionalNexus, entrance)
        println("You returned to the Dimensional Nexus's entrance.")

      case _ =>
        println("Invalid input. Please try again.")

  def askForName() =
    var validInput = false
    while (!validInput) do
      println("Try to remember your name.")
      print("Enter your first name: ")
      var firstName = readLine().trim
      while (firstName.isEmpty) do
        println("First name cannot be empty. Please try again.")
        print("Enter your first name: ")
        firstName = readLine().trim
      print("Enter your last name: ")
      var lastName = readLine().trim
      while (lastName.isEmpty) do
        println("Last name cannot be empty. Please try again.")
        print("Enter your last name: ")
        lastName = readLine().trim
      playerName = s"$firstName $lastName"
      println(s"Nice, your name is ${playerName}.")
      println("\n\nWell, relax and have fun with the game.\n\n")
      println("------------------------------------------------------------------------")
      validInput = true
    playerName

  def printWelcomeMessage() =

    println("\n\nWelcome to Dimensional Rift: The Quest for the Artifact!")
    println("In a world unraveling at the seams, echoes of time call to you. ")
    println("Unravel the mysteries of the rift, confront hidden truths, and seek the Artifact of Stability.")
    println("Your journey begins now... Will you restore the balance, or will chaos reign?")
    println("\n\n--------------------------------------------------------------------------------\n\n")

  def displayHelp() =
    println("Cypher: Now, about your commands. This world may seem vast, but you can navigate it with a few key actions.")
    println("go: Use this command to move through the Rift. When you’re in an area, check the available directions and pick a path.")
    println("switch: Want to try another weapon? Switch to a different one from your inventory with this command.")
    println("use: If you find useful items, you can use them while exploring. Just be sure to check your inventory.")
    println("fast travel: Once you’ve unlocked locations and areas, you can fast travel between them. This will save you time when backtracking.")
    println("inventory: Want to see what you're carrying? Use this to check your weapons, abilities, and items at any time.")
    println("display: This will show you your current stats—HP, SP, weapons, abilities, and so on. It's always good to check how prepared you are before a big fight.")
    println("return: If things get too dangerous, you can use this to return to the entrance.")
    println("solve puzzle: Use this command to solve the puzzle in the current area - if there are any.")

  def newGame() =
/*
    println("\n\nCharacters, art, and storylines depicted in this game are purely the work of fiction. ")
    println("Any similarity to persons living or dead is purely coincidental.\n\n")
    println("Use headphones for the best experience.")
    print("Press Enter to continue...")
    readLine()

*/
    val yourName = askForName()
    readLine()

    val player = new Player(
      yourName,
      200,
      200,
      50,
      50,
      15,
      15,
      0,
      1,
      Buffer[Ability](agi),
      Buffer[Item](),
      0,
      mutable.Buffer[Location](dimensionalNexus),
      dimensionalNexus,
      entrance,
      Fist,
      mutable.Buffer[Weapon](Fist))

    printWelcomeMessage()
    player.addArea(dimensionalNexus, entrance)
    println("You wake up.")
    readLine()
    println("You see a door in front of you.")
    readLine()
    println("You decide to enter.")
    player.travelTo(forgottenLibrary, hallway)
    player.addLocation(forgottenLibrary)
    player.addArea(forgottenLibrary, hallway)
    readLine()
    println("You're in the forgotten library.")
    println("What will you do now?")

    var continueGame = true
    while (continueGame) do
      if (!player.itemList.contains(libraryKeyItem)) then
        if (player.itemList.contains(chamberKeyItem) &&
            player.itemList.contains(sanctumKeyItem) &&
            player.itemList.contains(archiveKeyItem)) then
          player.acquireItem(libraryKeyItem)
        else
          val input = readLine()
          getStringCommand(player, input)

          if (isGameOver) then
            println("Game Over!")
            continueGame = false
      else
        println("Ready to go?")
        println("1. Yes, I'm ready.")
        println("2. No, I need to prepare.")
        val inputInt = action.getInputNumberNoPrinting(Vector(1, 2))

        if inputInt == 1 then
          player.travelTo(forgottenLibrary, throne)
          player.addArea(forgottenLibrary, throne)
          println("You are not at the throne.")
          val bossEncounter = new BossEncounter(player, libraryBoss)
          bossEncounter.start()

    println("Game finished.")

}
