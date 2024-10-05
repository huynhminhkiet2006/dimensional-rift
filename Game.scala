import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.io.StdIn.*
import scala.util.*
import scala.util.control.Breaks.*

class Game {

  private var playerName: String = ""
  val action = new GameAction
  var isGameOver = false
  var currentAct = 1

  def randomEncounterChance(area: Area): Boolean =
    Random.nextDouble() < 1

  def spawnRandomEnemy(area: Area): Option[Enemy] =

    val enemyList = area match

      //Library
      case `hallway`  => Vector()
      case `archive`  => Vector(shadowLibrarian, ancientTome, curatorGuardian)
      case `chamber`  => Vector(curatorGuardian, echoWrath)
      case `sanctum`  => Vector(echoWrath, dimensionalShade)

      //Cavern
      case `chasm`    => Vector()
      case `abyss`    => Vector(abyssWatcher, lightWarden, shardStalker)
      case `grotto`   => Vector(shardStalker, lightWarden, shardgeist)
      case `hall`     => Vector(lightWarden, frostboundSentinel)
      case `vein`     => Vector(frostboundSentinel, shardgeist)

      //Forest
      case `clearing` => Vector()
      case `grove`    => Vector(chronoBeast, timeboundWrath)
      case `thicket`  => Vector(timeboundWrath, echoingPhantom)
      case `spring`   => Vector(chronoBeast, timeboundWrath, temporalLurker)
      case `glade`    => Vector(echoingPhantom, temporalLurker, aegisOfAges)

    if enemyList.nonEmpty then
      Some(enemyList(Random.nextInt(enemyList.length)))
    else
      None

  //Nexus
  entrance.setNeighbors(Vector("north" -> hallway                                                        ))
  
  //Library
  hallway. setNeighbors(Vector("north" -> throne, "east" -> archive,                    "west" -> chamber))
  chamber. setNeighbors(Vector(                   "east" -> hallway,                    "west" -> sanctum))
  sanctum. setNeighbors(Vector("north" -> throne, "east" -> chamber                                      ))
  archive. setNeighbors(Vector("north" -> throne,                                       "west" -> hallway))
  throne.  setNeighbors(Vector(                                      "south" -> hallway                  ))

  //Cavern
  chasm.   setNeighbors(Vector("north" -> heart,  "east" -> abyss,   "south" -> hall,   "west" -> grotto ))
  abyss.   setNeighbors(Vector(                                                         "west" -> chasm  ))
  grotto.  setNeighbors(Vector(                   "east" -> chasm                                        ))
  hall.    setNeighbors(Vector("north" -> chasm,                     "south" -> vein                     ))
  vein.    setNeighbors(Vector("north" -> hall                                                           ))
  heart.   setNeighbors(Vector(                                      "south" -> chasm                    ))

  //Forest
  clearing.setNeighbors(Vector(                   "east" -> grove,   "south" -> hourglass, "west" -> thicket ))
  grove.   setNeighbors(Vector(                                      "south" -> spring,    "west" -> clearing))
  thicket. setNeighbors(Vector(                   "east" -> clearing, "south" -> glade                       ))
  spring.  setNeighbors(Vector("north" -> grove                                                              ))
  glade.   setNeighbors(Vector("north" -> thicket                                                            ))
  hourglass.setNeighbors(Vector("north" -> clearing                                                          ))

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
                    println(s"You have acquired a key item: ${libPuzzle.reward}.")
                    actor.acquireItem(libPuzzle.reward)
                  else
                    println("The answer you seek eludes you... the echoes of your failure fade into the shadows.")
                    println("The walls close in, and you feel a cold hand guiding you back to the hallway.")
                    println("You are drawn away from the puzzle, leaving the forgotten knowledge behind for now.")
                    actor.travelTo(forgottenLibrary, hallway)
                    println(s"You are now in ${actor.currentArea}")

              case cavPuzzle: CavernPuzzle =>
                if cavPuzzle.checkIfSolved then
                  println("The puzzle here has already been solved.")
                else
                  cavPuzzle.displayPuzzle()
                  val input = action.getInputNumberNoPrinting((1 to 3).toVector)
                  if cavPuzzle.checkAnswer(input) then
                    println("The walls hum with energy, and a faint glow envelops the room.")
                    println("A piece of forgotten history unravels before you, its secrets whispered on the wind.")
                    println(s"You have revealed a fragment of the crystal puzzle: ${cavPuzzle.reward}.")
                    println("The energy in the room shifts, and the fragment seems to pulse with power in your hands.")
                    actor.acquireItem(cavPuzzle.reward)
                  else
                    println("The crystal's light flickers, but the answer slips from your grasp.")
                    println("The ground trembles beneath you, and the air grows heavy with dread.")
                    println("A surge of energy courses through the room, sending you back to the Echoing Chasm.")
                    actor.travelTo(crystalCavern, chasm)

                    println("You feel the weight of your failure, as the cavern drains a part of your strength.")
                    val lostHP = actor.maxHP * 10 / 100
                    actor.updateHP(actor.maxHP - lostHP)
                    println(s"You lose $lostHP HP as the cold, relentless force of the crystal claims a piece of you.")

              case forPuzzle: ForestPuzzle =>
                if forPuzzle.checkIfSolved then
                  println("The puzzle here has already been solved.")
                else
                  forPuzzle.displayPuzzle()
                  val input = action.getInputNumberNoPrinting((1 to 4).toVector)
                  if forPuzzle.checkAnswer(input) then
                    println("The forest hums in approval, the twisted flow of time aligning with your insight.")
                    println("As the leaves rustle, a glimmering essence materializes in your hands, pulsing with temporal energy.")
                    println(s"You have acquired an essence: ${forPuzzle.reward}.")
                    actor.acquireItem(forPuzzle.reward)
                  else
                    println("Time slips through your fingers, twisting out of reach... the answer remains hidden.")
                    println("A deep growl reverberates through the forest. From the shadows, a creature born of twisted time emerges.")
                    println("The forest offers no mercy as it sends one of its guardians to confront you.")
                    spawnRandomEnemy(actor.currentArea) match
                      case Some(enemy) =>
                        val encounter = new Encounter(actor, enemy)
                        if (!encounter.start()) then
                          isGameOver = true
                          break()
                      case None =>

          case None => println("There is no puzzle here to solve.")

      case "use" =>
        if (actor.getUsableItem.isEmpty) then
          println("You don't have any items to use now.")
        else
          actor.getUsableItem.zipWithIndex.foreach { case ((item, count), index) => println(s"${index + 1}. ${item} x$count") }
          val validChoiceList = (1 to actor.getUsableItem.size).toVector
          val inputInt = action.getInputNumber("Choose item to use: ", validChoiceList)
          actor.getItemByNumber(actor.getUsableItem, inputInt) match
            case Some(item) => actor.useItem(item)
            case None => println("Item not found.")
      case "switch" =>
        actor.displayWeaponList()
        val inputInt = action.getInputNumber("Choose weapon to switch: ", (1 to actor.weaponList.size).toVector)
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
                  println(s"You see the sign on the front door: $throne")
                  println("The door is tightly shut.")
                  println("What will you do now?")
                else
                  actor.go(traveledDirection)
                  println(s"You use the ${libraryKeyItem.name} to enter.")
                  println(s"You are now in ${actor.currentArea}.")
                  if (!actor.currentLocation.areaList.contains(actor.currentArea)) then
                    actor.addArea(actor.currentLocation, actor.currentArea)
                    println(s"You can now fast travel to ${actor.currentArea}.\n")
                  if (!libraryBoss.isDefeated) then
                    val bossEncounter = new BossEncounter(actor, libraryBoss)
                    if (!bossEncounter.start()) then
                      break()
              if area == heart then
                if !actor.itemList.contains(cavernKeyItem) then
                  println(s"You see an inscription on the crystal wall: $heart")
                  println("A barrier of light blocks your path.")
                  println("What will you do now?")
                else
                  actor.go(traveledDirection)
                  println(s"You use the ${cavernKeyItem.name} to enter.")
                  println(s"You are now in ${actor.currentArea}.")
                  if (!actor.currentLocation.areaList.contains(actor.currentArea)) then
                    actor.addArea(actor.currentLocation, actor.currentArea)
                    println(s"You can now fast travel to ${actor.currentArea}.\n")
                  if (!cavernBoss.isDefeated) then
                    val bossEncounter = new BossEncounter(actor, cavernBoss)
                    if (!bossEncounter.start()) then
                      break()
              if area == hourglass then
                if !actor.itemList.contains(forestKeyItem) then
                  println(s"You see the ancient inscription etched into the shattered doorway: $hourglass.")
                  println("The door stands frozen in time, sealed by an invisible force.")
                  println("What will you do now?")
                else
                  actor.go(traveledDirection)
                  println(s"You use the ${forestKeyItem.name} to enter.")
                  println(s"You are now in ${actor.currentArea}.")
                  if (!actor.currentLocation.areaList.contains(actor.currentArea)) then
                    actor.addArea(actor.currentLocation, actor.currentArea)
                    println(s"You can now fast travel to ${actor.currentArea}.\n")
                  if (!forestBoss.isDefeated) then
                    val bossEncounter = new BossEncounter(actor, forestBoss)
                    if (!bossEncounter.start()) then
                      break()
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
                      case cavPuzzle: CavernPuzzle  =>
                        if !cavPuzzle.checkIfSolved then
                          println("A puzzle awaits you here.\n")
                      case forPuzzle: ForestPuzzle  =>
                        if !forPuzzle.checkIfSolved then
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
                      case cavPuzzle: CavernPuzzle  =>
                        if !cavPuzzle.checkIfSolved then
                          println("A puzzle awaits you here.\n")
                      case forPuzzle: ForestPuzzle  =>
                        if !forPuzzle.checkIfSolved then
                          println("A puzzle awaits you here.\n")
                  case None =>
              if (randomEncounterChance(actor.currentArea)) then
                spawnRandomEnemy(actor.currentArea) match
                  case Some(enemy) =>
                    val encounter = new Encounter(actor, enemy)
                    if (!encounter.start()) then
                      isGameOver = true
                      break()
                  case None =>
                    println("The area seems quiet... for now.")
                    println("What will you do now?")
              else
                println("The area seems quiet... for now.")
                println("What will you do now?")
          case None =>
      case "view" =>
        println(s"1. Weapon\n2. Ability\n3. Item\n")
        val inputInt = action.getInputNumber("Choose category: ", (1 to 3).toVector)
        inputInt match
          case 1 =>
            actor.displayWeaponList()
            val inputInt = action.getInputNumber("Choose weapon to view: ", (1 to actor.weaponList.size).toVector)
            actor.weaponList(inputInt - 1).displayStat()
          case 2 =>
            actor.displayAbilityList()
            val inputInt = action.getInputNumber("Choose ability to view: ", (1 to actor.weaponList.size).toVector)
            println(s"${actor.abilityList(inputInt - 1)}: ${actor.abilityList(inputInt - 1).description}")
          case 3 =>
            if (actor.getItem.isEmpty) then
              println("You don't have any items to view now.")
            else
              actor.getItem.zipWithIndex.foreach { case ((item, count), index) => println(s"${index + 1}. ${item} x$count") }
              val validChoiceList = (1 to actor.getItem.size).toVector
              val inputInt = action.getInputNumber("Choose item to view: ", validChoiceList)
              actor.getItemByNumber(actor.getItem, inputInt) match
                case Some(item) => item.description()
                case None => println("Item not found.")
      case "fast travel" =>
        actor.displayAvailableLocation()
        val inputInt = action.getInputNumber("Choose location: ", (1 to actor.unlockedLocation.size).toVector)
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
                case cavPuzzle: CavernPuzzle =>
                  if !cavPuzzle.checkIfSolved then
                    println("A puzzle awaits you here.\n")
            case None =>
          println(s"${actor.unlockedLocation(inputInt - 1).areas(inputInt2 - 1).description}")
          if (randomEncounterChance(actor.currentArea)) then
            spawnRandomEnemy(actor.currentArea) match
              case Some(enemy) =>
                val encounter = new Encounter(actor, enemy)
                if (!encounter.start()) then
                  isGameOver = true
                  break()
              case None =>
                println("The area seems quiet... for now.")
                println("What will you do now?")
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
    println("view: Want to view detailed description on everything you have in hand? This is the command for you.")

  def newGame() =

    println("\n\nCharacters, art, and storylines depicted in this game are purely the work of fiction. ")
    println("Any similarity to persons living or dead is purely coincidental.\n\n")
    println("Use headphones for the best experience.")
    print("Press Enter to continue...")
    readLine()


    val yourName = askForName()
    readLine()

    val player = new Player(
      yourName,
      200,
      200,
      50,
      50,
      20,
      20,
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

    //printWelcomeMessage()

    var continueGame = true
    breakable:
      while (continueGame) do
        currentAct match
          case 1 =>
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
            while (!player.itemList.contains(libraryKeyItem)) do
              if (player.itemList.contains(chamberKeyItem) &&
                  player.itemList.contains(sanctumKeyItem) &&
                  player.itemList.contains(archiveKeyItem)) then
                println("You noticed all 3 key items light up when you put them close together in your inventory.")
                println(s"$chamberKeyItem, $sanctumKeyItem and $archiveKeyItem have fused into $libraryKeyItem!")
                player.removeItem(chamberKeyItem)
                player.removeItem(sanctumKeyItem)
                player.removeItem(archiveKeyItem)
                player.acquireItem(libraryKeyItem)
                println("You feel like you can unlock The Throne of the Forgotten area now.")
              else
                val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  continueGame = false
                  break()
            while (!libraryBoss.isDefeated) do
              val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  continueGame = false
                  break()
            println("End of Act 1")
            println("Press enter to continue...")
            currentAct = 2
            readLine()
          case 2 =>
            println("Act 2")
            player.travelTo(crystalCavern, chasm)
            player.addLocation(crystalCavern)
            player.addArea(crystalCavern, chasm)
            println("You awake in somewhere.")
            while (!player.itemList.contains(cavernKeyItem)) do
              if (player.itemList.contains(hallFragment) &&
                player.itemList.contains(abyssFragment) &&
                player.itemList.contains(grottoFragment) &&
                player.itemList.contains(veinFragment)) then
                player.acquireItem(cavernKeyItem)
              else
                val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  continueGame = false
                  break()

            println("You feel like you can unlock The Heart Of Glass area now.")
            while (!cavernBoss.isDefeated) do
              val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  continueGame = false
                  break()

            println("End of Act 2")
            currentAct = 3
          case 3 =>
            println("Act 3")
            player.travelTo(timeTwistedForest, clearing)
            player.addLocation(timeTwistedForest)
            player.addArea(timeTwistedForest, clearing)
            println("You awake in somewhere.")
            while (!player.itemList.contains(forestKeyItem)) do
              if (player.itemList.contains(grooveEssence) &&
                player.itemList.contains(thicketEssence) &&
                player.itemList.contains(springEssence) &&
                player.itemList.contains(gladeEssence)) then
                player.acquireItem(forestKeyItem)
              else
                val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  continueGame = false
                  break()
            println("You feel like you can unlock the Broken Hourglass area now.")
            while (!forestBoss.isDefeated) do
              val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  continueGame = false
                  break()

            println("End of Act 2")
            currentAct = 3
    println("Game finished.")

}
