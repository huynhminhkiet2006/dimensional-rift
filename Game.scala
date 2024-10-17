import scala.collection.mutable.*
import scala.io.StdIn.*
import java.io.*
import scala.util.*
import scala.util.control.Breaks.*

class Game {

  private var playerName: String = ""
  val action = new GameAction
  var isGameOver = false

  def randomEncounterChance(area: Area): Boolean =
    Random.nextDouble() < 0.7

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
  clearing. setNeighbors(Vector(                   "east" -> grove,   "south" -> hourglass, "west" -> thicket ))
  grove.    setNeighbors(Vector(                                      "south" -> spring,    "west" -> clearing))
  thicket.  setNeighbors(Vector(                   "east" -> clearing, "south" -> glade                       ))
  spring.   setNeighbors(Vector("north" -> grove                                                              ))
  glade.    setNeighbors(Vector("north" -> thicket                                                            ))
  hourglass.setNeighbors(Vector("north" -> clearing                                                           ))

  def getStringCommand(actor: Player, input: String) =

    val lowerCaseInput = input.trim.toLowerCase
    lowerCaseInput match
      case "solve" =>
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
                    println("\nNice. What's next?")
                  else
                    println("The answer you seek eludes you... the echoes of your failure fade into the shadows.")
                    println("You are drawn away from the puzzle, leaving the forgotten knowledge behind for now.")
                    println("The walls close in, and you feel a cold hand guiding you back to the Dimly Lit Hallway.")
                    actor.travelTo(forgottenLibrary, hallway)
                    println("What will you do now?")

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
                    println("\nNice. What's next?")
                  else
                    println("The crystal's light flickers, but the answer slips from your grasp.")
                    println("The ground trembles beneath you, and the air grows heavy with dread.")
                    println("A surge of energy courses through the room, sending you back to the Echoing Chasm.")
                    actor.travelTo(crystalCavern, chasm)

                    action.pressEnter()

                    println("You feel the weight of your failure, as the cavern drains a part of your strength.")
                    val lostHP = actor.maxHP * 10 / 100
                    actor.updateHP(actor.maxHP - lostHP)
                    println(s"You lose $lostHP HP as the cold, relentless force of the crystal claims a piece of you.")
                    println("What will you do now?")

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
                    println("\nNice. What's next?")
                  else
                    println("Time slips through your fingers, twisting out of reach... the answer remains hidden.")
                    println("A deep growl reverberates through the forest. From the shadows, a creature born of twisted time emerges.")
                    println("The forest offers no mercy as it sends one of its guardians to confront you.")
                    spawnRandomEnemy(actor.currentArea) match
                      case Some(enemy) =>
                        val encounter = new Encounter(actor, enemy)
                        if (!encounter.start()) then
                          isGameOver = true
                      case None =>

          case None => println("There is no puzzle here to solve.")

      case "use" =>
        if actor.getUsableItem.isEmpty then
          println("You don't have any items to use now.\n")
        else
          println("Choose an item (or type 'cancel' to go back):")
          actor.getUsableItem.toList.zipWithIndex.foreach:
            case ((item, count), index) =>
            println(s"${index + 1}. ${item} x$count")

          val input = readLine().trim

          if (input.equalsIgnoreCase("cancel")) then
            println("What will you do now?")
          else
            try
              val choice = input.toInt
              if (choice < 1 || choice > actor.getUsableItem.size) then
                println("Invalid choice. Please choose a valid item.")
              else
                val selectedItem = actor.getItemByNumber(actor.getUsableItem, choice)
                selectedItem match
                  case Some(item) =>
                    actor.useItem(item)
                    println("What will you do now?")
                  case None =>
                    println("Item not found.")
            catch
              case _: NumberFormatException =>
                println("Please enter a valid number or 'cancel' to go back.")


      case "switch" =>
        var cnt = 1
        actor.weaponList.sortBy(_.atkPower).foreach:
          weapon =>
            print(s"$cnt. ")
            weapon.displayStat()
            cnt += 1
        val inputInt = action.getInputNumber("Choose weapon to switch: ", (1 to actor.weaponList.size).toVector)
        actor.switchWeapon(actor.weaponList(inputInt - 1))
        println("Nice. What's next?")
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
                      isGameOver = true
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
                      isGameOver = true
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
                      isGameOver = true
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
                println(s"${actor.currentArea.name}: ${actor.currentArea.description}\n")
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
                    chooseBattleMusic(actor.currentLocation)
                    val encounter = new Encounter(actor, enemy)
                    if (!encounter.start()) then
                      isGameOver = true
                    chooseBackgroundMusic(actor.currentLocation)
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
            actor.weaponList.foreach(_.displayStat())
          case 2 =>
            actor.abilityList.foreach(ability => println(s"${ability}: ${ability.description}."))
          case 3 =>
            if (actor.getItem.isEmpty) then
              println("You don't have any items to view now.")
            else
              actor.getItem.foreach(_._1.description())
        println("Ok. What's next?")
      case "travel" =>
        actor.displayAvailableLocation()
        val inputInt = action.getInputNumber("Choose location: ", (1 to actor.unlockedLocation.size).toVector)
        if inputInt == 1 then
          actor.travelTo(dimensionalNexus, entrance)
          println("You returned to the Dimensional Nexus's entrance.")
          println("There's only a door that leads to the Forgotten Library here.")
          println("What will you do now?")
        else
          val availableAreas = actor.unlockedLocation(inputInt - 1).areas.filterNot(_ == actor.currentArea)
          if (availableAreas.isEmpty) then
            println("There's no other area to travel to here.")
          else
            for i <- availableAreas.indices do
              println(s"${i + 1}. ${availableAreas(i)}")
            val validChoiceList2 = (1 to availableAreas.size).toVector
            val inputInt2 = action.getInputNumber("Choose area: ", validChoiceList2)
            actor.travelTo(actor.unlockedLocation(inputInt - 1), availableAreas(inputInt2 - 1))
            println(s"You traveled to ${availableAreas(inputInt2 - 1)}.")
            if actor.currentArea.keyArea then
              if actor.currentArea == throne then
                if !actor.itemList.contains(libraryKeyItem) then
                  println(s"You see the sign on the front door: $throne")
                  println("The door is tightly shut.")
                  println("What will you do now?")
                else
                  println(s"You use the ${libraryKeyItem.name} to enter.")
                  println(s"You are now in ${actor.currentArea}.")
                  if (!actor.currentLocation.areaList.contains(actor.currentArea)) then
                    actor.addArea(actor.currentLocation, actor.currentArea)
                    println(s"You can now fast travel to ${actor.currentArea}.\n")
                  if (!libraryBoss.isDefeated) then
                    val bossEncounter = new BossEncounter(actor, libraryBoss)
                    if (!bossEncounter.start()) then
                      isGameOver = true
              if actor.currentArea == heart then
                if !actor.itemList.contains(cavernKeyItem) then
                  println(s"You see an inscription on the crystal wall: $heart")
                  println("A barrier of light blocks your path.")
                  println("What will you do now?")
                else
                  println(s"You use the ${cavernKeyItem.name} to enter.")
                  println(s"You are now in ${actor.currentArea}.")
                  if (!actor.currentLocation.areaList.contains(actor.currentArea)) then
                    actor.addArea(actor.currentLocation, actor.currentArea)
                    println(s"You can now fast travel to ${actor.currentArea}.\n")
                  if (!cavernBoss.isDefeated) then
                    val bossEncounter = new BossEncounter(actor, cavernBoss)
                    if (!bossEncounter.start()) then
                      isGameOver = true
              if actor.currentArea == hourglass then
                if !actor.itemList.contains(forestKeyItem) then
                  println(s"You see the ancient inscription etched into the shattered doorway: $hourglass.")
                  println("The door stands frozen in time, sealed by an invisible force.")
                  println("What will you do now?")
                else
                  println(s"You use the ${forestKeyItem.name} to enter.")
                  println(s"You are now in ${actor.currentArea}.")
                  if (!actor.currentLocation.areaList.contains(actor.currentArea)) then
                    actor.addArea(actor.currentLocation, actor.currentArea)
                    println(s"You can now fast travel to ${actor.currentArea}.\n")
                  if (!forestBoss.isDefeated) then
                    val bossEncounter = new BossEncounter(actor, forestBoss)
                    if (!bossEncounter.start()) then
                      isGameOver = true
              if actor.currentArea == nexus then
                println("You step through the unknown portal at the entrance.")
                println(s"You are now in ${actor.currentArea}.")
                if (!finalBoss.isDefeated) then
                  val finalBossEncounter = new FinalBossEncounter(actor, finalBoss)
                  if (!finalBossEncounter.start()) then
                    isGameOver = true
            else
              actor.currentArea.containsPuzzle match
                case Some(puzzle) =>
                  puzzle match
                    case libPuzzle: LibraryPuzzle =>
                      if !libPuzzle.checkIfSolved then
                        println("A puzzle awaits you here.\n")
                    case cavPuzzle: CavernPuzzle =>
                      if !cavPuzzle.checkIfSolved then
                        println("A puzzle awaits you here.\n")
                    case forPuzzle: ForestPuzzle =>
                      if !forPuzzle.checkIfSolved then
                        println("A puzzle awaits you here.\n")
                case None =>

              println(s"${actor.unlockedLocation(inputInt - 1).areas(inputInt2 - 1).description}")
              if (randomEncounterChance(actor.currentArea)) then
                spawnRandomEnemy(actor.currentArea) match
                  case Some(enemy) =>
                    chooseBattleMusic(actor.currentLocation)
                    val encounter = new Encounter(actor, enemy)
                    if (!encounter.start()) then
                      isGameOver = true
                    chooseBackgroundMusic(actor.currentLocation)
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
        println("There's only a door that leads to the Forgotten Library here.")

      case "help" =>
        displayHelp()

      case _ =>
        println("Invalid input. Please try again.")

  def askForName() =
    var validInput = false
    while (!validInput) do
      println("\n\nTry to remember your name.")
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
      println("--------------------------------------------------------------------------------------")
      validInput = true
    playerName

  def printWelcomeMessage() =

    println("\n\nWelcome to Dimensional Rift: The Quest for the Artifact!")
    println("In a world unraveling at the seams, echoes of time call to you. ")
    println("Unravel the mysteries of the rift, confront hidden truths, and seek the Artifact of Stability.")
    println("Your journey begins now... Will you restore the balance, or will chaos reign?")
    println("\n\n--------------------------------------------------------------------------------------\n\n")

  def displayHelp() =

    println("\ngo: Use this command to move through the Rift. When you’re in an area, check the available directions and pick a path.")
    println("switch: Want to try another weapon? Switch to a different one from your inventory with this command.")
    println("use: If you find useful items, you can use them while exploring. Just be sure to check your inventory.")
    println("travel: Once you’ve unlocked locations and areas, you can fast travel between them. This will save you time when backtracking.")
    println("inventory: Want to see what you're carrying? Use this to check your weapons, abilities, and items at any time.")
    println("display: This will show you your current stats—HP, SP, weapons, abilities, and so on. It's always good to check how prepared you are before a big fight.")
    println("return: If things get too dangerous, you can use this to return to the entrance.")
    println("solve: Use this command to solve the puzzle in the current area - if there are any.")
    println("view: Want to view detailed description on everything you have in hand? This is the command for you.")
    println("help: This will show the full command list for you again, in case you want to try something out.\n")

  def gameOverPrompt(location: Location, area: Area) =

    println("\nAs your vision fades, you hear a distant whisper... Cypher's voice echoes in the darkness.")
    println("Cypher: 'Traveler, your journey is not yet over. Even in defeat, you have a choice.'")
    action.pressEnter()

    println(s"Cypher: 'You may return to the $area of the $location, where time itself will mend your wounds and restore your strength.'")
    println("Cypher: 'Or... if your heart has grown weary, you may let the journey end here.'")
    println("\nWhat will you choose?")
    println("1. Return to the central hub and fight again.")
    println("2. End the journey and let the rift close around you.")

    val inputInt = action.getInputNumberNoPrinting((1 to 2).toVector)

    inputInt match
      case 1 =>
        println("\nA surge of energy flows through you as the world around you shifts. Time rewinds, restoring your body and mind.")
        println(s"You find yourself back at the $area of the $location, your strength renewed, ready to face the challenges once more.")
        true

      case 2 =>
        println("\nYou feel the rift closing around you, its mysteries left unsolved. Your journey ends here, but the echoes of your path remain.")
        println("The screen fades to black as the final words linger in your mind... 'Until we meet again, Traveler.'")
        false

  def chooseBattleMusic(location: Location) =
    location match
      case `forgottenLibrary`  => MusicManager.loopMusic("BattleOfBloodshed")
      case `crystalCavern`     => MusicManager.loopMusic("FireViolin")
      case `timeTwistedForest` => MusicManager.loopMusic("ColdSteel")

  def chooseBackgroundMusic(location: Location) =
    location match
      case `forgottenLibrary`  => MusicManager.loopMusic("Eclipsed Memories")
      case `crystalCavern`     => MusicManager.loopMusic("Freezing Breath")
      case `timeTwistedForest` => MusicManager.loopMusic("SIlence")

  def displayTitleScreen() =

    MusicManager.loopMusic("Prelude")

    println("*******************************************************************")
    println("*                                                                 *")
    println("*                        DIMENSIONAL RIFT                         *")
    println("*                   The Quest for the Artifact                    *")
    println("*                                                                 *")
    println("*                    A Text-Based RPG by KH                       *")
    println("*                                                                 *")
    println("*             Use headphones for the best experience              *")
    println("*                                                                 *")
    println("*******************************************************************")


    println("\n1. NEW GAME: Start a new Adventure")
    println("2. LOAD GAME: Return to the Rift [Currently unavailable due to skill issue]")
    println("3. Quit")

    val input = action.getInputNumber("Choose an option: ", Vector(1, 3))

    input match
      case 1 =>
        println("\nStarting a new game...\n")
        newGame()
      case 3 =>
        println("\nExiting game... Goodbye!")
        System.exit(0)


  def newGame() =

    MusicManager.loopMusic("Whispers")

    println("\n\nCharacters, art, and storylines depicted in this game are purely the work of fiction. ")
    println("Any similarity to persons living or dead is purely coincidental.\n\n")
    Thread.sleep(1000)
    print("Press Enter to begin your journey into the unknown...")
    readLine()

    val yourName = askForName()
    readLine()

    val player = new Player(
      yourName,
      "player",
      "None",
      false,
      2000,
      2000,
      500,
      500,
      200,
      200,
      0,
      1,
      Buffer[Ability](megidolaon, godHand, tarunda, tarukaja, rakunda, rakukaja),
      Buffer[Item](forestKeyItem),
      0,
      Buffer[Location](dimensionalNexus),
      dimensionalNexus,
      entrance,
      Fist,
      Buffer[Weapon](Fist))

    printWelcomeMessage()
    action.pressEnter()


    println("\nDarkness envelopes your vision. Time itself seems to lose meaning.")
    println("A voice echoes in the distance, faint but insistent: 'Awaken, Traveler.'")
    println("The silence is broken only by a soft hum, as though reality itself is vibrating around you...")
    action.pressEnter()

    println("\nYou open your eyes. The air feels dense, as if reality itself is fragile.")
    println("Before you stands a door, old and heavy, yet pulsing with a strange energy.")

    action.pressEnter()
    println("\nA figure emerges from the shadows beside the door, her voice soft yet commanding.")
    println("Mysterious figure: 'It’s been a while since we’ve had a Traveler. The Rift is not for the faint of heart.'")
    action.pressEnter()
    println("She steps closer, her features still partially obscured by the gloom, though her eyes seem to glow faintly.")
    println("Mysterious figure: 'You stand within the Dimensional Rift, a realm adrift between forgotten realities.")
    action.pressEnter()
    println("The Dimensional Nexus, your current refuge, shields you from the chaos outside. \n" +
      "It is a sanctuary of stillness amidst the storm, where time itself seems to hesitate. \n" +
      "But beyond its doors... lies a world unraveling, where danger and uncertainty reign. \n" +
      "Tread carefully, for nothing is certain in the Rift.'")
    action.pressEnter()
    println("Mysterious figure: 'I am Cypher, your guide through this fractured reality. \n" +
      "Beyond this door lies your first step... if you're ready.'")
    action.pressEnter()

    println("\nInstinctively, you know this door leads to answers... and to danger.")
    println("Do you approach?")
    println("1. Yes")
    println("2. No")
    val inputInt = action.getInputNumberNoPrinting((1 to 2).toVector)

    inputInt match
      case 1 =>
        println("With each step, the door looms larger. It creaks open with an ominous groan, beckoning you inside.")
        action.pressEnter()
        println("The moment you pass through, the world shifts. The air grows cold, and a suffocating silence falls around you.")
      case 2 =>
        println("You hesitate, feeling the weight of indecision. But something compels you forward. The door opens on its own.")
        action.pressEnter()
        println("The moment you step through, reality warps around you, and the silence is all-consuming.")

    MusicManager.loopMusic("Eclipsed Memories")

    println("Cypher: 'Welcome, Traveler. This place you now stand in is called the Forgotten Library. " +
      "It exists outside of time and space.'")
    action.pressEnter()

    println("Cypher: 'The books here hold more than knowledge. Rearranging them in the right order will reveal key items.'")
    println("Cypher: 'Look closely. The path forward is hidden among the pages.'")
    action.pressEnter()

    println("Cypher: 'Every item you collect will bring you closer to unlocking the Throne of the Forgotten.\n It lies beyond the puzzle, waiting for those who truly understand the secrets within these walls.'")
    println("Cypher: 'Be patient, Traveler. The final piece will reveal itself when the time is right.'")
    action.pressEnter()

    println("Cypher: 'To navigate this realm, you will need to use certain commands. Let me guide you through them.'")
    action.pressEnter()

    displayHelp()
    action.pressEnter()

    println("Cypher: 'I’ve shown you what you need to survive, but the path ahead is yours to walk. " +
      "Stay sharp, Traveler. We will meet again when the time is right.'")
    action.pressEnter()

    println("With a faint smile, she fades into the shadows, her presence lingering for just a moment before disappearing completely.")
    println("The air around you feels heavier in her absence, the silence deafening.")
    action.pressEnter()
    println("You stand alone in the library hallway, the unknown stretching out ahead of you.")
    println("What will you do now?")

    player.addArea(dimensionalNexus, entrance)
    player.travelTo(forgottenLibrary, hallway)
    player.addLocation(forgottenLibrary)
    player.addArea(forgottenLibrary, hallway)

    var continueGame = true

    gameLoop(player, 3)

  def gameLoop(player: Player, act: Int) =

    var currentAct = act
    var continueGame = true

    breakable:
      while (continueGame) do

        currentAct match

          case 1 =>

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
                  MusicManager.loopMusic("Game Over")
                  if (gameOverPrompt(forgottenLibrary, hallway)) then
                    player.selfRecovery()
                    player.travelTo(forgottenLibrary, hallway)
                    MusicManager.loopMusic("Eclipsed Memories")
                    println("What will you do now?")
                    isGameOver = false
                  else
                    continueGame = false
                    break()

            while (!libraryBoss.isDefeated) do
              val input = readLine()
              getStringCommand(player, input)
              if isGameOver then
                MusicManager.loopMusic("Game Over")
                if (gameOverPrompt(forgottenLibrary, hallway)) then
                  player.selfRecovery()
                  player.travelTo(forgottenLibrary, hallway)
                  println("What will you do now?")
                  isGameOver = false
                else
                  continueGame = false
                  break()

            MusicManager.loopMusic("Eclipsed Memories")
            println("\nThe defeated boss gasps, its form flickering like a dying flame.")
            println("Archivus the Eternal Scholar: 'You may have won this battle, but the shadows will always linger in the depths...'")
            action.pressEnter()
            println("As the boss crumbles, a radiant stone falls to the ground—a Luminary Stone, pulsing with energy.")
            println("You pick up the Luminary Stone, unaware of its true significance.")
            action.pressEnter()
            println("Suddenly, a familiar presence envelops you as Cypher steps into view.")
            println("Cypher: 'Well done, Traveler. You've triumphed over the darkness, but this victory is just the beginning.'")
            action.pressEnter()
            println("Cypher gestures to the stone in your hand.")
            println("Cypher: 'This Luminary Stone is a key—a beacon of hope that will unlock the next chapter of your journey: the Crystal Cavern.'")
            println("In an instant, the world shifts around you. Light envelops your vision, and you feel a surge of energy.")

            action.pressEnter()

            MusicManager.loopMusic("Freezing Breath")
            println("When the light fades, you find yourself standing at the entrance of the Crystal Cavern, surrounded by shimmering crystals.")
            println("Cypher stands beside you, her eyes filled with purpose.")
            action.pressEnter()
            println("Cypher: 'The Crystal Cavern holds secrets long forgotten, and the path ahead is fraught with challenges. But remember: not everything is as it seems.'")
            println("Cypher: 'Like the Forgotten Library, this Cavern holds secrets buried beneath layers of time. The path to the Heart of Glass is deceptive, but every puzzle has its truth.'")
            action.pressEnter()
            println("Cypher: 'Trust your instincts, and be wary of the illusions time can weave.'")
            action.pressEnter()
            println("With renewed determination, you step forward into the Crystal Cavern, ready to face whatever lies ahead.")
            println("What will you do now?")
            player.selfRecovery()

            player.travelTo(crystalCavern, chasm)
            player.addLocation(crystalCavern)
            player.addArea(crystalCavern, chasm)

            currentAct = 2

          case 2 =>

            while (!player.itemList.contains(cavernKeyItem)) do
              if (player.itemList.contains(hallFragment) &&
                player.itemList.contains(abyssFragment) &&
                player.itemList.contains(grottoFragment) &&
                player.itemList.contains(veinFragment)) then
                println("You noticed all 4 fragments light up when you put them close together in your inventory.")
                println(s"$hallFragment, $abyssFragment, $grottoFragment and $veinFragment have fused into $cavernKeyItem!")
                player.removeItem(hallFragment)
                player.removeItem(abyssFragment)
                player.removeItem(grottoFragment)
                player.removeItem(veinFragment)
                player.acquireItem(cavernKeyItem)
                println("You feel like you can unlock The Heart Of Glass area now.")
              else
                val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  MusicManager.loopMusic("Game Over")
                  if (gameOverPrompt(crystalCavern, chasm)) then
                    player.selfRecovery()
                    player.travelTo(crystalCavern, chasm)
                    println("What will you do now?")
                    isGameOver = false
                  else
                    continueGame = false
                    break()

            while (!cavernBoss.isDefeated) do
              val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  MusicManager.loopMusic("Game Over")
                  if (gameOverPrompt(crystalCavern, chasm)) then
                    player.selfRecovery()
                    player.travelTo(crystalCavern, chasm)
                    println("What will you do now?")
                    isGameOver = false
                  else
                    continueGame = false
                    break()

            MusicManager.loopMusic("Freezing Breath")
            println(s"With a final, echoing roar, the ${cavernBoss.bossName} crumbles to the ground.")
            println("The air vibrates with energy as the ground quakes beneath you.")
            println("You have triumphed over this embodiment of despair, yet its defeat feels merely like the first step.")
            println("As the dust settles, a radiant object glimmers before you—a Chrono Prism, waiting to be claimed.")
            action.pressEnter()

            println("Cypher materializes from the shadows, her expression a mix of admiration and concern.")
            println("Cypher: 'You have done well, Traveler. The Chrono Prism holds the power to unlock your next path, the Time-Twisted Forest.'")
            println("Cypher: 'But heed my warning—beyond this point, dangers lurk that are beyond comprehension.'")
            action.pressEnter()

            println("In a flash of light, the ground beneath you shifts, a vortex swirling around you.")
            println("Reality bends and fractures until you find yourself engulfed in a swirling void.")
            action.pressEnter()

            MusicManager.loopMusic("Silence")
            println("Cypher: 'Welcome to the Time-Twisted Forest, a realm where memories and shadows intertwine. Here, illusions and truth dance together, and your mind will be tested.'")
            println("Cypher: 'But remember, Traveler, beyond these woods lies a far greater threat—one who seeks to unravel the very fabric of your journey.'")
            println("With her voice lingering in the air, she fades into the shadows, leaving you to face what lies ahead.")
            action.pressEnter()

            println("The forest stands before you, cloaked in mystery and whispers of forgotten tales.")
            println("Cypher's last words echo: 'Solve the riddles, untangle the logic puzzles, and the path will reveal itself.'")
            action.pressEnter()

            println("What will you do now?")

            player.selfRecovery()
            player.travelTo(timeTwistedForest, clearing)
            player.addLocation(timeTwistedForest)
            player.addArea(timeTwistedForest, clearing)
            currentAct = 3

          case 3 =>
            player.travelTo(timeTwistedForest, clearing)
            player.addLocation(timeTwistedForest)
            player.addArea(timeTwistedForest, clearing)
            while (!player.itemList.contains(forestKeyItem)) do
              if (player.itemList.contains(grooveEssence) &&
                player.itemList.contains(thicketEssence) &&
                player.itemList.contains(springEssence) &&
                player.itemList.contains(gladeEssence)) then
                println("You noticed all 4 essences light up when you put them close together in your inventory.")
                println(s"$grooveEssence, $thicketEssence, $springEssence and $gladeEssence have fused into $forestKeyItem!")
                player.removeItem(grooveEssence)
                player.removeItem(thicketEssence)
                player.removeItem(springEssence)
                player.removeItem(gladeEssence)
                player.acquireItem(forestKeyItem)
              else
                val input = readLine()
                getStringCommand(player, input)
                if isGameOver then
                  MusicManager.loopMusic("Game Over")
                  if (gameOverPrompt(timeTwistedForest, clearing)) then
                    player.selfRecovery()
                    player.travelTo(timeTwistedForest, clearing)
                    println("What will you do now?")
                    isGameOver = false
                  else
                    continueGame = false
                    break()

            println("You feel like you can unlock the Broken Hourglass area now.")
            while (!forestBoss.isDefeated) do
              val input = readLine()
              getStringCommand(player, input)
              if isGameOver then
                MusicManager.loopMusic("Game Over")
                if (gameOverPrompt(timeTwistedForest, clearing)) then
                  player.selfRecovery()
                  player.travelTo(timeTwistedForest, clearing)
                  println("What will you do now?")
                  isGameOver = false
                else
                  continueGame = false
                  break()

            MusicManager.loopMusic("Silence")
            println("\nWith a final, anguished roar, the forest boss collapses, its form disintegrating into ethereal wisps.")
            println("A heavy silence falls, but a strange resonance lingers in the air, a sound both haunting and familiar.")
            println("Suddenly, the ground shakes, and cracks of blinding light appear beneath you, illuminating forgotten truths.")
            action.pressEnter()

            println("From the depths of the forest, a shadowy figure emerges, wreathed in the remnants of shattered time.")
            println("It is a reflection of your own past mistakes, a being formed from the echoes of choices you have made.")
            action.pressEnter()

            println("Cypher materializes beside you, her gaze locked onto the entity. 'This is the true nature of your journey.'")
            println("'Every decision has led to this moment, where the threads of fate intertwine, revealing the darkness you must confront.'")
            action.pressEnter()

            println("As the figure raises its hand, a wave of energy pulses outward, distorting reality and pulling you toward the unknown.")
            println("'You cannot escape the consequences of your actions,' it intones, its voice a chilling whisper that reverberates through the air.")
            action.pressEnter()

            println("The air grows heavy, and you feel your essence being drawn into the light, fracturing your perception of time and space.")
            println("Before you can react, Cypher grabs your arm, her grip firm yet reassuring. 'You must rise above this darkness!'")
            action.pressEnter()

            println("In a flash, the world around you shatters into countless pieces, and you find yourself standing in a vast expanse filled with swirling voids.")
            println("This realm, a tapestry woven from the remnants of countless timelines, pulses with an unsettling energy.")
            action.pressEnter()

            println("Each flicker of light represents a choice made, a life lived, and a path taken, echoing the weight of untold stories.")
            println("'Gather the remnants of your courage and confront the ultimate truth,' Cypher urges. 'The Dark Nexus awaits, and only you can seal it.'")
            println("As the light begins to dim, Cypher’s voice shifts, revealing a deeper resonance, almost like an echo from another time.")
            action.pressEnter()

            println("'But first, you must understand: I am not just your guide. I am a fragment of your past, a part of your lost memories.'")
            println("'You were chosen to face the darkness within yourself, to unearth the truth hidden in the depths of your heart.'")
            println("'The Dimensional Rift was created to test those who have strayed from their path, and you are its chosen Traveler.'")
            println("'With each step you take, you draw closer to your own fate—a fate entwined with mine.'")
            action.pressEnter()

            println("'But there is more you must recover, more you must reclaim. \n" +
              "The Artifact of Stability—an ancient relic lost to the void—holds the power to restore balance to the fractured timelines.'")
            println("'Without it, the Dimensional Rift will continue to unravel, and the chaos will consume not only this world but all realities bound to it.'")
            println("'The Artifact has long been guarded by forces beyond your comprehension, but you are the one destined to reclaim it.'")
            action.pressEnter()

            println("At this moment, the Temporal Shard in your inventory shimmers into existence, its glow marking the entrance to the Dark Nexus.")
            println("'This shard serves as your gateway, but remember: it is the Artifact of Stability that will mend the rift you seek to close.'")
            action.pressEnter()

            println("'It is my purpose to lead you to the Dark Nexus, where the true final boss awaits—an embodiment of your greatest fears, born of your deepest regrets.'")
            action.pressEnter()

            println("The energy around you fluctuates, revealing visions of your past: moments of joy, pain, and choices that haunt you still.")
            println("You stand at the precipice of an unimaginable challenge, your resolve tested more than ever as you prepare for the final act.")
            println("Only by confronting your past, and by retrieving the Artifact of Stability, can you hope to restore the balance and survive what lies ahead.")

            action.pressEnter()
            player.selfRecovery()

            println("\nYou steel yourself as you step forward, your resolve unshaken but your heart heavy with the weight of what is to come.")
            println("\nThe ground beneath you shatters, and your vision blurs as you're pulled into the heart of the Dark Nexus.")
            action.pressEnter()

            player.travelTo(darkNexus, nexus)
            player.addLocation(darkNexus)
            player.addArea(darkNexus, nexus)

            MusicManager.loopMusic("Shadowlord")
            println("You find yourself standing in a vast, shadowy expanse, where time and space seem to ripple unnaturally.")
            println("Cypher materializes beside you once more, her form flickering like an old memory.\n" +
              " 'This is it. The final truth.'")
            action.pressEnter()
            println("Cypher's gaze intensifies as she gestures to the swirling darkness around you.\n 'We are now within the Nexus of Reflection, " +
              "a place deep within the Dark Nexus itself.\n This is where your past, your choices, and your fears all converge.'")
            action.pressEnter()
            println("'The Nexus of Reflection holds no illusions. \n" +
              "Here, you will confront the truth of who you are and the path you've walked.\n" +
              " There is no escape, Traveler.'")
            action.pressEnter()

            println("'Beyond this lies the embodiment of your deepest fears—your final trial. ")
            println("Every step, every choice, has led you to this moment. \n" +
              "Now, you must face what you have become and uncover the truth hidden within.'")
            action.pressEnter()

            println("The final confrontation will begin soon.")
            println("Are you ready?")
            println("1. Yes, I'm so ready!")
            println("2. No, I need to prepare.")
            val inputInt = action.getInputNumberNoPrinting((1 to 2).toVector)

            inputInt match
              case 1 =>
                println("Cypher's voice echoes in your mind: 'Courage, Traveler. Your fate is sealed, and the hour of reckoning is upon you.'")
                println("With determination in your heart, you step forward into the unknown, ready to face your destiny.")
                action.pressEnter()
                println("\nThe air around you hums with energy as the darkness begins to stir.")
                println("A swirling mass of shadows starts to take form in front of you—a figure that shifts between the faces of your past.")
                println("'This is the embodiment of all you’ve feared, all you've tried to forget,' Cypher whispers.")
                action.pressEnter()

                println("The figure looms larger, its eyes gleaming with a terrible knowledge. 'Face your truth, Traveler,' it hisses, 'or be consumed by it.'")
                action.pressEnter()

                println("The dark energy around you intensifies, pulling you into the final battle.")
                println("Cypher's voice echoes in your mind: 'You are not alone in this. I am part of you. Use everything you've learned.'")
                action.pressEnter()

              case 2 =>
                println("Cypher sighs, her voice softer now: 'Even when doubt lingers, the path forward is already written in the stars.'")
                println("Take your time to prepare, Traveler. When you are ready, type 'ready' to face your destiny.")

                var ready = false
                while !ready do
                  println("You can prepare by managing your inventory, using items, changing weapons, etc.")
                  println("Type 'ready' when you are prepared to begin the final battle.")

                  while !ready do
                    val input = readLine().trim.toLowerCase()
                    if input == "ready" then
                      ready = true
                      println("Cypher's voice steadies: 'Now, there is no turning back. The moment of reckoning has come.'")
                      action.pressEnter()
                    else
                      if input != "go" && input != "travel" && input != "return" then
                        getStringCommand(player, input)
                      else
                        println("You cannot use that command now.")

            currentAct = 4

          case 4 =>

            println("You step forward, the air growing dense and heavy as a shimmering 'Veil of Forgotten Echoes' stands before you.")
            println("Cypher steps to your side, her voice soft but resolute. 'Beyond this veil lies your ultimate test.'")
            action.pressEnter()

            println("Cypher’s voice resonates within your mind as she steps forward beside you. 'In this final battle, I will lend you my strength.'")
            println("'I can aid you in this battle, but remember, the victory is yours. Use your CP wisely to harness my skills and secure your triumph.'")
            action.pressEnter()

            println("With a deep breath, you press forward, pushing through the veil as its ethereal form fades around you.")
            action.pressEnter()

            println("The Veil of Forgotten Echoes dissipates, revealing a vast, darkened arena where the final battle will unfold.")
            println("Shadows twist in every corner, the very fabric of reality warping as your final challenge awaits.")

            action.pressEnter()

            val finalBossEncounter = new FinalBossEncounter(player, finalBoss)
            if (!finalBossEncounter.start()) then
              isGameOver = true
            while (!finalBoss.isDefeated) do
              if isGameOver then
                MusicManager.loopMusic("Game Over")
                if (gameOverPrompt(dimensionalNexus, entrance)) then
                  player.selfRecovery()
                  player.travelTo(dimensionalNexus, entrance)
                  println("Use 'travel' command to retry the final fight.")
                  isGameOver = false
                else
                  continueGame = false
                  break()
              val input = readLine()
              getStringCommand(player, input)

            if (finalBoss.isDefeated) then
              println("Congratulations! You've defeated the final boss!")

    println("Game finished.")

}
