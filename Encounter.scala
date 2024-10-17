import scala.collection.mutable
import scala.io.StdIn.*

class Encounter(player: Player, enemy: Enemy):

  val action = new GameAction

  def explainBattleSystem() =

    println("\nAs you face your first enemy, Cypher steps forward from the shadows, her voice calm and instructive.")
    println("Cypher: 'This is your first battle, Traveler, so allow me to explain how combat works in this realm.'")
    action.pressEnter()

    println("Cypher: 'Combat is turn-based. You and your enemy will take turns attacking or using abilities.'")

    println("Cypher: 'Your attack and defense stats will influence how much damage you deal and receive.\n " +
      "Your HP (Health Points) represents your life force, and if it reaches zero, the battle is over for you.'")
    println("Cypher: 'SP (Skill Points) are consumed when using abilities. \n" +
      "Be mindful of your SP, as running out will limit your ability to use powerful skills.'")

    action.pressEnter()

    println("Cypher: 'Your key options in battle are: '")
    println("  1. Attack: Use your weapon to deal damage to the enemy.")
    println("  2. Abilities: Use your special abilities, like spells or techniques, which often have unique effects.\n" +
      "Keep in mind that abilities require either HP or SP to use.")
    println("  3. Defend: 'This option lets you brace yourself for the enemy’s next attack, reducing the damage you take during their turn.\n " +
      "It's helpful when you're low on health or facing a strong opponent.'")
    println("  4. Use Item: 'In battle, you can use items from your inventory.'")
    println("  5. Auto battle: 'You can let your instincts take over and automatically attack until the battle is over.\n" +
      "However, auto battle cannot be canceled once started, so consider carefully before choosing this option.'")

    action.pressEnter()

    println("Cypher: 'Most enemy you encounter has a weakness.\n" +
      " Your job is to find and exploit that weakness to gain the upper hand in battle.'")
    println("Cypher: 'Certain abilities will deal extra damage if you can figure out what the enemy is weak against. \n" +
      "  Experiment with your abilities and weapons to uncover these vulnerabilities.'")
    action.pressEnter()

    println("Cypher: 'When it’s your turn, consider your options carefully. \n" +
      "Victory isn’t always about raw strength—strategy and timing are just as important.'")
    println("Cypher: 'Now, it’s time to put what you’ve learned to the test. \n" +
      "Trust your instincts, Traveler, and may your courage guide your blade.'")
    action.pressEnter()

  def setUp() =
    player.HP = player.currentHP
    player.SP = player.currentSP
    player.atkPow = player.attackPower
    player.defPow = player.defensePower
    player.maxHP2 = player.maxHP
    player.maxSP2 = player.maxSP
    player.weapon = player.currentWeapon




  def start(): Boolean =
    println(s"A level ${enemy.level} ${enemy.name} appears!")
    println(s"Press Enter to start battle...")
    readLine()
    if !player.hasEncounteredEnemy then
      explainBattleSystem()
      player.hasEncounteredEnemy = true
    combatLoop()


  var turnCount = 0
  //From left to right: AtkBuff, AtkDebuff, DefBuff, DefDebuff
  val playerCount = mutable.Buffer(0, 0, 0, 0)
  val enemyCount = mutable.Buffer(0, 0, 0, 0)
  val playerFlag = mutable.Buffer(false, false, false, false)
  val enemyFlag = mutable.Buffer(false, false, false, false)

  private def playerCheck(index: Int) =
    index match
      case 0 => player.checkAtkBuff
      case 1 => player.checkAtkDebuff
      case 2 => player.checkDefBuff
      case 3 => player.checkDefDebuff

  private def enemyCheck(index: Int) =
    index match
      case 0 => enemy.checkAtkBuff
      case 1 => enemy.checkAtkDebuff
      case 2 => enemy.checkDefBuff
      case 3 => enemy.checkDefDebuff

  private def playerReset(index: Int) =
    index match
      case 0 => player.resetAtkBuff()
      case 1 => player.resetAtkDebuff()
      case 2 => player.resetDefBuff()
      case 3 => player.resetDefDebuff()

  private def enemyReset(index: Int) =
    index match
      case 0 => enemy.resetAtkBuff()
      case 1 => enemy.resetAtkDebuff()
      case 2 => enemy.resetDefBuff()
      case 3 => enemy.resetDefDebuff()

  private def combatLoop(): Boolean =

    setUp()
    while (player.HP > 0 && enemy.HP > 0) do

      val playerActed = playerTurn()

      for i <- 0 to 3 do
        if !playerFlag(i) then
          if playerCheck(i) then
            playerFlag(i) = true
            playerCount(i) = turnCount
        else
          if turnCount - playerCount(i) == 3 then
            playerReset(i)
            playerFlag(i) = false

      if (playerActed && player.HP > 0 && enemy.HP > 0) then

        enemyTurn()

        for i <- 0 to 3 do
        if !enemyFlag(i) then
          if enemyCheck(i) then
            enemyFlag(i) = true
            enemyCount(i) = turnCount
        else
          if turnCount - playerCount(i) == 3 then
            enemyReset(i)
            enemyFlag(i) = false

        action.pressEnter()

      turnCount += 1

    if (player.HP > 0) then
      MusicManager.loopMusic("Victory")
      println("You defeated the enemy!\n")
      println("Press Enter to see rewards...")
      readLine()
      postCombat()
      player.updateHP(player.HP)
      player.updateSP(player.SP)
      action.pressEnter()
      println("What will you do now?")
      true
    else
      println("You were defeated...")
      false

  private def playerTurn() =

    println(s"[Your Stats: HP: ${player.HP}, SP: ${player.SP}]")
    println(s"[Enemy Health: ${enemy.HP}]")
    println(playerFlag)
    println(enemyFlag)
    println("What will you do?")
    println("1. Attack")
    println("2. Use Ability")
    println("3. Defend")
    println("4. Use Item")
    println("5. Auto battle")

    val input = action.getInputNumberNoPrinting((1 to 5).toVector)

    input match
      case 1 =>
        player.attack(enemy)
        true
      case 2 =>
        val abilityUsed = useAbility()
        abilityUsed
      case 3 =>
        player.defend()
        true
      case 4 =>
        val itemUsed = chooseItem()
        itemUsed
      case 5 =>
        println("Auto battle engaged! The player will attack until the battle is over.")
        autoBattleLoop()
        true

  private def autoBattleLoop() =

    var autoBattleOn = true
    while (autoBattleOn && player.HP > 0 && enemy.HP > 0) do
      Thread.sleep(500)
      println(s"\n${player.name}'s turn!")
      player.attack(enemy)
      if enemy.HP > 0 then
        enemyTurn()
      if player.HP <= 0 || enemy.HP <= 0 then
        autoBattleOn = false
        println("Auto battle has ended.")

  private def useAbility(): Boolean =

    println("Choose an ability (or type 'cancel' to go back):")
    player.abilityList.zipWithIndex.foreach:
      case (ability, index) =>
      if ability.abilityType == "Physical" then
        println(s"${index + 1}. ${ability.name} (${ability.abilityType}): ${player.maxHP * ability.abilityCost / 100} HP")
      else
        println(s"${index + 1}. ${ability.name} (${ability.abilityType}): ${ability.abilityCost} SP")


    val input = readLine().trim

    if (input.equalsIgnoreCase("cancel")) then
      false
    else
      try
        val choice = input.toInt
        if (choice < 1 || choice > player.abilityList.size) then
          println("Invalid choice. Please choose a valid ability.")
          false
        else
          val selectedAbility = player.abilityList(choice - 1)
          if (!player.useAbility(selectedAbility, enemy)) then
            if selectedAbility.abilityType == "Physical" then
              println("You do not have enough HP!")
            else
              println("You do not have enough SP!")
            false
          else
            true
      catch
        case _: NumberFormatException =>
          println("Please enter a valid number or 'cancel' to go back.")
          false

  private def chooseItem(): Boolean =
    if player.getItemUsableInBattle.isEmpty then
      println("You don't have any items to use now.\n")
      false
    else
      println("Choose an item (or type 'cancel' to go back):")
      player.getItemUsableInBattle.toList.zipWithIndex.foreach:
        case ((item, count), index) =>
        println(s"${index + 1}. ${item} x$count")

      val input = readLine().trim

      if (input.equalsIgnoreCase("cancel")) then
        false
      else
        try
          val choice = input.toInt
          if (choice < 1 || choice > player.getItemUsableInBattle.size) then
            println("Invalid choice. Please choose a valid item.")
            false
          else
            val selectedItem = player.getItemByNumber(player.getItemUsableInBattle, choice)
            selectedItem match
              case Some(item) =>
                player.useItem(item)
                true
              case None =>
                println("Item not found.")
                false
        catch
          case _: NumberFormatException =>
            println("Please enter a valid number or 'cancel' to go back.")
            false

  private def enemyTurn() =
    println(s"\n${enemy.name}'s turn!")
    val action = enemy.enemyDecision(enemy.HP.toDouble / enemy.maxHP.toDouble)
      action match
        case "buff"  =>
          val abilityUsed = enemy.enemyUseBuff(player)
          if (!abilityUsed) then enemy.attack(player)
        case "debuff" =>
          val abilityUsed = enemy.enemyUseDebuff(player)
          if (!abilityUsed) then enemy.attack(player)
        case "attack" =>
          enemy.attack(player)
        case "physical" =>
          val abilityUsed = enemy.enemyUsePhysical(player)
          if (!abilityUsed) then enemy.attack(player)
        case "elemental" =>
          val abilityUsed = enemy.enemyUseElemental(player)
          if (!abilityUsed) then enemy.attack(player)
        case "healing" =>
          val abilityUsed = enemy.enemyUseHealing(player)
          if (!abilityUsed) then enemy.attack(player)
        case "defend" =>
          enemy.defend()

  private def postCombat(): Unit =
    
    player.gainXP(enemy.dropXP())
    val loot = enemy.dropLoot()

    for element <- loot do

      element match
        case ability: Ability =>
          if (!player.abilityList.contains(ability)) then
            player.acquireAbility(ability)
            println(s"\nYou acquired a new ability: ${ability.name}!")
            println(s"${ability.name}: ${ability.description}.\n")
            println("")
          else
            println(s"You already have ${ability.name}.\n")

        case weapon: Weapon =>
          if (!player.weaponList.contains(weapon)) then
            player.acquireWeapon(weapon)
            println(s"\nYou acquired a new weapon: ${weapon.name}!")
            weapon.displayStat()
            println("")
          else
            println(s"You already have ${weapon.name}.\n")

        case item: Item =>
          player.acquireItem(item)
          println(s"\nYou acquired: ${item.name}.")
          item.description()
          println("")


