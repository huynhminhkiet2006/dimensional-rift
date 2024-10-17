import scala.io.StdIn.*

class FinalBossEncounter(player: Player, boss: FinalBoss):

  val action = new GameAction
  var currentPhase = 1
  var CP = 250

  def setUp() =
    player.HP = player.currentHP
    player.SP = player.currentSP
    player.atkPow = player.attackPower
    player.defPow = player.defensePower
    player.maxHP2 = player.maxHP
    player.maxSP2 = player.maxSP
    player.weapon = player.currentWeapon

    boss.HP = boss.maxHP
    boss.SP = boss.maxSP

  def start(): Boolean =
    boss.bossYappingPhase1()
    MusicManager.loopMusic("FinalBossTheme")
    println("\nBattle start!\n")
    combatLoop()

  //Handling Cypher skills
  var turnCount = 0
  var currentTurn1 = 0
  var currentTurn3 = 0
  var currentTurn4 = 0

  var tempBoost = false
  var defendingCheck = false
  var checkDodge = false
  var actTwice = false

  private def combatLoop(): Boolean =
    setUp()
    while (player.HP > 0 && boss.HP > 0) do
      var playerActed = playerTurn()
      if actTwice then
        playerActed = playerTurn()
        if turnCount - currentTurn4 == 3 then
          actTwice = false
          println("Double turn ended.")
      if tempBoost then
        player.boostAtk()
        player.boostDef()
        tempBoost = false
      if turnCount - currentTurn1 == 3 then
        player.resetAtkBuff()
        player.resetDefBuff()
      if defendingCheck then
        player.defend()
        if turnCount - currentTurn3 == 3 then
          defendingCheck = false
          player.resetDefending()
          println("Echo Shield ended.")

      if (playerActed && player.HP > 0 && boss.HP > 0) then
        bossTurn()
        action.pressEnter()

      turnCount += 1

    if (player.HP > 0) then
      println("You defeated the boss!\n")
      boss.isDefeated = true
      postCombat()
      player.updateHP(player.HP)
      player.updateSP(player.SP)
      action.pressEnter()
      true
    else
      println("You were defeated...")
      false

  private def playerTurn() =

    println(s"[Your Stats: HP: ${player.HP}, SP: ${player.SP}, CP: $CP]")
    println(s"[Boss Health: ${boss.HP}]")
    println("What will you do?")
    println("1. Attack")
    println("2. Use Ability")
    println("3. Defend")
    println("4. Use Item")
    println("5. Auto battle")
    println("6. Cypher skill")

    val input = action.getInputNumberNoPrinting((1 to 6).toVector)

    input match
      case 1 =>
        player.attack(boss)
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
      case 6 =>
        val skillUsed = cypherSkill()
        skillUsed

  private def autoBattleLoop() =
    var autoBattleOn = true
    while (autoBattleOn && player.HP > 0 && boss.HP > 0) do
      Thread.sleep(500)
      println(s"\n${player.name}'s turn!")
      player.attack(boss)
      if boss.HP > 0 then
        bossTurn()
      if player.HP <= 0 || boss.HP <= 0 then
        autoBattleOn = false
        println("Auto battle has ended.")

  private def cypherSkill(): Boolean =

    println("Choose a skill (or type 'cancel' to go back):")
    println("1. Memory Surge: Fully recovers HP, increases attack and defense for three turns. Cost: 50 CP")
    println("2. Echo Shield: Reduces damage taken for three turns. Cost: 50 CP")
    println("3. See Through: 100% chance of dodging the next attack from the enemy and moderately recovers HP. Cost: 60 CP")
    println("4. Chrono Strike: Act twice per turn for three turns. Cost: 75 CP")

    val skillCost = Vector[Int](0, 90, 50, 60, 75)

    val input = readLine().trim

    if (input.equalsIgnoreCase("cancel")) then
      false
    else
      try
        val choice = input.toInt
        if (choice < 1 || choice > 4) then
          println("Invalid choice. Please choose a valid skill.")
          false
        else
          if CP < skillCost(choice) then
            println("You don't have enough CP!")
            false
          else
            choice match
              case 1 =>
                tempBoost = true
                currentTurn1 = turnCount
                player.heal(player.maxHP)
                println("Cypher's Memory Surge fully recovers your HP and bolsters your strength!")
              case 2 =>
                defendingCheck = true
                println("Cypher conjures an Echo Shield, reducing incoming damage for the next 3 turns!")
              case 3 =>
                checkDodge = true
                player.heal(player.maxHP * 45 / 100)
                println("Cypher's vision sharpens your reflexes, allowing you to dodge the next attack and heal!")
              case 4 =>
                actTwice = true
                currentTurn4 = turnCount
                println("Cypher manipulates time, granting you two actions per turn for the next 3 turns!")

            CP -= skillCost(choice)
            true

      catch
        case _: NumberFormatException =>
          println("Please enter a valid number or 'cancel' to go back.")
          false


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
          if (!player.useAbility(selectedAbility, boss)) then
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
      println("You don't have any items to use now.")
      false
    else
      println("Choose an item (or type 'cancel' to go back):")
      player.getItemUsableInBattle.toList.zipWithIndex.foreach { case ((item, count), index) =>
        println(s"${index + 1}. ${item} x$count")
      }

      val input = readLine().trim

      if (input.equalsIgnoreCase("cancel")) then
        false
      else
        try
          val choice = input.toInt
          if (choice < 1 || choice > player.getItemUsableInBattle.size) then
            println("Invalid choice. Please choose a valid ability.")
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

  private def bossTurn() =
    println(s"${boss.name}'s turn!")
    if checkDodge then
      println(s"${boss.name} misses the attack!")
      checkDodge = false
    else
      if (currentPhase == 1) then
        val action = boss.enemyUseElemental(player)
        if (!action) then boss.attack(player)
      else
        val action = boss.enemyDecision(boss.HP.toDouble / boss.maxHP.toDouble)
        action match
          case "attack" =>
            boss.attack(player)
          case "physical" =>
            val abilityUsed = boss.enemyUsePhysical(player)
            if (!abilityUsed) then boss.attack(player)
          case "elemental" =>
            val abilityUsed = boss.enemyUseElemental(player)
            if (!abilityUsed) then boss.attack(player)
          case "healing" =>
            val abilityUsed = boss.enemyUseHealing(player)
            if (!abilityUsed) then boss.attack(player)
          case "defend" =>
            boss.defend()

    if (currentPhase == 1 && boss.HP <= boss.maxHP / 2) then {
      boss.bossYappingPhase2()
      println("Press Enter to continue...")
      readLine()
      println("The darkness surrounding the boss grows stronger. It begins to use all of its powers!\n")
      currentPhase = 2
    }

  private def postCombat() =

    player.gainXP(boss.dropXP())
    player.acquireWeapon(boss.bossCurrentWeapon)
    println(s"You acquired a new legendary weapon: ${boss.bossCurrentWeapon.name}!")
    player.acquireItem(boss.bossDropItem)
    boss.bossCurrentWeapon.displayStat()


