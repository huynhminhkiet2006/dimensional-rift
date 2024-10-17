import scala.io.StdIn.readLine

class BossEncounter(player: Player, boss: Boss):

  val action = new GameAction

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
    println(s"A level ${boss.level} ${boss.name} appears!\n")
    action.pressEnter()
    boss.bossYapping()
    combatLoop()

  private def combatLoop(): Boolean =
    setUp()
    while (player.HP > 0 && boss.HP > 0) do
      val playerActed = playerTurn()
      if (playerActed && player.HP > 0 && boss.HP > 0) then
        bossTurn()
        println("Press Enter to continue...")
        readLine()
    if (player.HP > 0) then
      MusicManager.playMusicOnce("BossVictory")
      println("You defeated the boss!\n")
      boss.isDefeated = true
      println("Press Enter to see rewards...")
      readLine()
      postCombat()
      action.pressEnter()
      player.updateHP(player.HP)
      player.updateSP(player.SP)
      true
    else
      println("You were defeated...")
      false

  private def playerTurn() =

    println(s"[Your Stats: HP: ${player.HP}, SP: ${player.SP}]")
    println(s"[Boss Health: ${boss.HP}]")
    println("What will you do?")
    println("1. Attack")
    println("2. Use Ability")
    println("3. Defend")
    println("4. Use Item")
    println("5. Auto battle")

    val input = action.getInputNumberNoPrinting((1 to 5).toVector)

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
      println("You don't have any items to use now.\n")
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

  private def bossTurn() =
    println(s"\n${boss.name}'s turn!")
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

  private def postCombat() =
    
    player.gainXP(boss.dropXP())
    player.acquireWeapon(boss.bossCurrentWeapon)
    println(s"You acquired a new rare weapon: ${boss.bossCurrentWeapon.name}!")
    player.acquireItem(boss.bossDropItem)
    boss.bossCurrentWeapon.displayStat()
