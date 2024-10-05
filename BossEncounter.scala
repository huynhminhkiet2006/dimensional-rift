import scala.io.StdIn.readLine

class BossEncounter(player: Player, boss: Boss):

  val action = new GameAction

  def start(): Boolean =
    println(s"A level ${boss.level} ${boss.name} appears!\n")
    boss.bossYapping()
    combatLoop()

  private def combatLoop(): Boolean =
    while (player.HP > 0 && boss.HP > 0) do
      val playerActed = playerTurn()
      if (playerActed && boss.HP > 0) then
        bossTurn()
        readLine()
    if (player.HP > 0) then
      println("You defeated the boss!\n")
      boss.isDefeated = true
      postCombat()
      true
    else
      println("You were defeated... Game Over.")
      false

  private def playerTurn() =

    println(s"[Your Stats: ${player.HP}/${player.SP}]")
    println(s"[boss Health: ${boss.HP}]")
    println("What will you do?")
    println("1. Attack")
    println("2. Use Skill")
    println("3. Defend")
    println("4. Use Item")

    val input = action.getInputNumberNoPrinting((1 to 3).toVector)

    input match
      case 1 =>
        player.attack(boss)
        true
      case 2 =>
        val skillUsed = useSkill()
        skillUsed
      case 3 =>
        player.defend()
        true
      case 4 =>
        val itemUsed = chooseItem()
        itemUsed

  private def useSkill(): Boolean =

    println("Choose an ability (or type 'cancel' to go back):")
    player.abilityList.zipWithIndex.foreach { case (skill, index) =>
      println(s"${index + 1}. ${skill.name}")
    }

    val input = readLine().trim

    if (input.equalsIgnoreCase("cancel")) then {
      false
    } else {
      try {
        val choice = input.toInt
        if (choice < 1 || choice > player.abilityList.size) then {
          println("Invalid choice. Please choose a valid ability.")
          false
        } else {
          val selectedSkill = player.abilityList(choice - 1)
          if (!player.useAbility(selectedSkill, boss)) then {
            println("You do not have enough SP!")
            false
          } else {
            true
          }
        }
      } catch {
        case _: NumberFormatException =>
          println("Please enter a valid number or 'cancel' to go back.")
          false
      }
  }

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
    val action = boss.enemyDecision()
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
    println(s"You acquired a new important item: ${boss.bossDropItem.name}.")
