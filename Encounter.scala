import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

class Encounter(player: Player, enemy: Enemy) {

  val action = new GameAction

  def start(): Boolean = {
    println(s"A level ${enemy.level} ${enemy.name} appears!")
    combatLoop()
  }

  private def combatLoop(): Boolean = {
    while (player.HP > 0 && enemy.HP > 0) do {
      val playerActed = playerTurn()
      if (playerActed && enemy.HP > 0) then {
        enemyTurn()
        readLine()
      }
    }
    if (player.HP > 0) then {
      println("You defeated the enemy!")
      postCombat()
      player.updateHP(player.HP)
      player.updateSP(player.SP)
      true
    }
    else {
      println("You were defeated... Game Over.")
      false
    }
  }

  private def playerTurn() =

    println(s"[Your Stats: HP: ${player.HP}, SP: ${player.SP}]")
    println(s"[Enemy Health: ${enemy.HP}]")
    println("What will you do?")
    println("1. Attack")
    println("2. Use Skill")
    println("3. Defend")
    println("4. Use Item")

    val input = action.getInputNumberNoPrinting((1 to 4).toVector)

    input match
      case 1 =>
        player.attack(enemy)
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
    player.abilityList.zipWithIndex.foreach { case (ability, index) =>
      if ability.abilityType == "Elemental" || ability.abilityType == "Healing" then
        println(s"${index + 1}. ${ability.name} (${ability.abilityType}): ${ability.abilityCost} SP")
      else
        println(s"${index + 1}. ${ability.name} (${ability.abilityType}): ${player.maxHP * ability.abilityCost / 100} HP")
    }

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
            val selectedItem = player.getItemByNumber(player.getItemUsableInBattle, choice - 1)
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
    println(s"${enemy.name}'s turn!")
    val action = enemy.enemyDecision()
      action match
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

  private def postCombat(): Unit = {
    
    player.gainXP(enemy.dropXP())
    val loot = enemy.dropLoot()
    for element <- loot do
      element match {
        case ability: Ability =>
          if (!player.abilityList.contains(ability)) then
            player.acquireAbility(ability)
            println(s"You acquired a new ability: ${ability.name}!")
            println(s"${ability.name}: ${ability.description}.")
          else
            println(s"You already have ${ability.name}.")

        case weapon: Weapon =>
          if (!player.weaponList.contains(weapon)) then
            player.acquireWeapon(weapon)
            println(s"You acquired a new weapon: ${weapon.name}!")
            weapon.displayStat()
          else
            println(s"You already have ${weapon.name}.")

        case item: Item =>
          player.acquireItem(item)
          println(s"You acquired: ${item.name}.")
        case _ =>
          println("Unexpected loot type.")
    }

  }

}
