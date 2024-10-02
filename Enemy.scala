import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.util.Random

class Enemy(
  val name: String,
  val maxHP: Int,
  var currentHP: Int,
  var currentSP: Int,
  val attackPower: Int,
  val defensePower: Int,
  val level: Int,
  val currentWeapon: Weapon,
  val itemList: mutable.Buffer[Item],
  val abilityList: mutable.Buffer[Ability]) extends Character(name, maxHP, currentHP, currentSP, level, attackPower, defensePower, currentWeapon, abilityList):

  //For balancing purposes
  private val baseXP = 50
  private val variationPercentage = 0.10

  def enemyDecision(): String =

    val HPRatio = currentHP / maxHP
    val hasSP = SP > 0
    val randomValue = Random.nextDouble()

    if (HPRatio < 0.4 && randomValue < 0.7) then
      return "defend"

    if (HPRatio < 0.3 && randomValue < 0.5) then
      return "healing"

    if (hasSP && randomValue < 0.7) then
      return "elemental"

    if (HPRatio > 0.5 && randomValue < 0.7) then
      return "physical"

    "attack"

  def enemyUsePhysical(player: Player): Boolean =

    val availableAbilities = abilityList.filter(ability => ability.abilityType == "Physical").filter(ability => currentHP > maxHP * ability.abilityCost / 100)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    if (useAbility(chosenAbility, player)) then
      //println(s"${name} uses ${chosenAbility.name}!")
      true
    else
      false

  def enemyUseElemental(player: Player): Boolean =

    val availableAbilities = abilityList.filter(ability => ability.abilityType == "Elemental").filter(ability => ability.abilityCost <= SP)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    if (useAbility(chosenAbility, player)) then
      //println(s"${name} uses ${chosenAbility.name}!")
      true
    else
      false

  def enemyUseHealing(player: Player): Boolean =

    val availableAbilities = abilityList.filter(ability => ability.abilityType == "Healing").filter(ability => ability.abilityCost <= SP)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    if (useAbility(chosenAbility, player)) then
      //println(s"${name} uses ${chosenAbility.name}!")
      true
    else
      false

  def dropXP(): Int =

    val minXP = (baseXP * level * (1 - variationPercentage)).toInt
    val maxXP = (baseXP * level * (1 + variationPercentage)).toInt

    val finalXP = Random.between(minXP, maxXP + 1)

    finalXP

  def dropLoot(): Vector[Any] =

    //For balancing purposes
    val weaponDropChance = 0.9
    val abilityDropChance = 0.9
    val itemDropChance = 0.9
    var loot: Vector[Any] = Vector()

    if (Random.nextDouble() < weaponDropChance) then
      println(s"${name}'s weapon: ${currentWeapon} dropped!")
      loot = loot :+ currentWeapon


    if (Random.nextDouble() < abilityDropChance && abilityList.nonEmpty) then
      val droppedAbilities = abilityList.filter(_ => Random.nextBoolean())
      if (droppedAbilities.nonEmpty) then
        println(s"${name}'s abilities: ${droppedAbilities.mkString(" ")} dropped!")
        loot = loot ++ droppedAbilities

    if (Random.nextDouble() < itemDropChance && itemList.nonEmpty) then
      val droppedItems = itemList.filter(_ => Random.nextBoolean())
      if (droppedItems.nonEmpty) then
        println(s"${name}'s items: ${droppedItems.mkString(" ")} dropped!")
        loot = loot ++ droppedItems

    if (loot.isEmpty) then
      println(s"${name} dropped nothing.")

    loot

def shadowLibrarian = new Enemy("Shadow Librarian", 50, 50, 10, 10, 10, 3, golfClub, Buffer[Item](medicine), Buffer[Ability](bufu))
def ancientTome = new Enemy("Ancient Tome", 70, 70, 15, 15, 15, 6, golfClub, Buffer[Item](ointment), Buffer[Ability](agi, sonicPunch))
def curatorGuardian = new Enemy("Curator's Guardian", 90, 90, 20, 17, 17, 7, longSword, Buffer[Item](snuffSoul, ointment), Buffer[Ability](zio, sonicPunch))
def echoWrath = new Enemy("Echo Wraith", 100, 100, 25, 20, 20, 10, bastardSword, Buffer[Item](snuffSoul, ointment, healingI), Buffer[Ability](dia, agi, sonicPunch))
def dimensionalShade = new Enemy("Dimensional Shade", 60, 60, 10, 12, 12, 5, golfClub, Buffer[Item](soulDrop), Buffer[Ability](bufu, zio))



