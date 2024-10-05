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

//Library
def shadowLibrarian = new Enemy("Shadow Librarian", 50, 50, 10, 10, 10, 3, golfClub, Buffer[Item](medicine), Buffer[Ability](bufu, agi, garu))
def ancientTome = new Enemy("Ancient Tome", 70, 70, 15, 20, 10, 6, golfClub, Buffer[Item](ointment, healingI), Buffer[Ability](garu, assaultDrive, sonicPunch))
def curatorGuardian = new Enemy("Curator's Guardian", 90, 90, 20, 15, 20, 7, longSword, Buffer[Item](snuffSoul, ointment), Buffer[Ability](zio, assaultDrive))
def echoWrath = new Enemy("Echo Wraith", 100, 100, 25, 15, 25, 10, bastardSword, Buffer[Item](snuffSoul, ointment, healingI), Buffer[Ability](dia, agi, bufu, garu, zio, assaultDrive))
def dimensionalShade = new Enemy("Dimensional Shade", 60, 60, 10, 14, 10, 5, golfClub, Buffer[Item](soulDrop), Buffer[Ability](bufu, zio, dia, sonicPunch))

//Cavern
def abyssWatcher = new Enemy("Abyss Watcher", 120, 120, 30, 26, 18, 11, titaniumClub, Buffer[Item](medicine, muscleDrink), Buffer[Ability](agilao, zionga, powerSlash))
def shardStalker = new Enemy("Shard Stalker", 160, 160, 45, 32, 26, 14, gothicSword, Buffer[Item](lifeStone, chewingSoul, recovR200), Buffer[Ability](bufula, zionga, fatalEnd))
def lightWarden = new Enemy("Light Warden", 220, 220, 55, 35, 35, 18, type98Gunto, Buffer[Item](snuffSoul, healingI, healingII), Buffer[Ability](dia, diarama, powerSlash, garula))
def frostboundSentinel = new Enemy("Frostbound Sentinel", 275, 275, 70, 45, 45, 23, edge, Buffer[Item](pulsatingStone, lifeStone, healingI), Buffer[Ability](diarama, blackSpot, agilao, garula))
def shardgeist = new Enemy("Shardgeist", 250, 250, 60, 28, 36, 20, kageDachi, Buffer[Item](ointment, recovR200, pulsatingStone, healingII), Buffer[Ability](bufula, blackSpot, diarama, fatalEnd))

//Forest
def chronoBeast = new Enemy("Chrono Beast", 300, 300, 70, 40, 50, 27, midareHamon, Buffer[Item](lifeStone, moonDango), Buffer[Ability](bufudyne, diarama, braveBlade))
def timeboundWrath = new Enemy("Abyss Watcher", 370, 370, 75, 65, 65, 31, kenka, Buffer[Item](moonDango, takemedic, sadayoTayaki), Buffer[Ability](agidyne, garudyne, blackSpot, giganticFist))
def echoingPhantom = new Enemy("Echoing Phantom", 450, 450, 85, 65, 80, 36, metalBat, Buffer[Item](snuffSoul, healingII, recovR200), Buffer[Ability](ziodyne, vileAssault, agidyne, diarahan))
def temporalLurker = new Enemy("Temporal Lurker", 540, 540, 110, 80, 75, 40, kijintou, Buffer[Item](healingIII, sadayoTayaki, bead), Buffer[Ability](agidyne, garudyne, bufudyne, ziodyne, megidola, vileAssault))
def aegisOfAges = new Enemy("Aegis Of Ages", 600, 600, 100, 90, 90, 45, kenka, Buffer[Item](sakuraAmezaiku, healingIII, bead), Buffer[Ability](agidyne, garudyne, bufudyne, braveBlade))