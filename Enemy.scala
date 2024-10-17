import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.util.Random

class Enemy(
  val name: String,
  val typee: String,
  val weakness: String,
  val maxHP: Int,
  var currentHP: Int,
  val maxSP: Int,
  var currentSP: Int,
  val attackPower: Int,
  val defensePower: Int,
  val level: Int,
  val currentWeapon: Weapon,
  val itemList: mutable.Buffer[Item],
  val abilityList: mutable.Buffer[Ability]) extends Character(name, typee, weakness, maxHP, currentHP, maxSP, currentSP, level, attackPower, defensePower, currentWeapon, abilityList):

  //For balancing purposes
  private val baseXP = 50
  private val variationPercentage = 0.10

  def enemyDecision(HPRatio: Double): String =

    val hasSP = SP > 0
    val randomValue = Random.nextDouble()

    if (HPRatio < 0.3 && randomValue < 0.3) then
      return "healing"

    if (HPRatio < 0.4 && randomValue < 0.3) then
      return "defend"

    if (hasSP && randomValue < 0.7) then
      return "elemental"

    if (hasSP && randomValue < 0.5) then
      return (if Random.nextDouble() > 0.5 then "buff" else "debuff")

    if (HPRatio > 0.5 && randomValue < 0.7) then
      return "physical"

    "attack"

  def enemyUsePhysical(player: Player): Boolean =

    val availableAbilities = abilityList.filter(_.abilityType == "Physical").filter(currentHP > maxHP * _.abilityCost / 100)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    useAbility(chosenAbility, player)

  def enemyUseElemental(player: Player): Boolean =

    val availableAbilities = abilityList
      .filter(_.abilityType != "Healing")
      .filter(_.abilityType != "Physical")
      .filter(_.abilityType != "Buff")
      .filter(_.abilityType != "Physical")
      .filter(_.abilityCost <= SP)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    useAbility(chosenAbility, player)


  def enemyUseHealing(player: Player): Boolean =

    val availableAbilities = abilityList.filter(_.abilityType == "Healing").filter(_.abilityCost <= SP)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    if (useAbility(chosenAbility, player)) then
      true
    else
      false

  def enemyUseDebuff(player: Player): Boolean =

    val availableAbilities = abilityList.filter(_.abilityType == "Debuff").filter(_.abilityCost <= SP)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    if (useAbility(chosenAbility, player)) then
      true
    else
      false

  def enemyUseBuff(player: Player): Boolean =

    val availableAbilities = abilityList.filter(_.abilityType == "Buff").filter(_.abilityCost <= SP)

    if (availableAbilities.isEmpty) then
      return false

    val chosenAbility = availableAbilities(Random.nextInt(availableAbilities.size))
    if (useAbility(chosenAbility, player)) then
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
        println(s"${name}'s abilities: ${if (droppedAbilities.nonEmpty) then droppedAbilities.mkString(", ") else ""} dropped!")
        loot = loot ++ droppedAbilities

    if (Random.nextDouble() < itemDropChance && itemList.nonEmpty) then
      val droppedItems = itemList.filter(_ => Random.nextBoolean())
      if (droppedItems.nonEmpty) then
        println(s"${name}'s items: ${if (droppedItems.nonEmpty) then droppedItems.mkString(", ") else ""} dropped!")
        loot = loot ++ droppedItems

    if (loot.isEmpty) then
      println(s"${name} dropped nothing.")

    loot

//Library
def shadowLibrarian = new Enemy("Shadow Librarian", "enemy", "Physical", 50, 50, 10, 10, 10, 10, 3, golfClub, Buffer[Item](medicine), Buffer[Ability](bufu, agi, garu))
def ancientTome = new Enemy("Ancient Tome", "enemy", "Fire", 70, 70, 15, 15, 20, 10, 6, golfClub, Buffer[Item](ointment, healingI), Buffer[Ability](agi, assaultDrive, sonicPunch))
def curatorGuardian = new Enemy("Curator's Guardian", "Wind", "enemy", 90, 90, 20, 20, 15, 20, 7, longSword, Buffer[Item](snuffSoul, ointment), Buffer[Ability](zio, assaultDrive))
def echoWrath = new Enemy("Echo Wraith", "enemy", "None", 100, 100, 25, 25, 15, 25, 10, bastardSword, Buffer[Item](snuffSoul, ointment, healingI), Buffer[Ability](dia, agi, bufu, garu, zio, assaultDrive))
def dimensionalShade = new Enemy("Dimensional Shade", "enemy", "Ice", 60, 60, 10, 10, 14, 10, 5, golfClub, Buffer[Item](soulDrop), Buffer[Ability](zio, dia, sonicPunch))

//Cavern
def abyssWatcher = new Enemy("Abyss Watcher", "enemy", "Wind", 120, 120, 30, 30, 26, 18, 11, titaniumClub, Buffer[Item](medicine, muscleDrink), Buffer[Ability](agilao, zionga, powerSlash, rakunda))
def shardStalker = new Enemy("Shard Stalker", "enemy", "Fire", 160, 160, 45, 45, 32, 26, 14, gothicSword, Buffer[Item](lifeStone, chewingSoul, recovR200), Buffer[Ability](bufula, zionga, fatalEnd, rakukaja))
def lightWarden = new Enemy("Light Warden", "enemy", "Elec", 220, 220, 55, 55, 35, 35, 18, type98Gunto, Buffer[Item](snuffSoul, healingI, healingII), Buffer[Ability](dia, diarama, powerSlash, garula, tarukaja))
def frostboundSentinel = new Enemy("Frostbound Sentinel", "enemy", "Ice", 275, 275, 70, 70, 45, 45, 23, edge, Buffer[Item](pulsatingStone, lifeStone, healingI), Buffer[Ability](diarama, blackSpot, agilao, garula, tarunda))
def shardgeist = new Enemy("Shardgeist", "enemy", "None", 250, 250, 60, 60, 28, 36, 20, kageDachi, Buffer[Item](ointment, recovR200, pulsatingStone, healingII), Buffer[Ability](bufula, blackSpot, diarama, fatalEnd, rakunda, tarunda))

//Forest
def chronoBeast = new Enemy("Chrono Beast", "enemy", "Fire", 300, 300, 70, 70, 40, 50, 27, midareHamon, Buffer[Item](lifeStone, moonDango), Buffer[Ability](bufudyne, diarama, braveBlade))
def timeboundWrath = new Enemy("Abyss Watcher", "enemy", "Elec", 370, 370, 75, 75, 65, 65, 31, kenka, Buffer[Item](moonDango, takemedic, sadayoTayaki), Buffer[Ability](agidyne, garudyne, blackSpot, giganticFist))
def echoingPhantom = new Enemy("Echoing Phantom", "enemy", "Physical" , 450, 450, 85, 85, 65, 80, 36, metalBat, Buffer[Item](snuffSoul, healingII, recovR200), Buffer[Ability](ziodyne, vileAssault, agidyne, diarahan))
def temporalLurker = new Enemy("Temporal Lurker", "enemy", "None", 540, 540, 110, 110, 80, 75, 40, kijintou, Buffer[Item](healingIII, sadayoTayaki, bead), Buffer[Ability](agidyne, garudyne, bufudyne, ziodyne, megidola, vileAssault))
def aegisOfAges = new Enemy("Aegis Of Ages", "enemy", "Ice", 600, 600, 100, 100, 90, 90, 45, kenka, Buffer[Item](sakuraAmezaiku, healingIII, bead), Buffer[Ability](agidyne, garudyne, braveBlade))