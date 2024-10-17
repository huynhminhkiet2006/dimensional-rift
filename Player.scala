import scala.collection.mutable
import scala.math.*

class Player(
              var name: String,
              val typee: String,
              var weakness: String,
              var hasEncounteredEnemy: Boolean,
              var maxHP: Int,
              var currentHP: Int,
              var maxSP: Int,
              var currentSP: Int,
              var attackPower: Int,
              var defensePower: Int,
              var xp: Int,
              var level: Int,
              var abilityList: mutable.Buffer[Ability],
              var itemList: mutable.Buffer[Item],
              var money: Int,
              var unlockedLocation: mutable.Buffer[Location],
              var currentLocation: Location,
              var currentArea: Area,
              var currentWeapon: Weapon,
              var weaponList: mutable.Buffer[Weapon]) extends Character(name, typee, weakness, maxHP, currentHP, maxSP, currentSP, attackPower, defensePower, level, currentWeapon, abilityList):

  var choseToDefend = false
  val baseXP = 50

  def displayInventory(): Unit =
    println("\nWeapons:")
    weaponList.foreach(weapon => println(s"- $weapon"))
    println("\nAbilities:")
    abilityList.foreach(ability => println(s"- $ability"))
    if getItem.isEmpty then
      println("\nItems: None")
    else
      println("\nItems: ")
      getItem.zipWithIndex.foreach { case ((item, count), index) => println(s"${index + 1}. ${item}: ${item.typee} x$count") }

  def displayCurrentLocation() =
    println(currentLocation)

  def displayAvailableLocation() =
    for i <- unlockedLocation.indices do
      println(s"${i + 1}. ${unlockedLocation(i)}")

  def displayAvailableArea(location: Location) =
    for i <- location.areas.indices do
      println(s"${i + 1}. ${location.areas(i)}")

  def addLocation(location: Location) =
    unlockedLocation += location

  def addArea(location: Location, area: Area) =
    location.areas += area

  def travelTo(location: Location, area: Area) =
    currentLocation = location
    currentArea = area

  def go(direction: String) =
    val destination = this.currentArea.neighbor(direction)
    this.currentArea = destination.getOrElse(this.currentArea)
    if destination.isDefined then "You go " + direction + "." else "You can't go " + direction + "."

  def acquireAbility(ability: Ability) =
    abilityList += ability

  def acquireWeapon(weapon: Weapon) =
    weaponList += weapon

  def acquireItem(item: Item) =
    itemList += item

  def removeItem(item: Item) =
    itemList -= item

  def displayWeaponList() =
    val sortedWeaponList = weaponList.sortBy(_.atkPower)
    for i <- sortedWeaponList.indices do
      println(s"${i + 1}: ${weaponList(i)}")

  def displayAbilityList() =
    val sortedAbilityList = abilityList.sortBy(_.abilityType)
    for i <- abilityList.indices do
      println(s"${i + 1}: ${abilityList(i)}")

  def switchWeapon(weapon: Weapon) =
    currentWeapon = weapon
    println(s"You switched to $currentWeapon.")

  def xpToNextLevel: Int = baseXP * level * level

  def gainXP(amount: Int) =
    xp += amount
    println(s"You gained $amount XP! Current XP: $xp")
    checkLevelUp()

  def checkLevelUp() =
    while (xp >= xpToNextLevel) do
      levelUp()

  def levelUp() =
    level += 1
    maxHP += (7 + level - 2)
    maxSP += (5 + level - 2)
    attackPower += (2 + level - 2)
    defensePower += (level - 2)
    println(s"Congratulations! You've reached Level $level!")
    println(s"Your stats have improved: Max HP: $maxHP, Max SP: $maxSP, Attack: $attackPower, Defense: $defensePower\n")

  def selfRecovery() =
    currentHP = maxHP
    currentSP = maxSP

  def displayFullStat() =
    println(s"${name}: ")
    println(s"Basic stats: Level: $level, HP: $currentHP/$maxHP, SP: $currentSP/$maxSP, Attack: $attackPower, Defense: $defensePower")
    println(s"Current weapon: $currentWeapon\n")
    println(s"Current location: $currentLocation.")
    println(s"Current area: $currentArea.\n")
    println("Unlocked locations and areas: ")
    for location <- unlockedLocation do
      println(s"${location.name}: ")
      for area <- location.areas do
        println(s"- ${area.name} ")
      println("")
    println("Inventory: ")
    displayInventory()

  def useItem(item: Item) =
    if item.typee == "Recover HP" then
      val newHP = min(maxHP, currentHP + item.power)
      val recoveredHP = newHP - currentHP
      currentHP = newHP
      println(s"You use ${item.name} to recover ${recoveredHP} HP. Your current HP: $currentHP")
      removeItem(item)
      this.HP = currentHP
    if item.typee == "Recover SP" then
      val newSP = min(maxSP, currentSP + item.power)
      val recoveredSP = newSP - currentSP
      currentSP = newSP
      println(s"You use ${item.name} and recover ${recoveredSP} SP. Your current SP: $currentSP")
      removeItem(item)
      this.SP = currentSP
    if item.typee == "Special" then
      if item.name == "Healing I" then
        val newHP = min(maxHP, currentHP + (maxHP * 40 / 100))
        val recoveredHP = newHP - currentHP
        currentHP = newHP
        println(s"You use ${item.name} to recover ${recoveredHP} HP. Your current HP: $currentHP")
        removeItem(item)
        this.HP = currentHP
      if item.name == "Healing II" then
        val newHP = min(maxHP, currentHP + (maxHP * 60 / 100))
        val recoveredHP = newHP - currentHP
        currentHP = newHP
        println(s"You use ${item.name} to recover ${recoveredHP} HP. Your current HP: $currentHP")
        removeItem(item)
        this.HP = currentHP
      if item.name == "Healing III" then
        currentHP = maxHP
        println(s"You use ${item.name} and fully recovers your HP. Your current HP: $currentHP")
        removeItem(item)
        this.HP = currentHP


  def sortedItemList = itemList.sortBy(_.typee)

  def getItem =

    var itemMap = Map[Item, Int]()
    for item <- sortedItemList do
      itemMap = itemMap.updatedWith(item):
        case Some(count) => Some(count + 1)
        case None => Some(1)

    
    itemMap.toSeq.sortBy { case (item, _) => item.typee }.toMap

  def getUsableItem =
    val usableItemList = mutable.Buffer[Item]()
    for item <- itemList do
      if item.typee != "Key Item" then
        usableItemList += item

    var usableItemMap = Map[Item, Int]()

    for item <- usableItemList do
      usableItemMap = usableItemMap.updatedWith(item):
        case Some(count) => Some(count + 1)
        case None => Some(1)

    usableItemMap

  def getItemUsableInBattle =
    val usableItemList = mutable.Buffer[Item]()
    for item <- itemList do
      if item.typee == "Recover HP" || item.typee == "Recover SP" then
        usableItemList += item

    var usableItemMap = Map[Item, Int]()

    for item <- usableItemList do
      usableItemMap = usableItemMap.updatedWith(item):
        case Some(count) => Some(count + 1)
        case None => Some(1)

    usableItemMap

  def getItemByNumber(itemMap: Map[Item, Int], number: Int): Option[Item] =
    val itemList = itemMap.zipWithIndex.map { case ((item, count), index) => (index + 1, item, count) }
    itemList.find { case (num, _, _) => num == number }.map { case (_, item, _) => item }

  def updateHP(amount: Int) = currentHP = amount
  def updateMaxHP(amount: Int) = maxHP = amount
  def updateSP(amount: Int) = currentSP = amount
  def updateMaxSP(amount: Int) = maxSP = amount
  def updateAttack(amount: Int) = attackPower = amount
  def updateDefense(amount: Int) = defensePower = amount
  def updateWeapon(weapon: Weapon) = currentWeapon = weapon
  def updateWeakness(value: String) = weakness = currentWeapon.weakness