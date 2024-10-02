import scala.collection.mutable
import scala.math.*

class Player(
              var name: String,
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
              var weaponList: mutable.Buffer[Weapon]) extends Character(name, maxHP, currentHP, currentSP, attackPower, defensePower, level, currentWeapon, abilityList):

  var choseToDefend = false
  val baseXP = 100

  def displayInventory(): Unit =
    println("\nWeapons:")
    weaponList.foreach(println)
    println("\nAbilities:")
    abilityList.foreach(println)
    println("\nItems:")
    itemList.foreach(println)

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

  def displayWeaponList() =
    for i <- weaponList.indices do
      println(s"${i + 1}: ${weaponList(i)}")

  def switchWeapon(weapon: Weapon) =
    currentWeapon = weapon
    println("You switched to " + currentWeapon)

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
    maxHP += (6 + level - 2)
    maxSP += (3 + level - 2)
    attackPower += (2 + level - 2)
    defensePower += (2 + level - 2)
    println(s"Congratulations! You've reached Level $level!")
    println(s"Your stats have improved: Max HP: $maxHP, Max SP: $maxSP, Attack: $attackPower, Defense: $defensePower")

  def selfRecovery() =
    restoreHP()

  def displayFullStat() =
    println(s"${name}: ")
    println(s"Basic stats: Level: $level, HP: $currentHP/$maxHP, SP: $currentSP/$maxSP, Attack: $attackPower, Defense: $defensePower")
    println(s"Current weapon: $currentWeapon")
    println("Inventory: ")
    displayInventory()
    println("Unlocked locations and areas: ")
    for location <- unlockedLocation do
      println(s"${location.name}: ")
      for area <- location.areas do
        println(s"${area.name} ")
      println("")

  def useItem(item: Item) =
    if item.typee == "Recover HP" then
      val newHP = min(maxHP, currentHP + item.power)
      val recoveredHP = newHP - currentHP
      currentHP = newHP
      println(s"You use ${item.name} to recover ${recoveredHP} HP. Your current HP: $currentHP")
      itemList -= item
    if item.typee == "Recover SP" then
      val newSP = min(maxSP, currentSP + item.power)
      val recoveredSP = newSP - currentSP
      currentSP = newSP
      println(s"You use ${item.name} and recover ${recoveredSP} SP. Your current SP: $currentSP")
      itemList -= item
    if item.typee == "Special" then
      if item.name == "Healing I" then
        currentHP = maxHP
        println(s"You use ${item.name} and fully recovers your HP!")
        itemList -= item

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

  def updateSP(amount: Int) = currentSP = amount