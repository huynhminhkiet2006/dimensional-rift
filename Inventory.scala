class Inventory:

  private var weapons = List[Weapon]()
  private var abilities = List[Ability]()
  private var items = List[Item]()

  def addWeapon(weapon: Weapon): Unit =
    weapons = weapons :+ weapon

  def addAbility(ability: Ability): Unit =
    abilities = abilities :+ ability

  def addItem(item: Item): Unit =
    items = items :+ item

  def getWeapons: List[Weapon] = weapons

  def getAbilities: List[Ability] = abilities

  def getItems: List[Item] = items
