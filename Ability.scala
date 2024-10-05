class Ability(
  val name: String,
  val description: String,
  val abilityType: String,
  val abilityCost: Int,
  val power: Int
):
  def use(target: Character) =
    println(s"$name used on ${target._name}!")
    if abilityType == "Healing" then
      target.heal(power)
    else if (target.checkDefending) then
      target.takeReducedDamage(target, power)
      target.resetDefending()
    else
      target.takeDamage(target, power)

  override def toString = name


//Elemental

//Library
val agi = new Ability("Agi", "Deals light Fire damage to 1 foe", "Elemental", 3, 20)
val bufu = new Ability("Bufu", "Deals light Ice damage to 1 foe", "Elemental", 4, 23)
val zio = new Ability("Zio", "Deals light Elec damage to 1 foe", "Elemental", 4, 23)
val garu = new Ability("Garu", "Deals light Wind damage to 1 foe", "Elemental", 3, 20)

//Cavern
val agilao = new Ability("Agilao", "Deals medium Fire damage to 1 foe", "Elemental", 8, 50)
val bufula = new Ability("Bufula", "Deals medium Ice damage to 1 foe", "Elemental", 10, 55)
val zionga = new Ability("Zionga", "Deals medium Elec damage to 1 foe", "Elemental", 10, 55)
val garula = new Ability("Garula", "Deals medium Wind damage to 1 foe", "Elemental", 8, 53)

//Forest
val agidyne = new Ability("Agidyne", "Deals heavy Fire damage to 1 foe", "Elemental", 13, 80)
val bufudyne = new Ability("Bufudyne", "Deals heavy Ice damage to 1 foe", "Elemental", 15, 87)
val ziodyne = new Ability("Ziodyne", "Deals heavy Elec damage to 1 foe", "Elemental", 15, 88)
val garudyne = new Ability("Agilao", "Deals heavy Wind damage to 1 foe", "Elemental", 13, 80)
val megidola = new Ability("Megidola", "Deals heavy Almighty damage to 1 foe", "Elemental", 20, 100)

//Physical

//Library
val sonicPunch = new Ability("Sonic Punch", "Deals light Phys damage to 1 foe", "Physical", 8, 20)
val assaultDrive = new Ability("Assault Drive", "Deals light Phys damage to 1 foe", "Physical", 10, 26)

//Cavern
val powerSlash = new Ability("Power Slash", "Deals medium Phys damage to 1 foe", "Physical", 14, 50)
val fatalEnd = new Ability("Fatal End", "Deals medium Phys damage to 1 foe", "Physical", 15, 53)
val blackSpot = new Ability("Black Spot", "Deals medium Phys damage to 1 foe", "Physical", 16, 55)

//Forest
val giganticFist = new Ability("Gigantic Fist", "Deals heavy Phys damage to 1 foe", "Physical", 18, 90)
val vileAssault = new Ability("Vile Assault", "Deals heavy Phys damage to 1 foe", "Physical", 20, 95)
val braveBlade = new Ability("Brave Blade", "Deals heavy Phys damage to 1 foe", "Physical", 20, 100)
val godHand = new Ability("God's Hand", "Deals extra heavy Phys damage to 1 foe", "Physical", 22, 120)

//Healing

//Library
val dia = new Ability("Dia", "Slightly restores your HP", "Healing", 3, -80)

//Cavern
val diarama = new Ability("Diarama", "Moderately restores your HP", "Healing", 7, -150)

//Forest
val diarahan = new Ability("Diarahan", "Strongly restores your HP", "Healing", 7, -320)

