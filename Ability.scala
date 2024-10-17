class Ability(
  val name: String,
  val description: String,
  val abilityType: String,
  val abilityCost: Int,
  val power: Int
):
  def use(caster: Character, target: Character) =
    if abilityType == "Healing" then
      println(s"$name used on ${caster._name}!")
      caster.heal(power)
    else if abilityType == "Buff" then
      name match
        case "Tarukaja" =>
          caster.boostAtk()
          println(s"$name used on ${caster._name}!")
        case "Rakukaja" =>
          caster.boostDef()
          println(s"$name used on ${caster._name}!")
    else if abilityType == "Debuff" then
      name match
        case "Tarunda" =>
          target.debuffAtk()
          println(s"$name used on ${target._name}!")
        case "Rakunda" =>
          target.debuffDef()
          println(s"$name used on ${target._name}!")
    else
      println(s"$name used on ${target._name}!")

      var damageTaken = power / 2 + caster._attackPower / 2
      val originalDamage = damageTaken

      if caster.checkAtkBuff then damageTaken += originalDamage * 20 / 100
      if target.checkDefDebuff then damageTaken += originalDamage * 20 / 100
      if target.checkDefBuff then damageTaken -= originalDamage * 20 / 100

      if (target.checkDefending) then
        target.takeReducedDamage(damageTaken)
        target.resetDefending()
      else
        if target._weakness == abilityType then
          println(s"${caster._name} hit a weakness!")
          target.takeDamage(damageTaken + originalDamage * 20 / 100)
        else
          target.takeDamage(damageTaken)

  override def toString = name


//Elemental

//Library
val agi = new Ability("Agi", "Deals light Fire damage to 1 foe. Cost: 3 SP", "Fire", 3, 20)
val bufu = new Ability("Bufu", "Deals light Ice damage to 1 foe. Cost: 4 SP", "Ice", 4, 23)
val zio = new Ability("Zio", "Deals light Elec damage to 1 foe. Cost: 4 SP", "Elec", 4, 23)
val garu = new Ability("Garu", "Deals light Wind damage to 1 foe. Cost: 3 SP", "Wind", 3, 20)

//Cavern
val agilao = new Ability("Agilao", "Deals medium Fire damage to 1 foe. Cost: 8 SP", "Fire", 8, 50)
val bufula = new Ability("Bufula", "Deals medium Ice damage to 1 foe. Cost: 10 SP", "Ice", 10, 55)
val zionga = new Ability("Zionga", "Deals medium Elec damage to 1 foe. Cost: 10 SP", "Elec", 10, 55)
val garula = new Ability("Garula", "Deals medium Wind damage to 1 foe. Cost: 8 SP", "Wind", 8, 53)


//Forest
val agidyne = new Ability("Agidyne", "Deals heavy Fire damage to 1 foe. Cost: 13 SP", "Fire", 13, 100)
val bufudyne = new Ability("Bufudyne", "Deals heavy Ice damage to 1 foe. Cost: 15 SP", "Ice", 15, 107)
val ziodyne = new Ability("Ziodyne", "Deals heavy Elec damage to 1 foe. Cost: 15 SP", "Elec", 15, 108)
val garudyne = new Ability("Agilao", "Deals heavy Wind damage to 1 foe. Cost: 13 SP", "Wind", 13, 100)
val megidola = new Ability("Megidola", "Deals heavy Almighty damage to 1 foe. Cost: 20 SP", "Almighty", 20, 115)

//Final Boss
val ragnarok = new Ability("Ragnarok", "Deals severe Fire damage to 1 foe. Cost: 20 SP", "Fire", 20, 150)
val niflheim = new Ability("Niflheim", "Deals severe Ice damage to 1 foe. Cost: 25 SP", "Ice", 25, 170)
val thunderReign = new Ability("Thunder Reign", "Deals severe Elec damage to 1 foe. Cost: 25 SP", "Elec", 25, 170)
val pantaRhei = new Ability("Panta Rhei", "Deals severe Wind damage to 1 foe. Cost: 20 SP", "Wind", 20, 150)
val megidolaon = new Ability("Megidolaon", "Deals severe Almighty damage to 1 foe. Cost: 32 SP", "Almighty", 32, 190)


//Physical

//Library
val sonicPunch = new Ability("Sonic Punch", "Deals light Phys damage to 1 foe. Cost: 8% max HP", "Physical", 8, 25)
val assaultDrive = new Ability("Assault Drive", "Deals light Phys damage to 1 foe. Cost: 10% max HP", "Physical", 10, 27)

//Cavern
val powerSlash = new Ability("Power Slash", "Deals medium Phys damage to 1 foe. Cost: 14% max HP", "Physical", 14, 55)
val fatalEnd = new Ability("Fatal End", "Deals medium Phys damage to 1 foe. Cost: 15% max HP", "Physical", 15, 60)
val blackSpot = new Ability("Black Spot", "Deals medium Phys damage to 1 foe. Cost: 16% max HP", "Physical", 16, 65)

//Forest
val giganticFist = new Ability("Gigantic Fist", "Deals heavy Phys damage to 1 foe. Cost: 18% max HP", "Physical", 18, 110)
val vileAssault = new Ability("Vile Assault", "Deals heavy Phys damage to 1 foe. Cost: 20% max HP", "Physical", 20, 115)
val braveBlade = new Ability("Brave Blade", "Deals heavy Phys damage to 1 foe. Cost: 20% max HP", "Physical", 20, 120)
val godHand = new Ability("God's Hand", "Deals extra heavy Phys damage to 1 foe. Cost: 20% max HP", "Physical", 22, 125)

//Nexus
val primalForce = new Ability("Primal Force", "Deals severe Phys damage to 1 foe. Cost: 24% max HP", "Physical", 24, 150)
val heavenBlade = new Ability("Heaven's Blade", "Deals severe Phys damage to 1 foe. Cost: 25% max HP", "Physical", 25, 175)


//Healing

//Library
val dia = new Ability("Dia", "Slightly restores your HP. Cost: 8 SP", "Healing", 8, 80)

//Cavern
val diarama = new Ability("Diarama", "Moderately restores your HP. Cost: 15 SP", "Healing", 15, 150)

//Forest
val diarahan = new Ability("Diarahan", "Strongly restores your HP. Cost: 25 SP", "Healing", 25, 320)

//Nexus
val recarm = new Ability("Diarahan", "Fully restores your HP. Cost: 40 SP", "Healing", 40, 10000000)


//Support

//Cavern
val tarukaja = new Ability("Tarukaja", "Increases your Attack for 3 turns. Cost: 12 SP", "Buff", 12, 0)
val rakukaja = new Ability("Rakukaja", "Increases your Defense for 3 turns. Cost: 12 SP", "Buff", 12, 0)
val rakunda = new Ability("Rakunda", "Decreases 1 foe's Defense for 3 turns. Cost: 10 SP", "Debuff", 10, 0)
val tarunda = new Ability("Tarunda", "Decreases 1 foe's Attack for 3 turns. Cost: 10 SP", "Debuff", 10, 0)