import java.lang

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

val agi = new Ability("Agi", "Deals light Fire damage to 1 foe", "Elemental", 3, 20)
val bufu = new Ability("Bufu", "Deals light Ice damage to 1 foe", "Elemental", 3, 20)
val zio = new Ability("Zio", "Deals light Elec damage to 1 foe", "Elemental", 4, 23)

val sonicPunch = new Ability("Sonic Punch", "Deals light Phys damage to 1 foe", "Physical", 11, 20)

val dia = new Ability("Dia", "Slightly restores your HP", "Healing", 3, -15)
