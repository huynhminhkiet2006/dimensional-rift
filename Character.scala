import scala.collection.mutable
import scala.util.Random

class Character(
  val _name: String,
  var _maxHP: Int,
  var _HP: Int,
  var _SP: Int,
  var _attackPower: Int,
  var _defensePower: Int,
  var _level: Int,
  var _currentWeapon: Weapon,
  var _abilityList: mutable.Buffer[Ability]):

  // Getter and Setter for HP
  def HP: Int = _HP
  def SP: Int = _SP
  def HP_= (newHP: Int) = _HP = math.max(0, newHP)
  def SP_= (newSP: Int) = _SP = math.max(0, newSP)

  private var isDefending = false

  def checkDefending = isDefending
  def defending() = isDefending = true
  def resetDefending() = isDefending = false

  def attack(target: Character) =
    println(s"${this._name} attacks!")
    if target.checkDefending then
      takeReducedDamage(target, this._currentWeapon.atkPower * 1/2 + this._attackPower * 1/2)
      target.resetDefending()
    else
      takeDamage(target, this._currentWeapon.atkPower * 1/2 + this._attackPower * 1/2)

  def defend() =
    defending()
    println(s"${this._name} braces for impact and will take reduced damage next turn!")

  def takeReducedDamage(target: Character, amount: Int) =
    val reducedDamage = amount - (amount * (50 + this._defensePower / 2) / 100)
    takeDamage(target, reducedDamage)

  def takeDamage(target: Character, amount: Int) =
    val variance = (Random.nextDouble() * 0.2) - 0.1
    val adjustedAmount = (amount + (amount * variance)).toInt

    target.HP -= adjustedAmount
    if (target.HP < 0) then target.HP = 0
    println(s"${target._name} takes $adjustedAmount damage!")

  def heal(amount: Int) =
    println(s"${this._name} heals ${-amount} HP!")
    HP += amount
    if (HP > _maxHP) then HP = _maxHP

  def restoreHP() = HP = _maxHP

  def useAbility(ability: Ability, target: Character): Boolean =
    if (ability.abilityType == "Physical") then
      if (_maxHP * ability.abilityCost / 100 >= HP) then
        false
      else
        ability.use(target)
        HP -= _maxHP * ability.abilityCost / 100
        true
    else if (ability.abilityType == "Elemental") then
      if (ability.abilityCost > SP) then
        false
      else
        ability.use(target)
        SP -= ability.abilityCost
        true
    else
      if (ability.abilityCost > SP) then
        false
      else
        ability.use(target)
        SP -= ability.abilityCost
        true
