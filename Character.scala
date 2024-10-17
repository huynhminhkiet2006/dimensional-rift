import scala.collection.mutable
import scala.util.Random

class Character(
  val _name: String,
  val _typee: String,
  val _weakness: String,
  var _maxHP: Int,
  var _HP: Int,
  var _maxSP: Int,
  var _SP: Int,
  var _attackPower: Int,
  var _defensePower: Int,
  var _level: Int,
  var _currentWeapon: Weapon,
  var _abilityList: mutable.Buffer[Ability]):

  // Getter and Setter
  def HP: Int = _HP
  def maxHP2: Int = _maxHP
  def maxSP2: Int = _maxSP
  def SP: Int = _SP
  def atkPow: Int = _attackPower
  def defPow: Int = _defensePower
  def weapon: Weapon = _currentWeapon

  def HP_= (newHP: Int) = _HP = math.max(0, newHP)
  def SP_= (newSP: Int) = _SP = math.max(0, newSP)
  def maxHP2_= (newmaxHP2: Int) = _maxHP = newmaxHP2
  def maxSP2_= (newmaxSP2: Int) = _maxSP = newmaxSP2
  def atkPow_= (newatkPow: Int) = _attackPower = newatkPow
  def defPow_= (newdefPow: Int) = _defensePower = newdefPow
  def weapon_= (newweapon: Weapon) = _currentWeapon = newweapon

  private var isDefending = false

  def checkDefending = isDefending
  def defending() = isDefending = true
  def resetDefending() = isDefending = false

  val atkHolder = this.atkPow
  val defHolder = this.defPow

  var checkAtkBuff = false
  var checkDefBuff = false
  var checkAtkDebuff = false
  var checkDefDebuff = false

  def debuffAtk() =
    println(s"${this._name} attack is reduced for the next three turns!")
    checkAtkDebuff = true

  def boostAtk() =
    println(s"${this._name} increase attack for the next three turns!")
    checkAtkBuff = true

  def resetAtkBuff() =
    println(s"${this._name}'s attack buff reverted.")
    checkAtkBuff = false

  def resetAtkDebuff() =
    println(s"${this._name}'s attack debuff reverted.")
    checkAtkDebuff = false

  def debuffDef() =
    println(s"${this._name} attack is reduced for the next three turns!")
    checkDefDebuff = true

  def boostDef() =
    println(s"${this._name} increase defense for the next three turns!")
    checkDefBuff = true

  def resetDefBuff() =
    println(s"${this._name}'s defense buff reverted.")
    checkDefBuff = false

  def resetDefDebuff() =
    println(s"${this._name}'s defense buff reverted.")
    checkDefDebuff = false

  def attack(target: Character) =
    println(s"${this._name} attacks!")
    if Random.nextDouble() < this._currentWeapon.accuracy.toDouble / 100.toDouble then

      var damageTaken = this._currentWeapon.atkPower * 1/2 + this.atkPow * 1/2
      val originalDamage = damageTaken

      if target.checkDefDebuff then damageTaken += originalDamage * 20 / 100
      if this.checkAtkBuff then damageTaken += originalDamage * 20 / 100
      if target.checkDefBuff then damageTaken -= originalDamage * 20 / 100

      if target.checkDefending then
        target.takeReducedDamage(damageTaken)
        target.resetDefending()
      else
        if target._weakness == "Physical" then
          println(s"${this._name} hit a weakness!")
          target.takeDamage(damageTaken + originalDamage * 20 / 100)
        else
          target.takeDamage(damageTaken)
    else
      println(s"${target._name} dodges the attack!")

  def defend() =
    defending()
    println(s"${this._name} braces for impact and will take reduced damage next turn!")

  def takeReducedDamage(amount: Int) =
    val reducedDamage = amount - (amount * math.min(80, (50 + this.defPow / 2)) / 100)
    takeDamage(reducedDamage)

  def takeDamage(amount: Int) =
    val variance = (Random.nextDouble() * 0.2) - 0.1
    val adjustedAmount = (amount + (amount * variance)).toInt

    HP -= adjustedAmount
    if (HP < 0) then HP = 0
    println(s"${_name} takes $adjustedAmount damage!")

  def heal(amount: Int) =
    println(s"${this._name} heals ${amount} HP!. Current HP: $HP HP")
    HP += amount
    if (HP > maxHP2) then HP = maxHP2

  def useAbility(ability: Ability, target: Character) =
    if (ability.abilityType == "Physical") then
      if (maxHP2 * ability.abilityCost / 100 >= HP) then
        false
      else
        ability.use(this, target)
        HP -= maxHP2 * ability.abilityCost / 100
        true
    else
      if (ability.abilityCost > SP) then
        false
      else
        ability.use(this, target)
        SP -= ability.abilityCost
        true