import scala.collection.mutable

class Boss(
  val bossName: String,
  val bossLocation: Location,
  val bossMaxHP: Int,
  val bossCurrentHP: Int,
  val bossCurrentSP: Int,
  val bossAttackPower: Int,
  val bossDefensePower: Int,
  val bossLevel: Int,
  val bossCurrentWeapon: Weapon,
  val bossItemList: mutable.Buffer[Item],
  val bossAbilityList: mutable.Buffer[Ability],
  var isDefeated: Boolean) extends Enemy(bossName, bossMaxHP, bossCurrentHP, bossCurrentSP, bossAttackPower, bossDefensePower, bossLevel, bossCurrentWeapon, bossItemList, bossAbilityList):

  def bossYapping() =
    if bossLocation == forgottenLibrary then
      println("The pages of history are written in blood and ink. Which will yours be?")
      println("Countless souls have wandered these halls seeking truth... none have survived the revelation.")
      println("You come seeking answers, yet you may not survive the question.")

  def bossDropWeapon = bossCurrentWeapon

  def bossDropItem = bossItemList.head

val libraryBoss = new Boss("Archivus the Eternal Scholar", forgottenLibrary, 150, 150, 100, 25, 25, 12, zweihander, mutable.Buffer[Item](cavernUnlockedItem), mutable.Buffer[Ability](agi, bufu, zio, dia, sonicPunch), false)







