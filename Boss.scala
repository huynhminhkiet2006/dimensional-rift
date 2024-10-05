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
    bossLocation match
      case `forgottenLibrary` =>
        println("The pages of history are written in blood and ink. Which will yours be?")
        println("Countless souls have wandered these halls seeking truth... none have survived the revelation.")
        println("You come seeking answers, yet you may not survive the question.")
      case `crystalCavern`    =>
        println("You dare to tread in my domain, mortal?")
        println("These crystals have witnessed eternity, and you... are nothing.")
        println("You seek the Chrono Prism, do you not? A futile pursuit. Time cannot be tamed.")
        println("Very well. I shall show you the futility of your quest.")
        println("Prepare yourself, for the light of eternity will be your undoing!")
      case _                  =>

  def bossDropWeapon = bossCurrentWeapon

  def bossDropItem = bossItemList.head

val libraryBoss = new Boss("Archivus the Eternal Scholar", forgottenLibrary, 150, 150, 100, 30, 25, 12, zweihander, mutable.Buffer[Item](cavernUnlockedItem), mutable.Buffer[Ability](agi, bufu, zio, garu, dia, sonicPunch, assaultDrive), false)

val cavernBoss = new Boss("The Prismatic Sovereign", crystalCavern, 350, 350, 200, 50, 60, 26, greatSword, mutable.Buffer[Item](forestUnlockedItem), mutable.Buffer[Ability](agilao, bufula, zionga, garula, diarama, powerSlash, fatalEnd, blackSpot), false)

val forestBoss = new Boss("Tempus, Guardian of the Shattered Epoch", timeTwistedForest, 700, 700, 350, 110, 110, 50, soulCrusher, mutable.Buffer[Item](), mutable.Buffer[Ability](megidola, godHand, diarahan), false)





