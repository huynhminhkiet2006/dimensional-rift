import scala.collection.mutable
import scala.io.StdIn.*

class Boss(
  val bossName: String,
  val bossType: String,
  val bossWeakness: String,
  val bossLocation: Location,
  val bossMaxHP: Int,
  val bossCurrentHP: Int,
  val bossMaxSP: Int,
  val bossCurrentSP: Int,
  val bossAttackPower: Int,
  val bossDefensePower: Int,
  val bossLevel: Int,
  val bossCurrentWeapon: Weapon,
  val bossItemList: mutable.Buffer[Item],
  val bossAbilityList: mutable.Buffer[Ability],
  var isDefeated: Boolean) extends Enemy(bossName, bossType, bossWeakness, bossMaxHP, bossCurrentHP, bossMaxSP, bossCurrentSP, bossAttackPower, bossDefensePower, bossLevel, bossCurrentWeapon, bossItemList, bossAbilityList):

  val action = new GameAction

  def bossYapping() =
    bossLocation match
      case `forgottenLibrary`  =>
        println(s"${name}: The pages of history are written in blood and ink. Which will yours be?")
        println(s"${name}: Countless souls have wandered these halls seeking truth... none have survived the revelation.")
        action.pressEnter()
        println(s"${name}: You come seeking answers, yet you may not survive the question.")
        println(s"\nPress Enter to start battle!")
        readLine()
        MusicManager.loopMusic("1stBossTheme")
      case `crystalCavern`     =>
        println(s"${name}: You dare to tread in my domain, mortal?")
        println(s"${name}: These crystals have witnessed eternity, and you... are nothing.")
        println(s"${name}: You seek the Chrono Prism, do you not? A futile pursuit. Time cannot be tamed.")
        action.pressEnter()
        println(s"${name}: Very well. I shall show you the futility of your quest.")
        println(s"${name}: Prepare yourself, for the light of eternity will be your undoing!")
        println(s"\nPress Enter to start battle!")
        readLine()
        MusicManager.loopMusic("2ndBossTheme")
      case `timeTwistedForest` =>
        println(s"${name}: You trespass in the realm where time fractures and bends.")
        println(s"${name}: The echoes of your fate are but whispers in the wind, lost to eternity.")
        action.pressEnter()
        println(s"${name}: Do you seek to rewrite your story? Know this: every moment you grasp slips further away.")
        println(s"${name}: Prepare to face the relentless tide of time itself!")
        println(s"\nPress Enter to start battle!")
        readLine()
        MusicManager.loopMusic("3rdBossTheme")

  def bossDropWeapon = bossCurrentWeapon

  def bossDropItem = bossItemList.head

val libraryBoss = new Boss("Archivus the Eternal Scholar", "enemy", "None", forgottenLibrary, 300, 300, 150, 150, 40, 35, 12, zweihander, mutable.Buffer[Item](cavernUnlockedItem), mutable.Buffer[Ability](agi, bufu, zio, garu, dia, sonicPunch, assaultDrive), false)

val cavernBoss = new Boss("The Prismatic Sovereign", "enemy", "None", crystalCavern, 600, 600, 200, 200, 70, 60, 26, greatSword, mutable.Buffer[Item](forestUnlockedItem), mutable.Buffer[Ability](agilao, bufula, zionga, garula, diarama, powerSlash, fatalEnd, blackSpot), false)

val forestBoss = new Boss("Tempus, Guardian of the Shattered Epoch", "enemy", "None", timeTwistedForest, 1200, 1200, 350, 350, 110, 110, 50, soulCrusher, mutable.Buffer[Item](nexusUnlockedItem), mutable.Buffer[Ability](megidola, godHand, diarahan), false)






