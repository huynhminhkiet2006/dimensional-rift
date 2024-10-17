import scala.collection.mutable

class FinalBoss (
  val bossName: String,
  val bossType: String,
  val bossWeakness: String,
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

  def bossYappingPhase1() =
    println(s"${name}: So... this is where your journey ends, Traveler. You have come far, but you do not yet understand the depths of your own fear. " +
  "I am the embodiment of all you sought to escape—time, regret, and the very essence of your shattered past.")
    println(s"${name}: But now, it's time to face the consequences of your choices. " +
      "The elements will guide you to your end. I will show you that no force, no matter how powerful, can stand against the inevitability of your downfall.")
    println(s"${name}: Let the storm of your memories begin, as the world around you collapses into the chaos you’ve tried so desperately to outrun.")

  def bossYappingPhase2() =
    println(s"${name}: You have pushed me to this. Now, I shall unleash all of my power. No more hiding behind the elements—prepare to face your worst fears!")

  def bossDropWeapon = bossCurrentWeapon

  def bossDropItem = bossItemList.head

val finalBoss = new FinalBoss("The Soul of the Rift", "enemy", "None", 2000, 2000, 500, 500, 150, 150, 60, bladeOfTotsuka, mutable.Buffer[Item](artifact), mutable.Buffer[Ability](ragnarok, niflheim, thunderReign, pantaRhei, megidolaon, primalForce, heavenBlade, diarahan), false)
