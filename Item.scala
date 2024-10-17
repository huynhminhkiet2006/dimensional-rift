class Item(val name: String, val typee: String, val power: Int) {

  override def toString = name

  def description() =
    if power != 0 then
      if typee == "Recover HP" then
        println(s"$name: recovers $power HP.")
      if typee == "Recover SP" then
        println(s"$name: recovers $power SP.")
    else
      if typee == "Special" then
        if name == "Healing I" then
          println(s"$name: Recovers 40% of your HP. Can only be used outside of battle.")
        if name == "Healing II" then
          println(s"$name: Recovers 60% of your HP. Can only be used outside of battle.")
        if name == "Healing III" then
          println(s"$name: Fully recovers your HP. Can only be used outside of battle.")
      else
        println(s"${name}. This item might be helpful in the future...")

}

//Library
val medicine = new Item("Medicine", "Recover HP", 50)
val soulDrop = new Item("Soul Drop", "Recover SP", 25)
val snuffSoul = new Item("Snuff Soul", "Recover SP", 35)
val ointment = new Item("Ointment", "Recover HP", 100)
val healingI = new Item("Healing I", "Special", 0)

val archiveKeyItem = new Item("Key of Lost Knowledge", "Key Item", 0)
val sanctumKeyItem = new Item("Key of Forgotten Whispers", "Key Item", 0)
val chamberKeyItem = new Item("Key of Sealed Memories", "Key Item", 0)
val libraryKeyItem = new Item("Relic of Eternal Insight", "Key Item", 0)

val cavernUnlockedItem = new Item("Luminary Stone", "Key Item", 0)


//Cavern
val muscleDrink = new Item("Muscle Drink", "Recover HP", 75)
val lifeStone = new Item("Life Stone", "Recover HP", 150)
val recovR200 = new Item("Recov-R: 200mg", "Recover HP", 200)
val chewingSoul = new Item("Chewing Soul", "Recover SP", 50)
val pulsatingStone = new Item("Pulsating Stone", "Recover SP", 70)
val healingII = new Item("Healing II", "Special", 0)


val abyssFragment = new Item("Fragment of the Luminous Abyss", "Key Item", 0)
val grottoFragment = new Item("Fragment of the Shattered Grotto", "Key Item", 0)
val hallFragment = new Item("Fragment of the Prism Hall", "Key Item", 0)
val veinFragment = new Item("Fragment of the Frozen Vein", "Key Item", 0)
val cavernKeyItem = new Item("Relic of Fractured Light", "Key Item", 0)

val forestUnlockedItem = new Item("Chrono Prism", "Key Item", 0)


//Forest
val moonDango = new Item("Moon Dango", "Recover HP", 120)
val takemedic = new Item("Takemedic", "Recover HP", 300)
val bead = new Item("Bead", "Recover HP", 500)
val sadayoTayaki = new Item("Sadayo Tayaki", "Recover SP", 75)
val sakuraAmezaiku = new Item("Sakura Amezaiku", "Recover SP", 100)
val healingIII = new Item("Healing III", "Special", 0)


val grooveEssence = new Item("Essence of Transience", "Key Item", 0)
val thicketEssence = new Item("Essence of Echoes", "Key Item", 0)
val springEssence = new Item("Essence of Timeless Flow", "Key Item", 0)
val gladeEssence = new Item("Essence of Ephemeral Seasons", "Key Item", 0)
val forestKeyItem = new Item("Relic of Temporal Balance", "Key Item", 0)

val nexusUnlockedItem = new Item("Temporal Shard", "Key Item", 0)

//Final Boss
val artifact = new Item("Artifact of Stability", "Key Item", 0)

