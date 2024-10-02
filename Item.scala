class Item(val name: String, val typee: String, val power: Int) {

  override def toString = name

  def description() =
    if power != 0 then
      if typee == "Recover HP" then
        println(s"$name: recover $power HP.")
      if typee == "Recover SP" then
        println(s"$name: recover $power HP.")
      if typee == "Special" then
        if name == "Healing I" then
          println(s"$name: Fully recover you HP. Can only be used outside of battle.")
    else
      println(s"${name}. This item might be helpful in the future...")

}

//Library
val medicine = new Item("Medicine", "Recover HP", 50)
val soulDrop = new Item("Soul Drop", "Recover SP", 25)
val snuffSoul = new Item("Snuff Soul", "Recover SP", 35)
val ointment = new Item("Ointment", "Recover HP", 100)
val healingI = new Item("Healing I", "Special", 0)

val archiveKeyItem = new Item("Fragment of Lost Knowledge", "Key Item", 0)
val sanctumKeyItem = new Item("Fragment of Forgotten Whispers", "Key Item", 0)
val chamberKeyItem = new Item("Fragment of Sealed Memories", "Key Item", 0)
val libraryKeyItem = new Item("Relic of Eternal Insight", "Library Key Item", 0)

val cavernUnlockedItem = new Item("Luminary Stone", "Cavern Unlock Item", 0)

//Cavern