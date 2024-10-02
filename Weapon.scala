class Weapon(val name: String, val atkPower: Int, val accuracy: Int):
  override def toString: String = this.name
  def displayStat() = println(s"${name}: Attack Power: ${atkPower}, Accuracy: ${accuracy}")

val Fist = new Weapon(
  "Bare Fists",
  10,
  100
)

//Library Weapon drop
val golfClub = new Weapon(
  "Golf Club",
  12,
  93
)

val longSword = new Weapon(
  "Long Sword",
  15,
  92
)

val bastardSword = new Weapon(
  "Bastard Sword",
  18,
  92
)

val zweihander = new Weapon(
  "Zweihander",
  22,
  95
)



