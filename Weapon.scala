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
  25,
  95
)


//Cavern weapon drop
val titaniumClub = new Weapon(
  "Titanium Club",
  23,
  96
)

val gothicSword = new Weapon(
  "Gothic Sword",
  27,
  94
)

val type98Gunto = new Weapon(
  "Type-98 Gunto",
  32,
  98
)

val edge = new Weapon(
  "Edge",
  38,
  92
)

val kageDachi = new Weapon(
  "Kage-Dachi",
  45,
  92
)

val greatSword = new Weapon(
  "Great Sword",
  55,
  84
)


//Forest weapon drop
val midareHamon = new Weapon(
  "Midare Hamon",
  50,
  95
)

val kenka = new Weapon(
  "Kenka",
  60,
  90
)

val metalBat = new Weapon(
  "Metal Bat",
  80,
  80
)

val kijintou = new Weapon(
  "Kijintou",
  70,
  98
)

val yashiroSword = new Weapon(
  "Yashiro Sword",
  90,
  96
)

val soulCrusher = new Weapon(
  "Soul Crusher",
  110,
  88
)




