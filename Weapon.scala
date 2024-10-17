class Weapon(val name: String, val atkPower: Int, val accuracy: Int, val weakness: String):

  override def toString: String = name
  def displayStat() = println(s"${name}: Attack Power: ${atkPower}, Accuracy: ${accuracy}, Weakness: $weakness")

val Fist = new Weapon(
  "Bare Fists",
  10,
  100,
  "None"
)

//Library Weapon drop
val golfClub = new Weapon(
  "Golf Club",
  14,
  93,
  "Ice"
)

val longSword = new Weapon(
  "Long Sword",
  28,
  92,
  "Fire"
)

val bastardSword = new Weapon(
  "Bastard Sword",
  25,
  92,
  "Elec"
)

val zweihander = new Weapon(
  "Zweihander",
  35,
  95,
  "Wind"
)


//Cavern weapon drop
val titaniumClub = new Weapon(
  "Titanium Club",
  20,
  96,
  "Ice"
)

val gothicSword = new Weapon(
  "Gothic Sword",
  28,
  94,
  "Wind"
)

val type98Gunto = new Weapon(
  "Type-98 Gunto",
  36,
  98,
  "Elec"
)

val edge = new Weapon(
  "Edge",
  41,
  92,
  "Fire"
)

val kageDachi = new Weapon(
  "Kage-Dachi",
  50,
  92,
  "None"
)

val greatSword = new Weapon(
  "Great Sword",
  60,
  84,
  "None"
)


//Forest weapon drop
val midareHamon = new Weapon(
  "Midare Hamon",
  55,
  95,
  "Fire"
)

val kenka = new Weapon(
  "Kenka",
  65,
  90,
  "Ice"
)

val metalBat = new Weapon(
  "Metal Bat",
  72,
  80,
  "Wind",
)

val kijintou = new Weapon(
  "Kijintou",
  80,
  98,
  "Elec"
)

val yashiroSword = new Weapon(
  "Yashiro Sword",
  92,
  96,
  "None"
)

val soulCrusher = new Weapon(
  "Soul Crusher",
  120,
  88,
  "None"
)

//Final Boss
val bladeOfTotsuka = new Weapon(
  "Blade Of Totsuka",
  155,
  96,
  "None"
)


