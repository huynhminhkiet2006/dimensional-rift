import scala.collection.mutable
import scala.collection.mutable.Map

class Area(val name: String, val description: String, val keyArea: Boolean, val containsPuzzle: Option[Puzzle]):

  private val neighbors = Map[String, Area]()
  private val itemStorage = Map[Item, Area]()
  private val nameList = Map[String, Item]()

  def exitsAvailable = neighbors.keys.toVector

  def displayExitsAvailable() =
    println("Exits available: ")
    for i <- exitsAvailable.indices do
      println(s"${i + 1}: ${exitsAvailable(i)}")

  def addItem(item: Item) =
    itemStorage += (item -> this)
    nameList += (item.name -> item)

  def contains(itemName: String): Boolean = itemStorage.keys.exists(_.name == itemName)

  def neighbor(direction: String) = this.neighbors.get(direction)

  def setNeighbor(direction: String, neighbor: Area) =
    this.neighbors += direction -> neighbor

  def setNeighbors(exits: Vector[(String, Area)]) =
    this.neighbors ++= exits

  def fullDescription: String =
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    var itemList = ""
    for items <- itemStorage do
      itemList += s"${items._1} "
    if itemList.nonEmpty then
      val YouSeeHere = "\nYou see here: " + itemList
      this.description + YouSeeHere + exitList
    else
      this.description + exitList

  override def toString = name

//Nexus
val entrance = new Area("Entrance", "The entrance to the unknown.", false, None)

//Library
val hallway = new Area(
  "Dimly Lit Hallway",
  "A long, narrow corridor shrouded in shadows. The air feels heavy with whispers of the past.",
  false,
  None
)

val archive = new Area(
  "Forgotten Archives",
  "A room filled with dusty shelves and cobweb-covered tomes. A strange energy pulses in the air.",
  false,
  Some(archivePuzzle)
)

val chamber = new Area(
  "The Curator’s Chamber",
  "An oddly warm space, cluttered with peculiar artifacts and personal belongings. Memories linger here.",
  false,
  Some(chamberPuzzle)
)

val sanctum = new Area(
  "Sanctum of Echoes",
  "A cavernous room where every sound reverberates unsettlingly. The walls seem to listen.",
  false,
  Some(sanctumPuzzle)
)

val throne = new Area(
  "Throne of the Forgotten",
  "A vast, dimly lit room dominated by an imposing throne draped in tattered banners. \nA heavy tension fills the air, awaiting the fateful encounter.",
  true,
  None
)

//Cavern
val chasm = new Area(
  "Echoing Chasm",
  "A vast chamber where every sound echoes endlessly. The walls shimmer with crystals, distorting both light and sound.",
  false,
  None
)

val abyss = new Area(
  "Luminous Abyss",
  "A deep pit lit by faintly glowing crystals. The darkness feels alive.",
  false,
  Some(abyssPuzzle)
)

val grotto = new Area(
  "Shattered Grotto",
  "Fragments of broken crystals litter the ground, a place of forgotten despair.",
  false,
  Some(grottoPuzzle)
)

val hall = new Area(
  "Prism Hall",
  "Crystal pillars scatter light into endless, shifting hues, obscuring your way.",
  false,
  Some(hallPuzzle)
)

val vein = new Area(
  "Frozen Vein",
  "A freezing passage filled with cold, blue light. The ice cracks with each step.",
  false,
  Some(veinPuzzle)
)

val heart = new Area(
  "The Heart of Glass",
  "A room of sharp crystals reflecting light, hiding something dangerous.",
  true,
  None
)


//Forest
val clearing = new Area(
  "Timeless Clearing",
  "A serene glade where time seems to stand still. The air is still, and the sky above looks frozen in perpetual dusk.",
  false,
  None
)

val grove = new Area(
  "Duskbloom Grove",
  "A thicket of trees with glowing, violet blossoms. Time flows strangely here, as if the flowers bloom and wither within moments.",
  false,
  Some(groovePuzzle)
)

val thicket = new Area(
  "Whispering Thicket",
  "The wind carries soft whispers through the gnarled trees.\n" +
    "It's impossible to tell if the voices are warnings or merely echoes of the past.",
  false,
  Some(thicketPuzzle)
)

val glade = new Area(
  "Fading Glade",
  "A dying forest clearing where the grass turns gray and brittle. " +
  "The landscape shifts between vibrant and withered, as if caught between seasons.",
  false,
  Some(gladePuzzle)
)

val spring = new Area(
  "Eternal Spring",
  "A bubbling spring with crystal-clear water, surrounded by ancient stones. " +
  "The flow of the water seems to defy the passage of time.",
  false,
  Some(springPuzzle)
)

val hourglass = new Area(
  "Broken Hourglass",
  "A vast, eerie chamber surrounded by shattered timepieces. " +
  "The ground is littered with broken clocks and glass, and an oppressive silence hangs in the air.",
  true,
  None
)