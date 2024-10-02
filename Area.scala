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
