import scala.collection.mutable

class Location(val name: String, val areas: mutable.Buffer[Area]) {

  override def toString: String = name

  def areaList = areas

}

val dimensionalNexus = new Location("Dimensional Nexus", mutable.Buffer())
val forgottenLibrary = new Location(s"The Forgotten Library", mutable.Buffer())
val crystalCavern = new Location("The Crystal Cavern", mutable.Buffer())
val timeTwistedForest = new Location("Time-twisted Forest", mutable.Buffer())
