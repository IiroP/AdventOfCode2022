import scala.io.Source

object Common:

  def readInput(day: Int): Vector[String] =
    var file = Source.fromFile(s"day${day}_input")
    val lines = file.getLines().toVector
    file.close()
    lines
