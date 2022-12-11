import scala.io.Source

class Day6:

  def readInput: Vector[String] =
    var file = Source.fromFile(s"day6_input")
    val lines = file.getLines().toVector
    file.close()
    lines

  def findMarker(line: String, length: Int) =
    var result = 0
    var current = length - 1
    while result == 0 && current < line.length do
      val unique = line.substring(current-length+1, current+1).toSet
      if unique.size == length then
        result = current
      current += 1
    result + 1


@main def day6_start() =
  val day = Day6()
  val input = day.readInput.head
  println(day.findMarker(input, 4))
  println(day.findMarker(input, 14))