import scala.io.Source
import scala.collection.mutable.{Buffer, Map}

class Day12:

  private val lines = readInput
  private val gridX = lines(0).length
  private val gridY = lines.length
  private var target = (0,0)
  private var start = (0,0)
  private val grid = generateArray() // Array(y)(x)

  private def readInput =
    var file = Source.fromFile("input/day12_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def generateArray() =
    val letters = Array.ofDim[Int](gridY, gridX)
    for line <- lines.indices do
      for column <- lines(line).indices do
        var value = lines(line)(column)

        if value == 'S' then
          start = (line, column)
          value = 'a'
        else if value == 'E' then
          target = (line, column)
          value = 'z'

        letters(line)(column) = value
    letters

  private def coordinates =
    val result = Buffer[(Int,Int)]()
    for x <- Range(0, gridX) do
      for y <- Range(0, gridY) do
        val pos = (y, x)
        result += pos
    result.toVector


  private def possibleNext(coords: (Int, Int)) =
    val (line, column) = coords
    val possible = Buffer[(Int, Int)]()
    val value = grid(line)(column)

    def addPos(newLine: Int, newColumn: Int): Unit =
      if grid(newLine)(newColumn) <= value + 1 then
        possible += ((newLine, newColumn))

    if line != 0 then
      addPos(line - 1, column) // up
    if column != 0 then
      addPos(line, column - 1) // left
    if line != gridY - 1 then
      addPos(line + 1, column) // down
    if column != gridX - 1 then
      addPos(line, column+1) // right

    possible.toVector

  // From https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
  private def dijkstra(source: (Int, Int)) =
    val graph = coordinates
    val dist = Map[(Int,Int), Int]() //shortest distance to each node
    val prev = Map[(Int,Int), Option[(Int,Int)]]() //previous step on the way to each node
    var remaining = Buffer[(Int,Int)]()

    for point <- graph do
      dist(point) = 100000 // "infinity"
      prev(point) = None
      remaining += point
    dist(source) = 0

    while remaining.nonEmpty do
      val u = remaining.minBy( dist(_) )
      remaining -= u

      for v <- possibleNext(u).filter( remaining.contains(_) ) do
        val alt = dist(u) + 1
        if alt < dist(v) then
          dist(v) = alt
          prev(v) = Some(u)

    (dist, prev)

  def shortestRoute(startPos: (Int, Int), targetPos: (Int, Int)) =
    val (dist, prev) = dijkstra(startPos)
    val path = Buffer[(Int, Int)]()
    var u: Option[(Int, Int)] = Some(targetPos)
    if prev(u.get).isDefined || u.get == startPos then
      while u.isDefined do
        path.prepend(u.get)
        u = prev(u.get)
    path.length

  def task1: Int = shortestRoute(start, target) - 1

  def task2: Int =
    var shortest: Option[Int] = None
    for y <- grid.indices do
      val x = 0 // Only column 0 has possible starting positions
      if grid(y)(x) == 'a'.toInt then
        val route = shortestRoute((y,x), target) - 1
        if shortest.isEmpty then
          shortest = Some(route)
        else if shortest.isDefined && shortest.get > route then
          shortest = Some(route)
    shortest.getOrElse(0)


@main def day12_start() =
  val day = Day12()
  println(day.task1)
  println(day.task2)