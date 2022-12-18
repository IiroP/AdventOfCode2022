import scala.io.Source
import scala.math.{min, max}
import scala.collection.mutable.Buffer

object Day14 extends App:

  private val lines = readInput
  private var lowest = 8
  private var leftX = 501
  private val grid = linesToGrid
  private val sandStart = (500, 0)

  simulate()
  //grid.printGrid()

  case class Grid(minX: Int, maxX: Int, maxY: Int):
    private val blocked = Array.ofDim[Boolean](maxX - minX + 3, maxY + 2)

    private def toReal(pos: (Int, Int)) = (pos(0) - minX + 1, pos(1))

    private def toFake(pos: (Int, Int)) = (pos(0) + minX - 1, pos(1))

    def isEmpty(pos: (Int, Int)) =
      val (x, y) = toReal(pos)
      !blocked(x)(y)

    def block(pos: (Int, Int)) =
      val (x, y) = toReal(pos)
      blocked(x)(y) = true

    def nextPosition(current: (Int, Int)): (Int, Int) =
      val (x,y) = toReal(current)
      if y == maxY || x == 0 then // abyss
        toFake(x, y)
      else if !blocked(x)(y + 1) then
        toFake(x, y + 1)
      else if !blocked(x - 1)(y + 1) then
        toFake(x - 1, y + 1)
      else if !blocked(x + 1)(y + 1) then
        toFake(x + 1, y + 1)
      else
        toFake(x, y)

    def printGrid() =
      for line <- blocked.transpose do
        for point <- line do
          if point then
            print("#")
          else
            print(".")
        print("\n")


  private def readInput =
    var file = Source.fromFile("input/day14_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def linesToGrid =
    var minX = 494
    val minY = 0
    var maxX = 503
    var maxY = 9
    val rocks = Buffer[(Int, Int)]()

    // Helper method to calculate rock positions
    def rockCoordinates(rules: Array[(Int,Int)]): Vector[(Int,Int)] =
      var prev = rules.head
      val result = Buffer[(Int,Int)]()
      for point <- rules.tail do
        if prev(0) == point(0) then // x is same
          val start = min(prev(1), point(1))
          val end = max(prev(1), point(1))
          result ++= (start to end).toVector.map( (prev(0), _) )
        else if prev(1) == point(1) then // y is same
          val start = min(prev(0), point(0))
          val end = max(prev(0), point(0))
          result ++= (start to end).toVector.map( (_, prev(1)) )
        prev = point
      result.toVector

    def toCoords(pair: Array[String]): (Int, Int) =
      val temp = pair.map(_.strip().toInt)
      (temp(0), temp(1))

    // Calculate rock positions for all rules
    for line <- lines do
      val points = line.split("->").map( _.split(",") ).map( toCoords(_) )
      val left = points.minBy( coords => coords(0) )
      val right = points.maxBy( coords => coords(0) )
      val bottom = points.maxBy( coords => coords(1) )
      minX = min(minX, left(0))
      maxX = max(maxX, right(0))
      maxY = max(maxY, bottom(1))
      rocks ++= rockCoordinates(points)

    lowest = maxY
    leftX = minX
    // Create grid
    val result = Grid(minX, maxX, maxY)
    for rock <- rocks do
      result.block(rock)

    result

  private def simulate() =
    var abyss = false
    var sandCounter = 0

    while !abyss do
      var pos = sandStart
      var rest = false
      while !rest do
        val next = grid.nextPosition(pos)

        if next(1) == lowest then
          abyss = true

        if next == pos then
          rest = true
        else
          pos = next
      grid.block(pos)
      if !abyss then
        sandCounter += 1

    println(sandCounter)




