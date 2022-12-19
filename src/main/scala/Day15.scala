import scala.io.Source
import scala.math.abs
import scala.collection.mutable.Buffer

object Day15 extends App:

  private val lines = readInput
  private val lineNumber = 2000000
  private val sensors = parseSensors
  private val grid = generateGrid // true means it cannot contain beacon

  task1()
  //grid.printGrid()

  private def readInput =
    var file = Source.fromFile("input/day15_input")
    val result = file.getLines().toVector
    file.close()
    result

  class Sensor(val pos: (Int, Int), val beacon: (Int, Int)):

    private def distance(other: (Int, Int)): Int =
      abs(pos(0) - other(0)) + abs(pos(1) - other(1))

    def radius = distance(beacon)

    def inRadius(point: (Int, Int)): Boolean =
      distance(point) <= distance(beacon)

  end Sensor

  case class Grid(maxX: Int, maxY: Int, maxDist: Int):
    private val blocked = Array.ofDim[Boolean](maxX + 2 * maxDist) //maxY + 2 * maxDist)

    private def toReal(pos: (Int, Int)) = (pos(0) + maxDist, pos(1) + maxDist)

    private def toFake(pos: (Int, Int)) = (pos(0) - maxDist, pos(1) - maxDist)

    def block(pos: (Int, Int)) =
      val (x,y) = toReal(pos)
      if blocked.indices.contains(x) then
        blocked(x) = true
      else
        println(s"Pos $pos is outside the range")

    def printGrid() =
      for point <- blocked do
        if point then
          print("#")
        else
          print(".")
      print("\n")

    def countBlockedOnRow: Int =
      blocked.count(_ == true)

  private def parseSensors: Buffer[Sensor] =
    val result = Buffer[Sensor]()
    val pattern = raw"Sensor at x=(-*\d+), y=(-*\d+): closest beacon is at x=(-*\d+). y=(-*\d+)".r
    for line <- lines do
      line match
        case pattern(sensorX, sensorY, beaconX, beaconY) =>
          val sensor = Sensor((sensorX.toInt, sensorY.toInt), (beaconX.toInt, beaconY.toInt))
          result += sensor
    result

  private def generateGrid =
    val maxX = sensors.maxBy(_.pos(0)).pos(0)
    val maxY = sensors.maxBy(_.pos(1)).pos(1)
    val maxDist = sensors.maxBy(_.radius).radius

    val result = Grid(maxX, maxY, maxDist)

    for sensor <- sensors do
      val (x,y) = sensor.pos
      for xDiff <- Range(-(sensor.radius), sensor.radius) do
        val newX = x + xDiff
        val newY = lineNumber
        if sensor.inRadius((newX, newY)) && (newX, newY) != sensor.beacon then
          result.block((newX, newY))
          //result.block((x,y))

    result

  def task1() =
    val answer = grid.countBlockedOnRow
    println(answer)

