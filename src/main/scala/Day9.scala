import scala.io.Source
import scala.collection.mutable.Buffer
import scala.math.abs

object Day9 extends App:

  private val lines = readInput
  private val directions = parseDirections
  private var head = Pos(0,0)
  private val tail = Buffer.fill(9)(Pos(0,0))
  private val tailVisited = Buffer[Pos](Pos(0,0))
  private val tailVisited9 = Buffer[Pos](Pos(0,0))

  tasks()

  case class Pos(x: Int, y: Int):

    def add(xDiff: Int, yDiff: Int): Pos =
      Pos(x + xDiff, y + yDiff)

    def difference(other: Pos): (Int, Int) =
      val xDiff = this.x - other.x
      val yDiff = this.y - other.y
      (xDiff, yDiff)

  end Pos

  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day9_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseDirections: Vector[(String, Int)] =
    val result = Buffer[(String, Int)]()
    for line <- lines do
      val parts = line.strip().split(" ")
      result += ((parts(0), parts(1).toInt))
    result.toVector

  private def updateTail() =
    def clamp(value: Int) =
      if value > 0 then
        1
      else if value < 0 then
        -1
      else
        0

    for part <- Range(0, tail.length) do
      var (xDiff, yDiff) = (0,0)
      if part == 0 then
        val diff = head.difference(tail.head)
        xDiff = diff(0)
        yDiff = diff(1)
      else
        val diff = tail(part-1).difference(tail(part))
        xDiff = diff(0)
        yDiff = diff(1)

      if !(abs(xDiff) <= 1 && abs(yDiff) <= 1) then
        xDiff = clamp(xDiff)
        yDiff = clamp(yDiff)
        tail(part) = tail(part).add(xDiff, yDiff)

    tailVisited += tail.head
    tailVisited9 += tail.last

  def run() =
    for line <- directions do
      val command = line(0)
      for i <- Range(0, line(1)) do
        command match
          case "U" =>
            head = head.add(0,1)
          case "D" =>
            head = head.add(0,-1)
          case "R" =>
            head = head.add(1,0)
          case "L" =>
            head = head.add(-1,0)
        updateTail()

  def tasks() =
    run()
    val task1 = tailVisited.toSet.size
    val task2 = tailVisited9.toSet.size
    println(task1)
    println(task2)





