import scala.io.Source
import scala.collection.mutable.{Buffer, Map}

class Day5:

  private var lines = Vector[String]()
  private var initial = Vector[String]()
  private var instructions = Vector[Vector[String]]()
  private val boxes = Map[Int, Buffer[Char]]()

  def readInput() =
    var file = Source.fromFile("day5_input")
    lines = file.getLines().toVector
    initial = lines.takeWhile(_ != "").reverse
    instructions = lines.reverse.takeWhile(_ != "").map(_.split(" ").toVector).reverse

    file.close()

  def parseInstructions1() =
    for line <- instructions do
      val amount = line(1).toInt
      val start = line(3).toInt
      val target = line(5).toInt
      val moved = boxes(start).take(amount).reverse
      boxes(start) --= moved
      boxes(target).prependAll(moved)

  def parseInstructions2() =
    for line <- instructions do
      val amount = line(1).toInt
      val start = line(3).toInt
      val target = line(5).toInt
      val moved = boxes(start).take(amount)
      boxes(start) --= moved
      boxes(target).prependAll(moved)

  def parseInitial() =
    val firstLine = initial(0)
    val nums = firstLine.split(" ").filter(_.nonEmpty).map(_.toInt).toVector
    val lastChar = firstLine.split(" ").last
    val maxIndex = firstLine.indexOf(lastChar)

    nums.foreach( boxes(_) = Buffer() )

    for line <- initial.tail do
      var index = 1
      while index <= maxIndex && index < line.length do
        val num = firstLine(index).toString.toInt
        if line(index) != ' ' then
          boxes(num).prepend(line(index))
        index += 4

  def topBoxes =
    boxes.values.toVector.map(_.head).mkString

  def boxesValue = boxes

@main def day5_start() =
  val day = Day5()
  day.readInput()
  day.parseInitial()
  println(day.boxesValue)
  //day.parseInstructions1()
  day.parseInstructions2()
  println(day.topBoxes)