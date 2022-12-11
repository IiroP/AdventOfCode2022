import scala.io.Source
import scala.collection.mutable.Buffer

class Day10:

  private val lines = readInput()
  private var x = 1
  private var cycles = 1
  private val signalStrengths = Buffer[Int]()
  private val outputLines = Buffer[String]()
  private var currentLine = ""

  private def readInput() =
    var file = Source.fromFile("day10_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def cycleDone() =
    val pixel = (cycles - 1) % 40
    val range = x-1 to x+1
    if range.contains(pixel) then
      currentLine += "##"
    else
      currentLine += ".."

    if pixel == 39 then
      outputLines.append(currentLine)
      currentLine = ""

    if cycles == 20 || (cycles - 20) % 40 == 0 then
      val signal = x * cycles
      signalStrengths += signal
    cycles += 1

  def processInput() =
    for line <- lines do
      val parts = line.split(" ")
      cycleDone()
      if parts(0) == "addx" then
        cycleDone()
        x += parts(1).toInt

  def sumOfSignals = signalStrengths.sum

  def printOutput() =
    for line <- outputLines do
      println(line)
    println(currentLine)

@main def day10_start() =
  val day = Day10()
  day.processInput()
  println(day.sumOfSignals)
  day.printOutput()
