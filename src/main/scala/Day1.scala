
import scala.io.StdIn.readLine
import scala.collection.mutable.Buffer

class Day1:
  private var inventory = Buffer[Int]()

  def readInput() =
    var previousLine = ""
    var currentLine = readLine()
    while previousLine.nonEmpty || currentLine.nonEmpty do
      var elf = 0
      while currentLine.nonEmpty do
        elf += currentLine.toInt
        previousLine = currentLine
        currentLine = readLine()
      inventory += elf
      previousLine = currentLine
      currentLine = readLine()

  def maxCalories = inventory.max

  def topThree = inventory.sorted.reverse.take(3).sum

@main def start() =
  val task1 = Day1()
  task1.readInput()
  println(s"Most calories: ${task1.maxCalories}")
  println(s"Top 3 total: ${task1.topThree}")
