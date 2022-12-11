
import scala.io.Source

class Day3:

  private var lines = Vector[String]()

  def readInput() =
    var file = Source.fromFile("day3_input")
    lines = file.getLines().toVector
    file.close()

  private def findCommon(line: String): Char =
    val firstHalf = line.take(line.length / 2)
    val secondHalf = line.takeRight(line.length / 2)
    var common: Option[Char] = None
    for char <- firstHalf do
      if secondHalf.contains(char) then
        common = Some(char)

    common.getOrElse(' ')

  private def findCommon3(lines: Vector[String]): Char =
    var common: Option[Char] = None
    for char <- lines(0) do
      if lines(1).contains(char) && lines(2).contains(char) then
        common = Some(char)

    common.getOrElse(' ')

  private def letterValue(letter: Char): Int =
    if letter.isLower then
      letter.toInt - 'a'.toInt + 1
    else if letter.isUpper then
      letter.toInt - 'A'.toInt + 27
    else
      0

  def calculateValue: Int =
    lines.map(findCommon(_)).map(letterValue(_)).sum

  def calculateGroupValue: Int =
    lines.grouped(3).map( findCommon3(_) ).map( letterValue(_) ).sum


@main def day3_start =
  val day = Day3()
  day.readInput()
  println(day.calculateValue)
  println(day.calculateGroupValue)