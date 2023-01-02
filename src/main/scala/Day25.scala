import scala.io.Source
import scala.math.pow

object Day25 extends App:

  private val lines = readInput

  task1()

  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day25_input")
    val result = file.getLines().toVector
    file.close()
    result

  def snafuToLong(input: String) =
    val symbols = Map('-' -> -1, '=' -> -2, '0' -> 0, '1' -> 1, '2' -> 2)
    var result: Long = 0
    for i <- input.indices do
      val multiplier = symbols(input.reverse(i)).toLong
      val base = math.pow(5, i).toLong
      result += multiplier * base
    result

  def intToSnafu(input: Long) =
    val symbols = Map(-2 -> '=', -1 -> '-', 0 -> '0', 1 -> '1', 2 -> '2')

    def findLastPlace(snafu: Long) =
      var answer = 0
      while snafu > maxFromX(answer) do
        answer += 1
      answer

    def maxFromX(last: Int) =
      var result: Long = 0
      for i <- 0 to last do
        val num = 2 * pow(5, i).toLong
        result += num
      result

    val lastPlace = findLastPlace(input)
    var remaining = input
    var result = ""
    for i <- (0 to lastPlace).reverse do
      val divider = pow(5, i).toLong
      var num = remaining / divider
      val remainder = remaining % divider
      val maxIfZero = maxFromX(i - 1)
      if remainder > maxIfZero then
        num += 1
      else if remainder < -maxIfZero then
        num -= 1
      remaining -= (num * divider)
      result = result + symbols(num.toInt)

    result

  def calculateSum =
    var result: Long = 0
    for line <- lines do
      result += snafuToLong(line)
    result

  def task1() =
    val result = calculateSum
    println(result)
    println(intToSnafu(result))
