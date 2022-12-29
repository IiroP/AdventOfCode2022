import scala.io.Source
import scala.math.abs

object Day20 extends App:

  private val lines = readInput
  private var initial = parseInput()
  private val maxIndex = initial.length - 1
  private var ordered = newList()

  task1()
  task2()

  case class Num(value: Long, initial: Int):

    override def toString: String = value.toString

  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day20_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseInput(key: Long = 1): Vector[Num] =
    lines.map(_.toInt).zipWithIndex.map(((num: Int, index: Int) => Num(key * num, index)))

  private def newIndex(initialIndex: Int, num: Long): Int =
    val index = initialIndex + num
    val remainingList = initial.length - 1
    val result = (index % remainingList).toInt
    (result + remainingList) % remainingList


  private def newList(times: Int = 1): Vector[Num] =
    val list = initial.toBuffer
    for i <- (0 until times) do
      for i <- initial do
        val initialIndex = list.indexOf(i)
        val index = newIndex(initialIndex, i.value)
        list.insert(index, list.remove(initialIndex))
    list.toVector

  private def nthNumber(n: Int): Long =
    val zero = ordered.find(_.value == 0).get
    val test = ordered.indexOf(zero)
    val index = (ordered.indexOf(zero) + n) % initial.length
    ordered(index).value

  def task1() =
    val nums = Vector(nthNumber(1000), nthNumber(2000), nthNumber(3000))
    println(nums.sum)

  def task2() =
    initial = parseInput(811589153)
    ordered = newList(10)
    val nums = Vector(nthNumber(1000), nthNumber(2000), nthNumber(3000))
    println(nums.sum)
