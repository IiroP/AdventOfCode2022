import scala.io.Source
import scala.collection.mutable.Buffer

class Day11:

  private var divisor = 1
  private val lines = readInput()
  private var monkeys = parseInput()
  private var divideWorries = 3

  case class Monkey(startItems: Vector[Long], operation: Long => Long, test: Long => Boolean, iftrue: Int, iffalse: Int):
    private var items = startItems.toBuffer
    private var inspected = 0.toLong

    def inspectedCount = inspected

    def addItem(item: Long) =
      items += item

    def inspect() =
      for item <- items do
        inspected += 1
        val newLevel = (operation(item) / divideWorries) % divisor
        if test(newLevel) then
          monkeys(iftrue).addItem(newLevel)
        else
          monkeys(iffalse).addItem(newLevel)
      items = Buffer[Long]()


  private def readInput() =
    var file = Source.fromFile("input/day11_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseInput(): Vector[Monkey] =
    val result = Buffer[Monkey]()
    for monkey <- lines.sliding(6, 7) do
      val startingItems = monkey(1).split(": ")(1).split(", ").map(_.toLong).toVector
      val test = monkey(3).split(" ").reverse.head.toInt
      divisor *= test
      val iftrue = monkey(4).split(" ").reverse.head.toInt
      val iffalse = monkey(5).split(" ").reverse.head.toInt

      def operation(old: Long): Long =
        val calculation = monkey(2).split("=")(1).trim.split(" ")
        val operator = calculation(1)
        if calculation(2) == "old" then
          if operator == "+" then
            old + old
          else if operator == "*" then
            old * old
          else if operator == "/" then
            old / old
          else
            0
        else
          val num = calculation(2).toLong
          if operator == "+" then
            old + num
          else if operator == "-" then
            old - num
          else if operator == "*" then
            old * num
          else if operator == "/" then
            old / num
          else
            0

      def testDivide(num: Long): Boolean =
        num % test == 0

      result += Monkey(startingItems, operation, testDivide, iftrue, iffalse)

    result.toVector

  def playRound() =
    for monkey <- monkeys do
      monkey.inspect()

  def play20() =
    for i <- Range(0,20) do
      playRound()

  def businessLevel =
    monkeys.map(_.inspectedCount).sorted.takeRight(2).product

  def part2() =
    divideWorries = 1
    divisor = 1
    monkeys = parseInput()
    for i <- Range(0,10000) do
      playRound()
    monkeys.map(_.inspectedCount).sorted.takeRight(2).product


@main def day11_start() =
  val day = Day11()
  day.play20()
  println(day.businessLevel)
  println(day.part2())
