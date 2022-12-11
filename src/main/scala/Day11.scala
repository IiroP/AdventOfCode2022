import scala.io.Source
import scala.collection.mutable.Buffer

class Day11:

  private val lines = readInput()
  private val monkeys = parseInput()

  case class Monkey(startItems: Vector[Int], operation: Int => Int, test: Int => Boolean, iftrue: Int, iffalse: Int):
    private var items = startItems.toBuffer
    private var inspected = 0

    def inspectedCount = inspected

    def addItem(item: Int) =
      items += item

    def inspect() =
      for item <- items do
        inspected += 1
        val newLevel = operation(item) / 3
        if test(newLevel) then
          monkeys(iftrue).addItem(newLevel)
        else
          monkeys(iffalse).addItem(newLevel)
      items = Buffer[Int]()


  private def readInput() =
    var file = Source.fromFile("input/day11_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseInput(): Vector[Monkey] =
    val result = Buffer[Monkey]()
    for monkey <- lines.sliding(6, 7) do
      val startingItems = monkey(1).split(": ")(1).split(", ").map(_.toInt).toVector
      val test = monkey(3).split(" ").reverse.head.toInt
      val iftrue = monkey(4).split(" ").reverse.head.toInt
      val iffalse = monkey(5).split(" ").reverse.head.toInt

      def operation(old: Int): Int =
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
          val num = calculation(2).toInt
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

      def testDivide(num: Int): Boolean =
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


@main def day11_start() =
  val day = Day11()
  day.play20()
  println(day.businessLevel)
