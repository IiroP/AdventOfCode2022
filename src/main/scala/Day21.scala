import scala.collection.mutable.{Buffer, Map}
import scala.io.Source

object Day21 extends App:

  private val lines = readInput
  private var monkeys = parseMonkeys

  task1()
  task2()

  case class Monkey(name: String, var number: Option[Double], var operation: Option[(String, String, String)]):

    def ready: Boolean = number.isDefined

    def operationAsText: String =
      if name == "humn" then
        "humn"
      else if operation.isDefined then
        val (monkey1, symbol, monkey2) = operation.get
        val left = monkeys(monkey1).operationAsText
        val right = monkeys(monkey2).operationAsText
        try
          symbol match
            case "+" => (left.toDouble + right.toDouble).toString
            case "-" => (left.toDouble - right.toDouble).toString
            case "/" => (left.toDouble / right.toDouble).toString
            case "*" => (left.toDouble * right.toDouble).toString
        catch
          case _: Throwable =>
            s"($left $symbol $right)"
      else
        number.get.toString

    def operationAsVector: Vector[Any] =
      if name == "humn" then
        Vector("humn")
      else if operation.isDefined then
        val (monkey1, symbol, monkey2) = operation.get
        val left = monkeys(monkey1).operationAsVector
        val right = monkeys(monkey2).operationAsVector
        try
          left match
            case leftDouble: Vector[Double] =>
              right match
                case rightDouble: Vector[Double] =>
                  symbol match
                    case "+" => Vector(leftDouble.head + rightDouble.head)
                    case "-" => Vector(leftDouble.head - rightDouble.head)
                    case "/" => Vector(leftDouble.head / rightDouble.head)
                    case "*" => Vector(leftDouble.head * rightDouble.head)
                case _ =>
                  Vector(left, symbol, right)
            case _ =>
              Vector(left, symbol, right)
        catch
          case _: Throwable =>
            Vector(left, symbol, right)
      else
        Vector(number.get)

    def calculate() =
      if operation.isDefined then
        val (monkey1, symbol, monkey2) = operation.get
        val left = monkeys(monkey1).number.get
        val right = monkeys(monkey2).number.get
        if symbol == "+" then
          this.number = Some(left + right)
        else if symbol == "-" then
          this.number = Some(left - right)
        else if symbol == "*" then
          this.number = Some(left * right)
        else if symbol == "/" then
          this.number = Some(left / right)

    def waitingFor: Vector[String] =
      val result = Buffer[String]()
      if operation.isDefined then
        val (monkey1, symbol, monkey2) = operation.get
        result += monkey1
        result += monkey2
      result.filterNot(monkeys(_).ready).toVector

  end Monkey

  case class Calculation(left: Vector[Any], operator: String, right: Vector[Any])


  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day21_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseMonkeys: Map[String, Monkey] =
    val result = Map[String, Monkey]()
    val operational = raw"(\w{4}): (\w{4}) (\W) (\w{4})".r
    val ready = raw"(\w{4}): (\d+)".r
    for line <- lines do
      line match
        case operational(name, monkey1, symbol, monkey2) =>
          val monkey = Monkey(name, None, Some((monkey1, symbol, monkey2)))
          result(name) = monkey
        case ready(name, number) =>
          val monkey = Monkey(name, Some(number.toDouble), None)
          result(name) = monkey
    result

  private def calculateValues(): Long =
    recursive("root")

    def recursive(name: String): Unit =
      val thisMonkey = monkeys(name)
      val missing = thisMonkey.waitingFor
      if missing.nonEmpty then
        missing.foreach( recursive(_) )
      thisMonkey.calculate()

    monkeys("root").number.get.toLong

  private def solveEquation(equation: Vector[Any], answer: Double): Double =
    var left = equation
    var result = answer
    while left != Vector("humn") do
      left match
        case Vector(newLeft: Vector[Any], operator: String, newRight: Vector[Any]) =>
          // If left is only a number and right is the remaining equation
          if newLeft.length == 1 && newRight.length > 1 then
            left = newRight
            newLeft.head match
              case num: Double =>
                if operator == "+" then
                  result -= num
                else if operator == "-" then
                  result *= -1
                  result += num
                else if operator == "*" then
                  result /= num
                else if operator == "/" then
                  result = num / result
          // If right is only a number and left is the remaining equation
          else if newLeft.length > 1 && newRight.length == 1 then
            left = newLeft
            newRight.head match
              case num: Double =>
                if operator == "+" then
                  result -= num
                else if operator == "-" then
                  result += num
                else if operator == "*" then
                  result /= num
                else if operator == "/" then
                  result *= num
          // Last phase, text vs number
          else if newLeft.length == 1 && newRight.length == 1 then
            newLeft.head match
              case num: Double =>
                left = newRight
                if operator == "+" then
                  result -= num
                else if operator == "-" then
                  result += num
                else if operator == "*" then
                  result /= num
                else if operator == "/" then
                  result = num / result
              case text: String =>
                left = newLeft
                newRight.head match
                  case rightNumber: Double =>
                    if operator == "+" then
                      result -= rightNumber
                    else if operator == "-" then
                      result += rightNumber
                    else if operator == "*" then
                      result /= rightNumber
                    else if operator == "/" then
                      result *= rightNumber
        case _ =>
          println(left)
    result



  def task1() =
    println(calculateValues())
    //println((monkeys("vpmn").number, monkeys("pqtt").number))

  def task2() =
    monkeys = parseMonkeys
    val calculations = monkeys("root").waitingFor.map(monkeys(_).operationAsVector)
    //println(monkeys("root").waitingFor.map(monkeys(_).operationAsText))
    val equation = calculations(0)
    calculations(1).head match
      case number: Double =>
        val result = solveEquation(calculations(0), number)
        println(result.toLong)
