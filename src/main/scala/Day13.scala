import scala.io.Source
import scala.collection.mutable.Buffer

class Day13:

  private val lines = readInput
  private val packets = Buffer[Array[Matchable]]()

  private def readInput =
    var file = Source.fromFile("input/day13_input")
    val result = file.getLines().toVector
    file.close()
    result

  def parseLines() =
    def parseList(text: String): Array[Matchable] =
      if text.contains('[') || text.contains(']') then
        val bracket_start = text.indexOf('[')

        def ending: Int =
          var starts = 0
          var ends = 0
          var result = 0
          var i = 0
          while i < text.length && result == 0 do
            if text(i) == '[' then
              starts += 1
            else if text(i) == ']' then
              ends += 1

            if starts != 0 && starts == ends then
              result = i
            i += 1
          result

        val bracket_end = ending
        val before = text.take(bracket_start).split(",").filter(_ != "").map(_.toInt)
        val after = parseList(text.takeRight(text.length - bracket_end - 1))
        val middle = parseList(text.slice(bracket_start + 1, bracket_end))
        val result: Array[Matchable] = before.appended(middle) ++ after
        result
      else
        val result = Array[Matchable]()
        result ++ text.split(",").filter(_ != "").map(_.toInt)


    val pairsInRightOrder = Buffer[Int]()
    var index = 1
    for pair <- lines.sliding(2, 3) do
      val left = parseList(pair(0).drop(1).dropRight(1))
      var right = parseList(pair(1).drop(1).dropRight(1))
      val result = inRightOrder(left, right)
      if result.getOrElse(false) then
        pairsInRightOrder += index
        packets += left
        packets += right
      else
        packets += right
        packets += left
      index += 1

    pairsInRightOrder.sum

  private def inRightOrder(leftObject: Matchable, rightObject: Matchable): Option[Boolean] =
    var valueL = -1
    var valueR = -1
    var listL = Array[Any]()
    var listR = Array[Any]()

    leftObject match
      case x: Int => valueL = x
      case x: Array[Any] => listL = x
    rightObject match
      case y: Int => valueR = y
      case y: Array[Any] => listR = y

    if valueL != -1 && valueR == -1 then // right is List, left is Int
      listL = Array(valueL)
      valueL = -1
    else if valueR != -1 && valueL == -1 then // right is Int, left is list
      listR = Array(valueR)
      valueR = -1

    if valueL == valueR && valueL != -1 then // both are integers and value is the same
      None
    else if valueL == -1 && valueR == -1 then // both are lists
      var found = false
      var result = if listL.length == listR.length then None else Some(listL.length < listR.length) // fallback condition checks if the left list ends earlier
      var index = 0
      while !found && index < listL.length && index < listR.length do
        inRightOrder(listL(index), listR(index)) match
          case None => index += 1
          case Some(res) =>
            found = true
            result = Some(res)
      result
    else // both are integers and value is not the same
      Some(valueL < valueR)

  private def compareAndSwap(left: Int): Boolean =
    val leftPacket = packets(left)
    val rightPacket = packets(left + 1)
    if !inRightOrder(leftPacket, rightPacket).getOrElse(false) then
      packets(left) = rightPacket
      packets(left + 1) = leftPacket
      false
    else
      true


  def task2() =
    // add dividers
    val divider1 = Array[Matchable](Array[Matchable](2))
    val divider2 = Array[Matchable](Array[Matchable](6))
    packets += divider1
    packets += divider2

    var allInOrder = 0
    var start = 1

    while allInOrder < 4 do
      var ordered = true
      for i <- Range(start, packets.length-1, 2) do
        val alreadyInOrder = compareAndSwap(i)
        if !alreadyInOrder then
          ordered = false
      if ordered then
        allInOrder += 1
      else
        allInOrder = 0
      start = (start + 1) % 2

    val decoderKey = (packets.indexOf(divider1) + 1) * (packets.indexOf(divider2) + 1)
    decoderKey

@main def day13_start() =
  val day = Day13()
  println(day.parseLines())
  println(day.task2())