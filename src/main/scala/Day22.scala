import scala.io.Source
import scala.collection.mutable.Buffer

object Day22 extends App:

  private val lines = readInput
  private val map = parseMap
  private val visitedMap = parseMap
  private val player = initPlayer
  private val commands = parseCommands
  private val visited = Buffer[(Int, Int)]()

  //printMap()
  task1()
  //printVisitedMap()
  //printVisited()

  private class Player(start: (Int, Int), private var facing: Int): // 0 means East, 1 South etc.

    private var pos = start

    def turn(direction: Char) =
      if direction == 'R' then
        facing = (facing + 1) % 4
      else if direction == 'L' then
        facing = (facing + 3) % 4 // same as -1

    def move(amount: Int) =
      if facing == 3 then
        yMove(amount, -1)
      else if facing == 0 then
        xMove(amount, 1)
      else if facing == 1 then
        yMove(amount, 1)
      else if facing == 2 then
        xMove(amount, -1)

    private def xMove(amount: Int, diff: Int) =
      var remaining = amount
      val yMap = map.transpose
      while remaining > 0 do
        visitedMap(pos(0))(pos(1)) = ">v<^"(facing)
        val (x,y) = pos
        var proposed = x + diff
        if proposed >= map.length || (diff > 0 && map(proposed)(y) == ' ') then // over right limit
          val firstRock = yMap(y).indexOf('#')
          val firstEmpty = yMap(y).indexOf('.')
          if firstEmpty < firstRock || firstRock == -1 then
            proposed = firstEmpty
          else
            remaining = 0
            proposed = x
        else if proposed < 0 || (diff < 0 && map(proposed)(y) == ' ') then // over left limit
          val lastRock = yMap(y).lastIndexOf('#')
          val lastEmpty = yMap(y).lastIndexOf('.')
          if lastEmpty > lastRock || lastRock == -1  then
            proposed = lastEmpty
          else
            remaining = 0
            proposed = x
        else if map(proposed)(y) == '#' then // rock
          remaining = 0
          proposed = x
        pos = (proposed, y)
        visited += pos
        remaining -= 1

    private def yMove(amount: Int, diff: Int) =
      var remaining = amount
      val yMap = map.transpose
      while remaining > 0 do
        visitedMap(pos(0))(pos(1)) = ">v<^"(facing)
        val (x,y) = pos
        var proposed = y + diff
        if proposed >= yMap.length || (diff > 0 && yMap(proposed)(x) == ' ') then // over bottom limit
          val firstRock = map(x).indexOf('#')
          val firstEmpty = map(x).indexOf('.')
          if firstEmpty < firstRock || firstRock == -1  then
            proposed = firstEmpty
          else
            remaining = 0
            proposed = y
        else if proposed < 0 || (diff < 0 && yMap(proposed)(x) == ' ') then // over top limit
          val lastRock = map(x).lastIndexOf('#')
          val lastEmpty = map(x).lastIndexOf('.')
          if lastEmpty > lastRock || lastRock == -1  then
            proposed = lastEmpty
          else
            remaining = 0
            proposed = y
        else if yMap(proposed)(x) == '#' then // rock
          remaining = 0
          proposed = y
        pos = (x, proposed)
        visited += pos
        remaining -= 1

    def password = 1000 * (pos(1) + 1) + 4 * (pos(0) + 1) + facing


  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day22_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseMap =
    val input = lines.dropRight(2)
    val width = input.maxBy(_.length).length
    val height = input.size
    val map = Array.tabulate(width, height)((_,_) => ' ')
    for line <- input.indices do
      for i <- input(line).indices do
        val value = input(line)(i)
        map(i)(line) = value
    map

  private def parseCommands =
    val input = lines.reverse.head.replaceAll("L", ",L,").replaceAll("R", ",R,")
    input.split(",").toVector


  private def initPlayer: Player =
    val test = map.transpose.head
    val x = map.transpose.head.indexOf('.') // first empty square on top row
    Player((x, 0), 0)

  private def runCommands() =
    def runCommand(command: String) =
      if command == "L" || command == "R" then
        player.turn(command.head)
      else
        player.move(command.toInt)

    commands.foreach(runCommand(_))
    println(player.password)


  def printMap() =
    for line <- map.transpose do
      println(line.mkString(""))

  def printVisitedMap() =
    for line <- visitedMap.transpose do
      println(line.mkString(""))

  def printVisited() =
    for line <- visited do
      println(line)

  def task1() =
    runCommands()