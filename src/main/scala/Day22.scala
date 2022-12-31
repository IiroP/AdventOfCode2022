import scala.io.Source
import scala.collection.mutable.Buffer

object Day22 extends App:

  private val cubeSize = 50
  private var zoneStarts = Vector((50,0), (100,0), (50,50), (0,100), (50,100), (0,150))
  private val zoneBorders = Map(
    0 -> ("1L", "2U", "3L", "5L"),
    1 -> ("4R", "2R", "0R", "5D"),
    2 -> ("1D", "4U", "3U", "0D"),
    3 -> ("4L", "5U", "0L", "2L"),
    4 -> ("1R", "5R", "3R", "2D"),
    5 -> ("4D", "1U", "0U", "3D")
  )
  private val lines = readInput
  private val map = parseMap
  private val visitedMap = parseMap
  private val visitedMap3D = parseMap
  private var player = initPlayer()
  private val commands = parseCommands
  private val visited = Buffer[(Int, Int)]()

  //printMap()
  task1()
  task2()
  //printVisitedMap()
  //printVisited()

  private class Player(start: (Int, Int), private var facing: Int, val cube: Boolean = false): // 0 means East, 1 South etc.

    private var pos = start

    def turn(direction: Char) =
      if direction == 'R' then
        facing = (facing + 1) % 4
      else if direction == 'L' then
        facing = (facing + 3) % 4 // same as -1

    private def zoneAt(coords: (Int, Int)) =
      val (x,y) = coords
      val limits = (x - x % cubeSize, y - y % cubeSize)
      zoneStarts.indexOf(limits)

    def move(amount: Int): Unit =
      if cube then
        var remaining = 0
        if facing == 3 then
          remaining = yMove3D(amount, -1)
        else if facing == 0 then
          remaining = xMove3D(amount, 1)
        else if facing == 1 then
          remaining = yMove3D(amount, 1)
        else if facing == 2 then
          remaining = xMove3D(amount, -1)

        if remaining > 0 then
          move(remaining)
      else
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
          val firstEmpty = yMap(y).indexOf('.')
          proposed = firstEmpty
        else if proposed < 0 || (diff < 0 && map(proposed)(y) == ' ') then // over left limit
          val lastEmpty = yMap(y).lastIndexOf('.')
          proposed = lastEmpty

        if map(proposed)(y) == '#' then // rock
          remaining = 0
          proposed = x
        pos = (proposed, y)
        //visited += pos
        remaining -= 1

    /**
     * Calculate new position and facing when wrapping aroung
     *
     * @param currentZone Current zone number (0-5)
     * @param currentBorder Current border (RDLU)
     * @param target Target zone and border (e.g. 0D, 3R)
     * @param borderCoordinate Current distance from zone start along the border
     * @return New coordinates and facing
     */
    private def borderPosAndFacing(currentZone: Int, currentBorder: Char, target: String, borderCoordinate: Int): ((Int,Int), Int) =
      val targetZone = target(0).asDigit
      val targetBorder = target(1)
      val borders = "RDLU"
      var difference = (borders.indexOf(targetBorder) + 2 - borders.indexOf(currentBorder) + 4) % 4
      var newPos = (0,0)
      var newBorderCoord = borderCoordinate
      if difference == 2 then
        newBorderCoord = cubeSize - borderCoordinate - 1

      // New facing
      var newFacing = (facing + difference + 4) % 4
      if difference == 0 then // opposite side
        newFacing = facing
      else if difference == 2 then // same side
        newFacing = (facing + 2) % 4

      // New position
      if targetBorder == 'R' then
        val (x,y) = zoneStarts(targetZone)
        newPos = (x + cubeSize - 1, y + newBorderCoord)
      else if targetBorder == 'L' then
        val (x,y) = zoneStarts(targetZone)
        newPos = (x, y + newBorderCoord)
      else if targetBorder == 'U' then
        val (x,y) = zoneStarts(targetZone)
        newPos = (x + newBorderCoord, y)
      else if targetBorder == 'D' then
        val (x,y) = zoneStarts(targetZone)
        newPos = (x + newBorderCoord, y + cubeSize - 1)

      (newPos, newFacing)


    private def xMove3D(amount: Int, diff: Int): Int =
      var remaining = amount
      val yMap = map.transpose
      while remaining > 0 do
        visitedMap3D(pos(0))(pos(1)) = ">v<^"(facing)
        val (x,y) = pos
        var proposedX = x + diff
        var proposedY = y
        var proposedFacing = facing
        if proposedX >= map.length || (diff > 0 && map(proposedX)(y) == ' ') then // over right limit
          val currentZone = zoneAt(pos)
          val target = zoneBorders(currentZone)._1
          val borderCoordinate = y - zoneStarts(currentZone)._2
          var (newPos, newFacing) = borderPosAndFacing(currentZone, 'R', target, borderCoordinate)
          proposedX = newPos(0)
          proposedY = newPos(1)
          proposedFacing = newFacing
        else if proposedX < 0 || (diff < 0 && map(proposedX)(y) == ' ') then // over left limit
          val currentZone = zoneAt(pos)
          val target = zoneBorders(currentZone)._3
          val borderCoordinate = y - zoneStarts(currentZone)._2
          var (newPos, newFacing) = borderPosAndFacing(currentZone, 'L', target, borderCoordinate)
          proposedX = newPos(0)
          proposedY = newPos(1)
          proposedFacing = newFacing

        if map(proposedX)(proposedY) == '#' then // rock
          remaining = 0
          proposedX = x
          proposedY = y
          proposedFacing = facing
        pos = (proposedX, proposedY)
        visited += pos
        remaining -= 1
        if proposedFacing != facing then
          facing = proposedFacing
          return remaining
      remaining


    private def yMove3D(amount: Int, diff: Int): Int =
      var remaining = amount
      val yMap = map.transpose
      while remaining > 0 do
        visitedMap3D(pos(0))(pos(1)) = ">v<^"(facing)
        val (x,y) = pos
        var proposedX = x
        var proposedY = y + diff
        var proposedFacing = facing
        if proposedY >= yMap.length || (diff > 0 && yMap(proposedY)(x) == ' ') then // over bottom limit
          val currentZone = zoneAt(pos)
          val target = zoneBorders(currentZone)._2
          val borderCoordinate = x - zoneStarts(currentZone)._1
          var (newPos, newFacing) = borderPosAndFacing(currentZone, 'D', target, borderCoordinate)
          proposedX = newPos(0)
          proposedY = newPos(1)
          proposedFacing = newFacing
        else if proposedY < 0 || (diff < 0 && yMap(proposedY)(x) == ' ') then // over top limit
          val currentZone = zoneAt(pos)
          val target = zoneBorders(currentZone)._4
          val borderCoordinate = x - zoneStarts(currentZone)._1
          var (newPos, newFacing) = borderPosAndFacing(currentZone, 'U', target, borderCoordinate)
          proposedX = newPos(0)
          proposedY = newPos(1)
          proposedFacing = newFacing

        if map(proposedX)(proposedY) == '#' then // rock
          remaining = 0
          proposedX = x
          proposedY = y
          proposedFacing = facing
        pos = (proposedX, proposedY)
        visited += pos
        remaining -= 1
        if proposedFacing != facing then
          facing = proposedFacing
          return remaining
      remaining

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
        //visited += pos
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


  private def initPlayer(cube: Boolean = false, overrideX: Option[Int] = None): Player =
    val test = map.transpose.head
    var x = map.transpose.head.indexOf('.') // first empty square on top row
    if overrideX.isDefined then
      x = overrideX.get
    Player((x, 0), 0, cube)

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

  def task2() =
    player = initPlayer(true)
    runCommands()
    //for line <- visitedMap3D.transpose do
    //  println(line.mkString(""))