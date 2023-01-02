import scala.io.Source
import scala.math.{exp, max}
import scala.collection.mutable.{Buffer, Queue}

object Day24 extends App:

  private val lines = readInput
  private val cycleX = lines.head.length - 2
  private val cycleY = lines.length - 2
  private val cycleLen = cycleX * cycleY
  private val (map, blizzards) = generateMap
  private val yMap = map.clone().map(_.clone())

  /*
  * Original idea:
  * - for every position in grid, calculate turns when it is empty
  * - you can go to any nearby cell with (current turn + 1) as empty, including current
  * - if no possible targets, abandon branch
  * - choose shortest
  */

  task1()

  class Blizzard(private val start: (Int, Int), direction: Char):

    def isHorizontal =
      direction == 'E' || direction == 'W'

    def isVertical =
      direction == 'N' || direction == 'S'

    private def nextMove(coords: (Int, Int)): (Int, Int) =
      var (x,y) = coords
      if direction == 'N' then
        y -= 1
      else if direction == 'E' then
        x += 1
      else if direction == 'S' then
        y += 1
      else if direction == 'W' then
        x -= 1
      ((x - 1 + cycleX) % cycleX + 1, (y - 1 + cycleY) % cycleY + 1)

    def allPositions(direction: Option[Char] = None): Vector[(Int, Int)] =
      var fCycle = cycleLen
      if direction.isDefined then
        if direction.get == 'x' then
          fCycle = cycleY
        else if direction.get == 'y' then
          fCycle = cycleX

      val result = Buffer[(Int, Int)](start, nextMove(start))
      //val cycleLen = if this.isHorizontal then cycleX else cycleY
      while result.length < fCycle do
        result += nextMove(result.last)
      result.toVector

  end Blizzard

  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day24_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def generateMap =
    val width = lines.head.length
    val height = lines.length
    val grid = Array.fill(width, height)(Vector[Int]())
    val bliz = Buffer[Blizzard]()
    val letters = Map('>' -> 'E', '<' -> 'W', '^' -> 'N', 'v' -> 'S')
    for y <- lines.indices do
      for x <- lines(y).indices do
        val value = lines(y)(x)
        if letters.contains(value) then
          bliz += Blizzard((x,y), letters(value))
        else if value == '#' then
          grid(x)(y) = (0 until cycleLen).toVector
    (grid, bliz.toVector)


  private def calculateBlizzards() =
    val xBliz = blizzards.filter(_.isVertical)
    val yBliz = blizzards.filter(_.isHorizontal)

    def markBlizzards(variable: Char) =
      var fBliz = xBliz
      var fMap = map
      var fCycle = cycleY

      if variable == 'y' then
        fMap = yMap
        fCycle = cycleX
        fBliz = yBliz

      val reserved = fBliz.map(_.allPositions(Some(variable)))

      for turn <- 0 until fCycle do
        val nonFree = reserved.map(_.apply(turn))
        for point <- nonFree do
          val (x,y) = point
          if !fMap(x)(y).contains(turn) then
            fMap(x)(y) = fMap(x)(y).appended(turn)

    markBlizzards('x')
    markBlizzards('y')

  def bfsRoute(start: (Int, Int), target: (Int, Int)): Int =

    case class State(pos: (Int, Int), turn: Int):

      private def options: Vector[(Int, Int)] =
        val (x,y) = pos
        val result = Buffer[(Int,Int)]()
        if x > 0 then
          result += ((x - 1, y))
        if x < lines.head.length then
          result += ((x + 1, y))
        if y > 0 then
          result += ((x, y - 1))
        if y < lines.length then //TODO: Store top limit in variable
          result += ((x, y + 1))
        result += pos
        result.toVector

      def possibleNext: Vector[State] =
        val nextTurn = turn + 1
        //val turnValue = nextTurn % cycleLen
        val (x,y) = pos
        val turnX = nextTurn % cycleY
        val turnY = nextTurn % cycleX
        val neighbors = options
        val possible = Buffer[State]()
        for neighbor <- neighbors do
          val (newX, newY) = neighbor
          if !map(newX)(newY).contains(turnX) && !yMap(newX)(newY).contains(turnY) then
            possible += State(neighbor, nextTurn % (2* cycleLen))
        possible.toVector

    end State

    // https://en.wikipedia.org/wiki/Breadth-first_search#Pseudocode
    def bfs(root: State) =
      val queue = Queue[State]()
      val explored = scala.collection.mutable.Set(root)
      queue.enqueue(root)
      var answer: Option[Int] = None
      while queue.nonEmpty && answer.isEmpty do
        val v = queue.dequeue()
        if v.pos == target then
          answer = Some(v.turn)
        else
          for next <- v.possibleNext do
            if !explored.contains(next) then
              explored += next
              queue.enqueue(next)
      if answer.isEmpty then
        debugBFS(explored.toSet)
      answer.getOrElse(-1)

    // Used for debugging (draws the map, marks previous positions in yellow if can stay and red if not)
    def debugBFS(explored: Set[State]) =
      for turn <- 200 to 210 do
        val i = turn % cycleLen
        val i_x = turn % cycleY
        val i_y = turn % cycleX
        println(s"Turn $i")
        for y <- map.head.indices do
          print(y%10)
          for x <- map.indices do
            if map(x)(y).contains(i_x) || yMap(x)(y).contains(i_y) then
              if explored.contains(State((x,y), i-1)) then
                print(s"${Console.RED}#${Console.RESET}")
              else
                print(s"#")
            else if explored.contains(State((x,y), i-1)) then
              print(s"${Console.YELLOW}o${Console.RESET}")
            else
              print(".")
          print("\n")

    val startState = State(start, 0)
    bfs(startState)


  def task1() =
    calculateBlizzards()
    val result = bfsRoute((1,0), (cycleX, cycleY + 1))
    println(result)
