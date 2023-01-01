import scala.io.Source
import scala.collection.mutable.Buffer

object Day23 extends App:

  private val lines = readInput
  private var (map, elves) = generateGrid

  task1()
  task2()

  class Elf(private var pos: (Int, Int)):

    private var proposal = pos
    private var index = 0 // which direction is first at next proposal

    def proposed: Option[(Int, Int)] =
      if proposal == pos then
        None
      else
        Some(proposal)

    // Neighbors starting from N (N, NE, E, ...) //TODO: Fix this
    private def neighbors: Vector[Boolean] =
      val (x,y) = pos
      val top = Vector(map(x-1)(y-1), map(x)(y-1), map(x+1)(y-1))
      val right = Vector(map(x+1)(y))
      val bottom = Vector(map(x-1)(y+1), map(x)(y+1), map(x+1)(y+1)).reverse
      val left = Vector(map(x-1)(y))
      top ++ right ++ bottom ++ left

    def move() =
      val (x,y) = pos
      val (newX, newY) = proposal
      pos = proposal
      map(x)(y) = false
      map(newX)(newY) = true

    def updateProposal() =
      proposal = newProposal
      index = (index + 1) % 4

    private def newProposal: (Int, Int) =
      val others = neighbors
      val (x,y) = pos

      def noElvesAt(a: Int, b: Int, c: Int): Boolean =
        Vector(others(a), others(b), others(c)).forall(_ == false)

      val n_empty = noElvesAt(0, 1, 2)
      val s_empty = noElvesAt(4, 5, 6)
      val w_empty = noElvesAt(6, 7, 0)
      val e_empty = noElvesAt(2, 3, 4)

      val conditions = Vector(n_empty, s_empty, w_empty, e_empty)
      val diffs = Vector((0, -1), (0, 1), (-1, 0), (1, 0))

      if others.forall(_ == false) then
        pos
      else
        var i = 0
        var result = (0, 0)
        while i < 4 && result == (0,0) do
          val newIndex = (index + i) % 4
          if conditions(newIndex) then
            val (xDiff, yDiff) = diffs(newIndex)
            result = (x + xDiff, y + yDiff)
          i += 1
        if result != (0,0) then
          result
        else
          pos

      //TODO: Move directions after turn

  end Elf


  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day23_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def generateGrid =
    val inputX = lines.length
    val inputY = lines.head.length
    val grid = Array.ofDim[Boolean](3 * inputX, 3 * inputY)
    val elves = Buffer[Elf]()
    for line <- lines.indices do
      for col <- lines(line).indices do
        if lines(line)(col) == '#' then
          val x = col + inputX
          val y = line + inputY
          grid(x)(y) = true
          elves += Elf((x, y))
    (grid, elves.toVector)

  private def reset() =
    val (newMap, newElves) = generateGrid
    map = newMap
    elves = newElves

  private def round(): Int =
    elves.foreach(_.updateProposal())
    val proposals = elves.map(_.proposed)
    val movingIndexes = proposals.filter(_.isDefined).filter(target => proposals.count(_ == target) == 1).map(proposals.indexOf(_))
    for index <- movingIndexes do
      elves(index).move()
    movingIndexes.length

  private def smallestGridWithElves =
    val minX = map.indexWhere(_.contains(true))
    val maxX = map.length - map.reverse.indexWhere(_.contains(true))
    val transposed = map.transpose
    val minY = transposed.indexWhere(_.contains(true))
    val maxY = transposed.length - transposed.reverse.indexWhere(_.contains(true))
    val result = map.slice(minX, maxX).map(_.slice(minY, maxY))
    result

  def printGrid() =
    for line <- map.transpose do
      for i <- line do
        if i then
          print("#")
        else
          print(".")
      print("\n")

  def task1() =
    for i <- Range(0, 10) do
      round()
    val emptyTiles = smallestGridWithElves.flatten.count(_ == false)
    println(emptyTiles)

  def task2() =
    reset()
    var count = 0
    var moving = 1
    while moving > 0 do
      moving = round()
      count += 1
    println(count)







