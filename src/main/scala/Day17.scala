import scala.io.Source
import scala.collection.mutable.{Buffer, Map}
import scala.math.max

object Day17 extends App:

  private val gasDirections = readInput
  private var gasIndex = 0
  private var gasCycleLooped = false
  private var rockNumber = 0 // first rock is 1
  private var grid = Array.ofDim[Boolean](4000, 7)
  private var highestRock = -1
  private var reference = Array.ofDim[Boolean](1,7)

  private class Rock(startPoints: Vector[(Int, Int)], val number: Int, startY: Int = 0):
    private var points = startPoints
    points =  points.map(((x, y) => (x, y + startY)))

    private def leftLimit: Int = points.minBy(_(0)).apply(0)
    private def rightLimit: Int = points.maxBy(_(0)).apply(0)
    private def topLimit: Int = points.maxBy(_(1)).apply(1)
    private def bottomLimit: Int = points.minBy(_(1)).apply(1)

    private def checkIfPossible(proposed: Vector[(Int, Int)]): Boolean =
      var possible = true
      for point <- proposed do
        val (x,y) = point
        if x < 0 || y < 0 || y >= grid.length || x >= grid.head.length then // check it's inside the grid
          possible = false
        else if grid(y)(x) then // check there's nothing else at that point
          possible = false
      possible

    def move(xDiff: Int, yDiff: Int): Boolean =
      val newPoints = points.map(((x, y) => (x + xDiff, y + yDiff))) // Extra () by IntelliJ...
      if checkIfPossible(newPoints) then
        points = newPoints
        true
      else
        false

    def lock() =
      for point <- points do
        val (x,y) = point
        grid(y)(x) = true
      val top = this.topLimit
      if grid(top + 1).forall(_ == false) then
        highestRock = top

  end Rock

  private val rocks = rockOptions

  //test()
  //task1()
  task2()
  //printGrid()

  private def rockOptions =
    val rock0 = Vector((2,0), (3,0), (4,0), (5,0))
    val rock1 = Vector((3,2), (2,1), (3,1), (4,1), (3,0))
    val rock2 = Vector((4,2), (4,1), (4,0), (3,0), (2,0))
    val rock3 = Vector((2,3), (2,2), (2,1), (2,0))
    val rock4 = Vector((2,1), (3,1), (2,0), (3,0))
    Vector(rock0, rock1, rock2, rock3, rock4)

  private def readInput =
    var file = Source.fromFile("input/day17_input")
    val result = file.getLines().toVector.head
    file.close()
    result

  private def nextDirection(): Char =
    val result = gasDirections(gasIndex)
    if !gasCycleLooped && gasIndex == 39 then
      gasCycleLooped = true
    gasIndex = (gasIndex + 1) % gasDirections.length
    result

  private def nextRock(): Rock =
    val index = rockNumber % rocks.length
    rockNumber += 1
    Rock(rocks(index), index, highestRock + 4)

  def printGrid() =
    for row <- grid.reverse do
      for point <- row do
        if point then
          print("#")
        else
          print(".")
      print("\n")

  def task2() =
    grid = Array.ofDim[Boolean](10000000, 7)
    val pairs = Map[(Int, Int), (Int, Int)]() // (Rocknumber, highestRock) -> (rockIndex, gasIndex)
    var cycleFirst = 0
    var cycleLast = 0

    def moveByGas(rock: Rock, direction: Char): Boolean =
      direction match
        case '>' => rock.move(1, 0)
        case '<' => rock.move(-1, 0)
        case _ => false

    def gridSliceFrom(row: Int) =
      grid.slice(row - 20, row)

    def sameValuesBelow(row1: Int, row2: Int): Boolean =
      var left = gridSliceFrom(row1)
      var right = gridSliceFrom(row2)
      var same = true
      var index = 0
      while same && index < left.length do
        if !(left(index) sameElements right(index)) then
          same = false
        index += 1
      same

    while rockNumber < 150000 do
      val rock = nextRock()
      var move = 0
      var stuck = false

      val pair = ((rock.number, gasIndex))
      val existingPairs = pairs.values.toVector
      if gasCycleLooped then
        val count = existingPairs.count(_ == pair)
        if count == 1 then
          val index = pairs.filter(_._2 == pair).head._1._1
          val newLength = rockNumber - index
          val previousHigh = pairs.filter(_._2 == pair).head._1._2
          if newLength > cycleLast - cycleFirst && (index < cycleFirst || cycleFirst == 0) then
            if previousHigh > 40 && sameValuesBelow(highestRock-40, previousHigh-40) then
              cycleLast = rockNumber - 1
              cycleFirst = index - 1
          end if

        if count < 2 then
          pairs((rockNumber, highestRock)) = pair

      while !stuck do
        var success = true
        if move % 2 == 0 then // Jet
          val direction = nextDirection()
          moveByGas(rock, direction)
        else // Fall
          success = rock.move(0, -1)

        move += 1

        if !success then
          rock.lock()
          stuck = true

      end while


    val cycleLength = cycleLast - cycleFirst

    val beforeCycle = task1(cycleFirst)
    val afterCycle = task1(cycleLast)
    val cycleAddition = afterCycle-beforeCycle
    val cycleCount = (1000000000000L - cycleFirst) / cycleLength
    val after = (1000000000000L - cycleFirst) % cycleLength
    val height_middle = cycleCount * cycleAddition
    val height_other = task1(cycleLast + after.toInt) - afterCycle + beforeCycle
    println(height_middle + height_other)

  def task1(amoutOfRocks: Int = 2022) =
    gasIndex = 0
    rockNumber = 0 // first rock is 1
    grid = Array.ofDim[Boolean](1000000, 7)
    highestRock = -1

    def moveByGas(rock: Rock, direction: Char): Boolean =
      direction match
        case '>' => rock.move(1, 0)
        case '<' => rock.move(-1, 0)
        case _ => false


    while rockNumber < amoutOfRocks do
      val rock = nextRock()
      var move = 0
      var stuck = false

      while !stuck do
        var success = true
        if move % 2 == 0 then // Jet
          val direction = nextDirection()
          moveByGas(rock, direction)
        else // Fall
          success = rock.move(0, -1)

        move += 1

        if !success then
          rock.lock()
          stuck = true
      end while

    val result = highestRock + 1
    result
