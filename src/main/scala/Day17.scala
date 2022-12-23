import scala.io.Source

object Day17 extends App:

  private val gasDirections = readInput
  private var gasIndex = 0
  private var rockNumber = 0 // first rock is 1
  private val grid = Array.ofDim[Boolean](4000, 7)
  private var highestRock = -1

  private class Rock(startPoints: Vector[(Int, Int)], startY: Int = 0):
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
  task1()
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
    gasIndex = (gasIndex + 1) % gasDirections.length
    result

  private def nextRock(): Rock =
    val index = rockNumber % rocks.length
    rockNumber += 1
    Rock(rocks(index), highestRock + 4)

  def printGrid() =
    for row <- grid.reverse do
      for point <- row do
        if point then
          print("#")
        else
          print(".")
      print("\n")

  def task1() =
    def moveByGas(rock: Rock, direction: Char): Boolean =
      direction match
        case '>' => rock.move(1, 0)
        case '<' => rock.move(-1, 0)
        case _ => false


    while rockNumber < 2022 do
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

    println(highestRock + 1)


