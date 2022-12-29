import scala.io.Source
import scala.collection.mutable.Buffer

object Day18 extends App:

  private val lines = readInput
  private val cubes = parseCubes
  private val cubeGrid = generateGrid

  task()

  case class Cube(x: Int, y: Int, z: Int):
    private var visible = 6

    def sidesVisible = visible

    def hideSide(amount: Int = 1) =
      if visible > 0 then
        visible -= amount

    override def toString: String = s"$visible"

  end Cube


  class Grid(val maxX: Int, val maxY: Int, val maxZ: Int):
    val grid = Array.fill[Option[Cube]](maxX+1,maxY+1,maxZ+1)(None)
    private val wayOut = Buffer[(Int,Int,Int)]()
    private val noWayOut = Buffer[(Int,Int,Int)]()

    def addCube(cube: Cube) =
      grid(cube.x)(cube.y)(cube.z) = Some(cube)
      val others = neighbors(cube.x, cube.y, cube.z)
      cube.hideSide(others.length)
      others.foreach(_.hideSide())

    def confirmEdges() =
      for
        newX <- 1 to maxX
        newY <- 1 to maxY
        newZ <- 1 to maxZ
      do
        val cell = grid(newX)(newY)(newZ)
        if cell.isDefined then
          val cube = cell.get
          val sides = cube.sidesVisible
          val empty = 6 - neighbors(newX, newY, newZ).length
          if sides != empty then
            println((newX, newY, newZ, sides, empty))

    def printEmpty() =
      for
        newX <- 1 to maxX
        newY <- 1 to maxY
        newZ <- 1 to maxZ
      do
        val cell = grid(newX)(newY)(newZ)
        if cell.isEmpty then
          println((newX, newY, newZ))

    private def addBorders() =
      for
        x <- 0 to maxX
        y <- 0 to maxY
        z <- 0 to maxZ
      do
        if x == 0 || y == 0 || z == 0 || x == maxX || y == maxY || z == maxZ then
          if grid(x)(y)(z).isEmpty then
            addCube(Cube(x,y,z))

    private def neighbors(x: Int, y: Int, z: Int) =
      val result = Buffer[Cube]()
      if x > 0 && grid(x-1)(y)(z).nonEmpty then
        result += grid(x-1)(y)(z).get
      if x < maxX && grid(x+1)(y)(z).nonEmpty then
        result += grid(x+1)(y)(z).get
      if y > 0 && grid(x)(y-1)(z).nonEmpty then
        result += grid(x)(y-1)(z).get
      if y < maxY && grid(x)(y+1)(z).nonEmpty then
        result += grid(x)(y+1)(z).get
      if z > 0 && grid(x)(y)(z-1).nonEmpty then
        result += grid(x)(y)(z-1).get
      if z < maxZ && grid(x)(y)(z+1).nonEmpty then
        result += grid(x)(y)(z+1).get
      result.toVector

    private def emptyNeighborCoords(x: Int, y: Int, z: Int) =
      val result = Buffer[(Int, Int, Int)]()
      if x > 0 && grid(x-1)(y)(z).isEmpty then
        result += ((x-1, y, z))
      if x < maxX && grid(x+1)(y)(z).isEmpty then
        result += ((x+1, y, z))
      if y > 0 && grid(x)(y-1)(z).isEmpty then
        result += ((x, y-1, z))
      if y < maxY && grid(x)(y+1)(z).isEmpty then
        result += ((x, y+1, z))
      if z > 0 && grid(x)(y)(z-1).isEmpty then
        result += ((x, y, z-1))
      if z < maxZ && grid(x)(y)(z+1).isEmpty then
        result += ((x, y, z+1))
      result.toVector

    def totalVisible: Int =
      val flatList = grid.flatten.flatten.filter(_.isDefined)
      flatList.map(_.get.sidesVisible).sum

    private def reverseCubeVisible =
      val (gX, gY, gZ) = (maxX + 2, maxY + 2, maxZ + 2)
      val newGrid = Grid(gX, gY, gZ)
      val outer = 2 * (gX + 1) * (gY + 1) + 2 * (gX + 1) * (gZ + 1) + 2 * (gY + 1) * (gZ + 1)
      val visited = Buffer[(Int, Int, Int)]()
      val empty = newGrid.grid.flatten.flatten.count(_.isEmpty)

      def recursion(x: Int, y: Int, z: Int): Unit =
        var next = emptyNeighborCoords(x, y, z)
        if grid(maxX - x)(maxY - y)(maxZ - z).isEmpty then
          next ++= emptyNeighborCoords(maxX - x, maxY - y, maxZ - z)
        val possible = next.filterNot(visited.contains(_))
        if possible.nonEmpty then
          visited ++= possible
          for coords <- possible do
            val (newX, newY, newZ) = coords
            newGrid.addCube(Cube(newX + 1, newY + 1, newZ + 1))
            recursion(newX, newY, newZ)

      newGrid.addCube(Cube(1,1,1))
      newGrid.addCube(Cube(maxX + 1, maxY + 1, maxZ + 1))
      recursion(0,0,0)
      newGrid.addBorders()
      //newGrid.printEmpty()
      newGrid.totalVisible - outer

    def reachableSurface: Int =
      reverseCubeVisible

  end Grid


  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day18_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseCubes =
    val result = Buffer[Cube]()
    for line <- lines do
      val numbers = line.split(",").map(_.toInt).take(3)
      result += Cube(numbers(0), numbers(1), numbers(2))
    result.toVector

  private def generateGrid =
    val maxX = cubes.maxBy(_.x).x
    val maxY = cubes.maxBy(_.y).y
    val maxZ = cubes.maxBy(_.z).z
    val grid = Grid(maxX, maxY, maxZ)
    for cube <- cubes do
      grid.addCube(cube)
    grid

  def task() =
    println(cubeGrid.totalVisible)
    println(cubeGrid.reachableSurface)

