import scala.io.Source

class Day8:

  private var lines = readInput()
  private val letterGrid = generateArray
  private val transposed = letterGrid.transpose
  private val gridSize = lines.length
  private val visible = Array.ofDim[Boolean](gridSize, gridSize)
  private val scores = Array.ofDim[Int](gridSize, gridSize)

  private def readInput() =
    var file = Source.fromFile("day8_input")
    val result = file.getLines().toVector
    file.close()
    result

  def checkVisibility() =
    for line <- letterGrid.indices do
      for column <- letterGrid(line).indices do
        val value = letterGrid(line)(column)
        if line == 0 || column == 0 || line == gridSize-1 || column == gridSize-1 then
          visible(line)(column) = true
        else if letterGrid(line).take(column).max < value || letterGrid(line).takeRight(letterGrid(line).length - column - 1).max < value then
          visible(line)(column) = true
        else if transposed(column).take(line).max < value || transposed(column).takeRight(transposed(column).length - line - 1).max < value then
          visible(line)(column) = true

  def checkScores() =
    for line <- letterGrid.indices do
      for column <- letterGrid(line).indices do
        scores(line)(column) = scenicScore(line, column)


  private def generateArray =
    val letters = Array.ofDim[Int](lines.length, lines.length)
    for line <- lines.indices do
      for column <- lines(line).indices do
        letters(line)(column) = lines(line)(column).asDigit
    letters

  private def scenicScore(line: Int, column: Int) =
    val value = letterGrid(line)(column)

    def score(direction: Array[Int]): Int =
      var result = 0
      var blocked = false
      for i <- direction.indices do
        val currentValue = direction(i)
        val previous = direction.take(i+1).max
        if i == 0 then // First tree
          result += 1
        else if previous <= currentValue && !blocked then
          if !blocked then
            result += 1
          else if currentValue == value then
            result += 1
        // Blocked at first same height tree
        if currentValue >= value then
          blocked = true
      result

    def score1(direction: Array[Int]): Int =
      val matches = direction.takeWhile(_ < value).length
      if matches < direction.length then
        matches + 1
      else
        matches

    val left = letterGrid(line).take(column).reverse
    val right = letterGrid(line).takeRight(letterGrid(line).length - column - 1)
    val top = transposed(column).take(line).reverse
    val bottom = transposed(column).takeRight(transposed(column).length - line - 1)
    var total = 1
    for dir <- Vector(left, right, top, bottom) do
      total *= score1(dir)
    total


  def howManyVisible: Int = visible.flatten.count(_ == true)

  def maxScore: Int = scores.flatten.max

  def debugInfo() =
    for line <- scores do
      println(line.mkString(","))



@main def day8_start() =
  val day = Day8()
  day.checkVisibility()
  day.checkScores()
  println(day.howManyVisible)
  println(day.maxScore)
  //day.debugInfo()