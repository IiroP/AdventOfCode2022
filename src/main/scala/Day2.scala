
import scala.io.Source

class Day2:

  private var lines = Vector[String]()

  def readInput() =
    var file = Source.fromFile("input/day2_input")
    lines = file.getLines().toVector
    file.close()

  private def points(opponent: Char, you: Char): Int =
    val points = Map('X' -> 1, 'Y' -> 2, 'Z' -> 3)
    val opp = Map('X' -> 'A', 'Y' -> 'B', 'Z' -> 'C')
    val wins = Map('X' -> 'C', 'Y' -> 'A', 'Z' -> 'B')
    var total = points(you)

    if opp(you) == opponent then
      total += 3
    else if wins(you) == opponent then
      total += 6
    total

  def totalPoints1 =
    lines.map(_.toVector).map( vec => points(vec(0), vec(2)) ).sum

  private def points2(opponent: Char, result: Char): Int =
    val points = Map('A' -> 1, 'B' -> 2, 'C' -> 3)
    val winCond = Map('X' -> 0, 'Y' -> 3, 'Z' -> 6)
    val opp = Map('X' -> 'A', 'Y' -> 'B', 'Z' -> 'C')
    val wins = Map('A' -> 'C', 'B' -> 'A', 'C' -> 'B')
    var total = winCond(result)

    if total == 0 then // lose
      val you = wins(opponent)
      total += points(you)
    else if total == 3 then // tie
      total += points(opponent)
    else
      val you = wins.map(_.swap)(opponent)
      total += points(you)
    total


  def totalPoints2 =
    lines.map(_.toVector).map( vec => points2(vec(0), vec(2)) ).sum

@main def start_day2() =
  val task1 = Day2()
  task1.readInput()
  println(task1.totalPoints1)
  println(task1.totalPoints2)
