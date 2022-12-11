import scala.io.Source

class Day4:

  private var lines = Vector[String]()

  def readInput() =
    var file = Source.fromFile("day4_input")
    lines = file.getLines().toVector
    file.close()

  def totalOverlap: Int =
    val pairs = lines.map( _.split("[,-]") )

    def overlap(pair: Array[String]): Boolean =
      if pair(0).toInt >= pair(2).toInt && pair(1).toInt <= pair(3).toInt then
        true
      else if pair(2).toInt >= pair(0).toInt && pair(3).toInt <= pair(1).toInt then
        true
      else
        false
    pairs.map( overlap(_) ).count( _ == true )

  def partialOverlap: Int =
    val pairs = lines.map( _.split("[,-]") )

    def overlap(pair: Array[String]): Boolean =
      val nums = pair.map(_.toInt)
      val first = Range(nums(0), nums(1) + 1)
      val second = Range(nums(2), nums(3) + 1)
      first.contains(nums(2)) || first.contains(nums(3)) || second.contains(nums(0)) || second.contains(nums(1))
    pairs.map( overlap(_) ).count( _ == true )


@main def day4_start() =
  val day = Day4()
  day.readInput()
  println(day.totalOverlap)
  println(day.partialOverlap)