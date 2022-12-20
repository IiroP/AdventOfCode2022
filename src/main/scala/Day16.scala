import scala.collection.mutable.{Buffer, Map}
import scala.io.Source
import scala.math.max

object Day16 extends App:

  private val lines = readInput
  private val valves = parseValves
  private val start = "AA"

  task1()

  private def readInput =
    var file = Source.fromFile("input/day16_input")
    val result = file.getLines().toVector
    file.close()
    result

  case class Valve(name: String, flow: Int, target: Vector[String]):

    private var opened = false
    private var openedAt = 0
    private var dijkstra_results = (Map[Valve, Int](), Map[Valve, Option[Valve]]())

   /* def isOpen = opened
    def whenOpened = openedAt

    def open(time: Int) =
      opened = true
      openedAt = time*/

    def nextValves = target.map( valves(_) )

    /*def releasedPressure(now: Int) =
      if isOpen then
        (now - openedAt) * flow
      else
        0*/

    def updateDistances() =
      dijkstra_results = dijkstra(this, valves.values.toVector)

    def shortestRoute(other: Valve) =
      val (dist, prev) = dijkstra_results
      val path = Buffer[Valve]()
      var u: Option[Valve] = Some(other)
      if prev(u.get).isDefined || u.get == other then
        while u.isDefined do
          path.prepend(u.get)
          u = prev(u.get)
      path.length - 1

/*    def reset() =
      opened = false
      openedAt = 0*/

  end Valve


  private def parseValves: Map[String, Valve] =
    val result = Map[String, Valve]()
    val pattern = raw"Valve (\w+) has flow rate=(\d+); .* valves? (.*)".r
    for line <- lines do
      line match
        case pattern(name, flow, targets) =>
          val target = targets.split(",").map(_.strip()).toVector
          val valve = Valve(name, flow.toInt, target)
          result(name) = valve
    result

  // From https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
  private def dijkstra(source: Valve, graph: Vector[Valve]) =
    val dist = Map[Valve, Int]() //shortest distance to each node
    val prev = Map[Valve, Option[Valve]]() //previous step on the way to each node
    var remaining = Buffer[Valve]()

    for point <- graph do
      dist(point) = 100000 // "infinity"
      prev(point) = None
      remaining += point
    dist(source) = 0

    while remaining.nonEmpty do
      val u = remaining.minBy( dist(_) )
      remaining -= u

      for v <- u.nextValves.filter( remaining.contains(_) ) do
        val alt = dist(u) + 1
        if alt < dist(v) then
          dist(v) = alt
          prev(v) = Some(u)

    (dist, prev)


  private def task1() =
    val timeLimit = 30
    val current = valves(start)
    val toBeOpened = valves.values.filter(_.flow > 0).toVector
    var bestScore = 0
    var bestPath = Vector[Valve]()
    valves.values.foreach(_.updateDistances())

    // Inspired by https://github.com/deivi-drg/advent-of-code-2022/blob/b25e60bc5722ec178d36dccac82e8ed983fe29ce/Day16/day16.py
    def nextMove(now: Valve, elapsed: Int, currentFlow: Int, alreadyFlown: Int, remaining: Vector[Valve], path: Vector[Valve]): Unit =
      val timeLeft = timeLimit - elapsed
      val trulyRemaining = remaining.filter( now.shortestRoute(_) < timeLeft )
      if trulyRemaining.isEmpty then
        val score = alreadyFlown + timeLeft * currentFlow
        if score > bestScore then
          bestScore = score
          bestPath = path
      else
        for point <- trulyRemaining do
          val distance = now.shortestRoute(point)
          val flownAfterOpen = alreadyFlown + (distance + 1) * currentFlow
          val timeAfterOpen = elapsed + distance + 1
          val newFlow = currentFlow + point.flow
          nextMove(point, timeAfterOpen, newFlow, flownAfterOpen, remaining.filterNot(_ == point), path.appended(point))

    nextMove(current, 0, 0, 0, toBeOpened, Vector(current))
    println(bestScore)
