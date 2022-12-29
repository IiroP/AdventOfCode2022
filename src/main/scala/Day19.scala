import scala.collection.mutable.{Buffer, Map}
import scala.io.Source
import scala.math.{max, min}

object Day19 extends App:

  private val lines = readInput
  private val blueprints = parseBlueprints

  task1()
  task2()

  case class Blueprint(num: Int, ore: Int, clay: Int, obsidian: (Int, Int), geode: (Int, Int)):

    def maxOreCost: Int = Vector(ore, clay, obsidian(0), geode(0)).max

    def maxOreSecondary: Int = Vector(ore, clay, obsidian(0)).max

    def qualityLevel: Int = num * simulate(this, 24)

    def maxGeodesInTime(minutes: Int) = simulate(this, minutes)


  case class State(elapsed: Int, ore: Int, clay: Int, obsidian: Int, geode: Int, oreProd: Int, clayProd: Int, obsidianProd: Int, geodeProd: Int):

    def asTuple = (elapsed, ore, clay, obsidian, geode, oreProd, clayProd, obsidianProd, geodeProd)

  private def readInput: Vector[String] =
    var file = Source.fromFile("input/day19_input")
    val result = file.getLines().toVector
    file.close()
    result

  private def parseBlueprints: Vector[Blueprint] =
    val result = Buffer[Blueprint]()
    val pattern = raw"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.".r
    for line <- lines do
      line match
        case pattern(num, ore, clay, obsidian1, obsidian2, geode1, geode2) =>
          val blueprint = Blueprint(num.toInt, ore.toInt, clay.toInt, (obsidian1.toInt, obsidian2.toInt), (geode1.toInt, geode2.toInt))
          result += blueprint
    result.toVector

  def simulate(blueprint: Blueprint, time: Int): Int =

    //State(elapsed, ore, clay, obsidian, geode, oreProd, clayProd, obsidianProd, geodeProd)
    val start = State(0, 0, 0, 0, 0, 1, 0, 0, 0)

    def bestGeoide: Int =
      val maxGeoidesAfterMinute = Array.fill(time + 1)(0)
      def recursion(state: State): Int =
        if maxGeoidesAfterMinute(state.elapsed) < state.geode then
          maxGeoidesAfterMinute(state.elapsed) = state.geode

        if state.elapsed == time then // time's up
          state.geode
        else if maxGeoidesAfterMinute(state.elapsed) > state.geode then // not enough geoides, ignore this path
          state.geode
        else if state.oreProd > blueprint.maxOreCost || state.clayProd > blueprint.obsidian(1) || state.obsidianProd > blueprint.geode(1) then // unnecessary production
          state.geode
        else
          val robots = possibleBuilds(state)
          val newStates = robots.map( updateState(state, _) )
          newStates.map( recursion(_) ).max
      recursion(start)

    def possibleBuilds(state: State): Vector[Option[String]] =
      var possible = Buffer[Option[String]]()
      if state.ore >= blueprint.ore then
        possible += Some("ore")
      if state.ore >= blueprint.clay then
        possible += Some("clay")
      if state.ore >= blueprint.obsidian(0) && state.clay >= blueprint.obsidian(1) then
        possible += Some("obsidian")
      if state.ore >= blueprint.geode(0) && state.obsidian >= blueprint.geode(1) then
        possible = Buffer(Some("geode"))
      else if blueprint.maxOreSecondary > (state.ore + state.oreProd) / 2 then
        possible += None
      possible.toVector

    def updateState(state: State, robotToBuild: Option[String] = None): State =
      var (elapsed, ore, clay, obsidian, geode, oreProd, clayProd, obsidianProd, geodeProd) = state.asTuple
      // Resources can only be incremented after build has started, but before the robot is ready
      val oreAdd = oreProd
      val clayAdd = clayProd
      val obsidianAdd = obsidianProd
      val geodeAdd = geodeProd

      def buildWithState(robot: String) =
        if robot == "ore" then
          val cost = blueprint.ore
          if cost <= ore then
            ore -= cost
            oreProd += 1
        else if robot == "clay" then
          val cost = blueprint.clay
          if cost <= ore then
            ore -= cost
            clayProd += 1
        else if robot == "obsidian" then
          val (oreCost, clayCost) = blueprint.obsidian
          if oreCost <= ore && clayCost <= clay then
            ore -= oreCost
            clay -= clayCost
            obsidianProd += 1
        else if robot == "geode" then
          val (oreCost, obsidianCost) = blueprint.geode
          if oreCost <= ore && obsidianCost <= obsidian then
            ore -= oreCost
            obsidian -= obsidianCost
            geodeProd += 1

      if robotToBuild.isDefined then
        buildWithState(robotToBuild.get)

      ore += oreAdd
      clay += clayAdd
      obsidian += obsidianAdd
      geode += geodeAdd
      elapsed += 1

      State(elapsed, ore, clay, obsidian, geode, oreProd, clayProd, obsidianProd, geodeProd)


    bestGeoide

  def task1() =
    val combinedQuality = blueprints.map(_.qualityLevel).sum
    println(combinedQuality)

  def task2() =
    val largestNumberOfGeodes = blueprints.take(3).map(_.maxGeodesInTime(32))
    println(largestNumberOfGeodes.product)