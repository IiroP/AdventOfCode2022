import scala.io.Source
import scala.collection.mutable.{Buffer, Map}

class Day7:

  private val lines = readInput()
  private val root = Dir("root", 0, Map[String, Dir](), None)
  private val filesystem = 70000000
  private val required = 30000000

  class Dir(val name: String, var files: Int, val subdirs: Map[String, Dir], val parent: Option[Dir]):

    def totalSize: Int =
      if subdirs.isEmpty then
        files
      else
        subdirs.values.map(_.totalSize).sum + files

    def allSubdirs: Vector[Dir] =
      if subdirs.isEmpty then
        Vector[Dir]()
      else
        val below = subdirs.values.flatMap( _.allSubdirs ).toVector
        val result = below ++ this.subdirs.values.toVector
        result

    override def toString: String = s"$name: $totalSize"

  end Dir

  private def readInput(): Vector[String] =
    var file = Source.fromFile(s"day7_input")
    val lines = file.getLines().toVector
    file.close()
    lines

  def debugInfo() =
    println(root.files)
    println(root.totalSize)
    println(root.allSubdirs)

  def bigDirs: Int =
    val dirs = root.allSubdirs.filter(_.totalSize < 100000)
    dirs.map(_.totalSize).sum

  def smallestDirToDelete =
    val free = filesystem - root.totalSize
    val missing = required - free
    val chosen = root.allSubdirs.filter(_.totalSize >= missing).minBy(_.totalSize)
    chosen.totalSize

  def parseData() =
    var current = root
    for line <- lines do
      val parts = line.split(" ")
      val lineType = parts.head
      if lineType == "$" then
        val command = parts(1)
        if command == "cd" then // change directory
          val target = parts(2)
          if target == "/" then
            current = root
          else if target == ".." then
            current = current.parent.get
          else if current.subdirs.contains( target ) then
            current = current.subdirs( target )
          else
            val newDir = Dir(target, 0, Map[String, Dir](), Some(current))
            current.subdirs(target) = newDir
            current = newDir
        end if
      else
        if parts(0) == "dir" then
          val target = parts(1)
          if !current.subdirs.contains( target ) then
            val newDir = Dir(target, 0, Map[String, Dir](), Some(current))
            current.subdirs(target) = newDir
        else
          current.files += parts(0).toInt

@main def day7_start() =
  val day = Day7()
  day.parseData()
  day.debugInfo()
  println(day.bigDirs)
  println(day.smallestDirToDelete)