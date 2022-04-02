import scala.collection.mutable.ListBuffer
import scala.io.Source

object main extends App {
//    val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
    .map(_.map(_.toString.toInt)).toArray

  var riskLevels = 0
  var lowPoints = ListBuffer[(Int, Int)]()

  for (i <- input.indices) {
    for (j <- input(i).indices) {
      if (checkSurrounded(input, i ,j)){
        riskLevels += 1 + input(i)(j)
        lowPoints += ((i, j))
      }
    }
  }

  println(s"Part 1: $riskLevels")

  val part2Input = input.map(_.toArray).map(_.map((_, false)))
  var basinSizes = ListBuffer[Int]()

  for ((i, j) <- lowPoints) {
    basinSizes += exploreBasin(part2Input, i, j)
  }

  println(basinSizes)
  println(basinSizes.sortBy(- _))
  println(basinSizes.sortBy(- _).take(3))

  var total = 1
  basinSizes.sortBy(- _).take(3).foreach(total *= _)

  println(s"Part 2: $total")


  def exploreBasin(grid: Array[Array[(Int, Boolean)]], i: Int, j: Int): Int = {
    grid(i)(j) = (grid(i)(j)._1 , true)
    var basinSize = 1
    for ((a, b) <- neighbours(grid, i, j)) {
      if (!grid(a)(b)._2 && grid(a)(b)._1 != 9) {
        basinSize += exploreBasin(grid, a, b)
      }
    }
    basinSize
  }

  def neighbours(grid: Array[Array[(Int, Boolean)]], i: Int, j: Int): List[(Int, Int)] = {
    var neighbours = ListBuffer[(Int, Int)]()
    try {
      grid(i-1)(j)
      neighbours += ((i-1, j))
    } catch {case _: ArrayIndexOutOfBoundsException =>}
    try {
      grid(i)(j-1)
      neighbours += ((i, j-1))
    } catch {case _: ArrayIndexOutOfBoundsException =>}
    try {
      grid(i+1)(j)
      neighbours += ((i+1, j))
    } catch {case _: ArrayIndexOutOfBoundsException =>}
    try {
      grid(i)(j+1)
      neighbours += ((i, j+1))
    } catch {case _: ArrayIndexOutOfBoundsException =>}
    neighbours.toList
  }

  def checkSurrounded(grid: Array[IndexedSeq[Int]], i: Int, j: Int): Boolean = {
    var up = false
    var down = false
    var left = false
    var right = false

    try
      if (grid(i-1)(j) > grid(i)(j)){
        left = true
      }
    catch {
      case _: IndexOutOfBoundsException => left = true
    }

    try
      if (grid(i)(j-1) > grid(i)(j)){
        up = true
      }
    catch {
      case _: IndexOutOfBoundsException => up = true
    }

    try
      if (grid(i+1)(j) > grid(i)(j)){
        right = true
      }
    catch {
      case _: IndexOutOfBoundsException => right = true
    }

    try
      if (grid(i)(j+1) > grid(i)(j)){
        down = true
      }
    catch {
      case _: IndexOutOfBoundsException => down = true
    }

    up && down && left && right
  }
}
