import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object Main extends App{
//        val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
    .map(_.map(_.toString.toInt))   // every number to int
    .map(_.toArray)                 // every row to array
    .toArray                        // all rows as array


  var totalFlashes: Long = 0
  var changedGrid: ArrayBuffer[ArrayBuffer[(Int, Boolean)]] = input.map(_.map((_, false)).to(ArrayBuffer)).to(ArrayBuffer) // add booleans to each field
  var temp: Long = 0

//  val test: ArrayBuffer[ArrayBuffer[(Int, Boolean)]] = ArrayBuffer(
//    ArrayBuffer((0, false),(0, false),(0, false)),
//    ArrayBuffer((0, false),(0, false),(0, false)),
//    ArrayBuffer((0, false),(0, false),(0, false))
//  )
//
//  println(test.forall(_.forall(_._1 == 0)))

  breakable {
    for (i <- 0 until Double.PositiveInfinity.toInt) {
      temp = flashGrid(changedGrid)
      if (i < 100) {
        totalFlashes += temp
      }
      if (i == 100) {
        println(f"Part 1: $totalFlashes")
      }
      if (changedGrid.forall(_.forall(_._1 == 0))) {
        println(f"Part 2: ${i + 1}")
        break
      }
    }
  }

  def flashGrid(grid: ArrayBuffer[ArrayBuffer[(Int, Boolean)]]): Long = {
    var total: Long = 0
    for (i <- grid.indices) {
      for (j <- grid(i).indices) {
        total += processField(grid, i, j)
      }
    }

    for (i <- grid.indices) { // return all fields to false
      for (j <- grid(i).indices) {
        grid(i)(j) = (grid(i)(j)._1, false)
      }
    }
    total
  }

  def processField(grid: ArrayBuffer[ArrayBuffer[(Int, Boolean)]], i: Int, j: Int): Long = {
    var flashes: Long = 0
    if (!grid(i)(j)._2) {
      grid(i)(j) = (grid(i)(j)._1 + 1, grid(i)(j)._2)
      if (grid(i)(j)._1 > 9) {
        grid(i)(j) = (0, true)
        flashes += 1
        for (n <- neighbours(grid, i, j)) {
          flashes += processField(grid, n._1, n._2)
        }
      }
    }
    flashes
  }

  def neighbours(grid: ArrayBuffer[ArrayBuffer[(Int, Boolean)]], i: Int, j: Int): List[(Int, Int)] = {
    val neighbours = ListBuffer[(Int, Int)]()
    try {
      grid(i-1)(j)
      neighbours += ((i-1, j))
    } catch {case _: IndexOutOfBoundsException =>}
    try {
      grid(i)(j-1)
      neighbours += ((i, j-1))
    } catch {case _: IndexOutOfBoundsException =>}
    try {
      grid(i+1)(j)
      neighbours += ((i+1, j))
    } catch {case _: IndexOutOfBoundsException =>}
    try {
      grid(i)(j+1)
      neighbours += ((i, j+1))
    } catch {case _: IndexOutOfBoundsException =>}
    try {
      grid(i-1)(j-1)
      neighbours += ((i-1, j-1))
    } catch {case _: IndexOutOfBoundsException =>}
    try {
      grid(i-1)(j+1)
      neighbours += ((i-1, j+1))
    } catch {case _: IndexOutOfBoundsException =>}
    try {
      grid(i+1)(j-1)
      neighbours += ((i+1, j-1))
    } catch {case _: IndexOutOfBoundsException =>}
    try {
      grid(i+1)(j+1)
      neighbours += ((i+1, j+1))
    } catch {case _: IndexOutOfBoundsException =>}


    neighbours.toList
  }
}
