import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object Main extends App {
//      val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  var input = Source.fromFile(filename).getLines()
    .map(_.map(_.toString.toInt).toArray).toArray

  input = expandField(input)

  //  var graph = EdgeWeightedDigraph()
  //
  //  for (i <- input.indices) {
  //    for (j <- input(i).indices) {
  //      for ((neighbourI, neighbourJ) <- getNeighbours(input, i, j)) {
  //        graph = graph.addEdge(DirectedEdge((i,j), (neighbourI, neighbourJ), input(neighbourI)(neighbourJ)))
  //      }
  //    }
  //  }

//  printGrid(expandField(input))

  val unvisitedNodes: mutable.Set[(Int, Int)] = mutable.Set()
  for (i <- input.indices) {
    for (j <- input(i).indices) {
      unvisitedNodes += ((i, j))
    }
  }
  val distance: mutable.Map[(Int, Int), Double] = mutable.Map().withDefaultValue(Double.PositiveInfinity)
  distance((0, 0)) = 0

  var current = (0, 0)

  breakable {
    while (true) {
      for (n <- getNeighbours(input, current._1, current._2) if unvisitedNodes.contains(n)) {
        val throughDistance = distance(current) + input(n._1)(n._2)
        if (distance(n) > throughDistance) distance(n) = throughDistance
      }
      //      debugPrinter(input, distance.toMap.withDefaultValue(Double.PositiveInfinity))
      unvisitedNodes.remove(current)
      if (unvisitedNodes.isEmpty) break
      current = distance.keys
        .filter(unvisitedNodes.contains)
        .minBy(distance(_))
      if (current == (input.length, input(0).length)) break
    }
  }

  println(distance(input.length - 1, input(0).length - 1).toInt)

  def getNeighbours(array: Array[Array[Int]], i: Int, j: Int): List[(Int, Int)] = {
    val neighbours: ListBuffer[(Int, Int)] = ListBuffer()
    if (i > 0) neighbours += ((i - 1, j))
    if (j > 0) neighbours += ((i, j - 1))
    if (i < array.length - 1) neighbours += ((i + 1, j))
    if (j < array(0).length - 1) neighbours += ((i, j + 1))
    neighbours.toList
  }

  def debugPrinter(array: Array[Array[Int]], distance: Map[(Int, Int), Double]): Unit = {
    for (i <- array.indices) {
      for (j <- array(i).indices) {
        if (distance((i, j)) != Double.PositiveInfinity) print(s"${distance((i, j))} ")
      }
      print("\n")
    }
    println("\n\n")
  }

  def expandField(array: Array[Array[Int]]): Array[Array[Int]] = {
    val newField: ArrayBuffer[Array[Int]] = ArrayBuffer()
    for (i <- 0 until 5) {
      for (row <- array) {
        newField += shift(row, i)
      }
    }

    val newNewField: ArrayBuffer[Array[Int]] = ArrayBuffer()
    for (row <- newField) {
      var newRow: Array[Int] = Array()
      for (i <- 0 until 5) {
        newRow = newRow ++ shift(row, i)
      }
      newNewField += newRow
    }

    newNewField.toArray
  }

  def shift(array: Array[Int], amount: Int): Array[Int] = {
    array.map({ number =>
      var newNumber = number + amount
      while (newNumber > 9) {
        newNumber -= 9
      }
      newNumber
    })
  }

  def printGrid(array: Array[Array[Int]]): Unit = {
    for (i <- array.indices) {
      for (j <- array(i).indices) {
        print(array(i)(j))
      }
      print("\n")
    }
  }
}
