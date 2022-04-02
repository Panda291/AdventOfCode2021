import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App{
//          val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
    .map(_.split("-"))
    .map({
      case Array(a, b) => (a, b)
    })
    .toList

  var allNodes: List[String] = List()
  input.foreach({a =>
    allNodes = a._1 :: allNodes
    allNodes = a._2 :: allNodes
  })

  allNodes = allNodes.distinct
  val part1 = explorePaths(input, "start", List(), smallTwice = true)
  println(s"Part 1: $part1")
  val part2 = explorePaths(input, "start", List(), smallTwice = false)
  println(s"Part 2: $part2")

  def explorePaths(connections: List[(String, String)], currentNode: String, visitedNodes: List[String], smallTwice: Boolean): Long = {
    var localVisitedNodes = visitedNodes
    var totalRoutes: Long = 0
    if (currentNode == "end") return 1
    if (currentNode.toLowerCase() == currentNode) localVisitedNodes = currentNode :: localVisitedNodes

    val ngbr: ListBuffer[String] = ListBuffer()

    for (to <- allNodes) {
      if (connected(connections, currentNode, to) && !localVisitedNodes.contains(to)) totalRoutes += explorePaths(connections, to, localVisitedNodes, smallTwice)
      else if (connected(connections, currentNode, to) && localVisitedNodes.contains(to) && !smallTwice && to != "start") {
        totalRoutes += explorePaths(connections, to, localVisitedNodes, smallTwice = true)
      }
    }

    for (ngbr <- ngbr) {

    }
    totalRoutes
  }

  def connected(connections: List[(String, String)], a: String, b: String): Boolean = {
    connections.contains((a,b)) ||connections.contains((b,a))
  }
}
