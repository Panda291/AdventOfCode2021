import scala.collection.mutable
import scala.io.Source

object main extends App{
//  val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val Pattern = "".r
  var board: mutable.Map[(Int, Int), Int] = mutable.Map[(Int, Int), Int]().withDefaultValue(0)
  Source.fromFile(filename).getLines()
    .map(LineSegment)
    .filter(_.cardinalLine)
    .map(_.getLine)
    .foreach(_.map(board(_) += 1))
  println(board.count(_._2 >= 2))

  board = mutable.Map[(Int, Int), Int]().withDefaultValue(0)
  Source.fromFile(filename).getLines()
    .map(LineSegment)
    .map(_.getLine)
    .foreach(_.map(board(_) += 1))
  println(board.count(_._2 >= 2))
}
