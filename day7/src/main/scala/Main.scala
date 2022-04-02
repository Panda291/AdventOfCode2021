import scala.Double.PositiveInfinity
import scala.io.Source

object Main extends App {
//  val filename = "src/resources/test_input.txt"
    val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
    .toList
    .head
    .split(",")
    .map(_.toInt)
    .toList


  var smallest: Long = PositiveInfinity.toLong
  for (i <- input.min to input.max) { // O(n²)
    var sum = 0
    for (el <- input) {
      sum += increasingSum((el - i).abs) // remove increasingSum to get part 1
    }
    if (sum < smallest) {
      smallest = sum
    }
  }
  println(smallest)

  def increasingSum(n: Int): Int = {

    var f = 0
    for(i <- 1 to n)
    {
      f = f + i;
    }

    f
  }
}
