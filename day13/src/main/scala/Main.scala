import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App{
//  val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
  var dots = ListBuffer[(Int, Int)]()
  var folds = ListBuffer[(String, Int)]()

  implicit class RegexOps(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val pattern = "(\\d+),(\\d+)".r

  input.foreach({
    case r"(\d+)${x},(\d+)${y}" => dots += ((x.toInt, y.toInt))
    case r"fold along (\w)${axis}=(\d+)${value}" => folds += ((axis, value.toInt))
    case r" " => true case _ => false
  })

  val newDots = ListBuffer[(Int, Int)]()
  if (folds.head._1 == "x") {
    for (dot <- dots) {
      if (dot._1 < folds.head._2) newDots += dot
      else newDots += ((2*folds.head._2 - dot._1, dot._2))
    }
  } else {
    for (dot <-dots) {
      if (dot._2 < folds.head._2) newDots += dot
      else newDots += ((dot._1, 2*folds.head._2 - dot._2))
    }
  }

  println(f"Part 1: ${newDots.distinct.length}")

  folds.foreach({fold =>
    val newDots = ListBuffer[(Int, Int)]()
    if (fold._1 == "x") {
      for (dot <- dots) {
        if (dot._1 < fold._2) newDots += dot
        else newDots += ((2*fold._2 - dot._1, dot._2))
      }
    } else {
      for (dot <-dots) {
        if (dot._2 < fold._2) newDots += dot
        else newDots += ((dot._1, 2*fold._2 - dot._2))
      }
    }
    dots = newDots.distinct
  })

  val maxX: Int = dots.map(_._1).max
  val maxY: Int = dots.map(_._2).max

  println("Part 2:")
  for (j <- 0 to maxY) {
    for (i <- 0 to maxX) {
      if (dots.contains((i,j))) print('#')
      else print('.')
    }
    print('\n')
  }
}
