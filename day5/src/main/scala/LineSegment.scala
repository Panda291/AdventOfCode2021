case class LineSegment(descriptor: String) {
  val points: Array[String] = descriptor.split(" -> ")
  val x1 :: y1 :: _ = points(0).split(",").map(_.toInt).toList
  val x2 :: y2 :: _ = points(1).split(",").map(_.toInt).toList

  def cardinalLine: Boolean = {
    x1 == x2 || y1 == y2
  }

  def getLine: List[(Int, Int)] = {
    if (x1 == x2) {
      if (y1 < y2) {
        (y1 to y2).toList.map((x1, _))
      } else (y2 to y1).toList.map((x1, _))
    } else if (y1 == y2) {
      if (x1 < x2) {
        (x1 to x2).toList.map((_, y1))
      } else (x2 to x1).toList.map((_, y1))
    } else {
      if (x1 < x2 && y1 < y2) {
        (x1 to x2).toList zip (y1 to y2).toList
      } else if (x1 < x2 && y1 > y2) {
        (x1 to x2).toList zip (y2 to y1).toList.reverse
      } else if (x1 > x2 && y1 < y2) {
        (x2 to x1).toList.reverse zip (y1 to y2).toList
      } else if (x1 > x2 && y1 > y2) {
        (x2 to x1).toList.reverse zip (y2 to y1).toList.reverse
      } else {
        List()
      }
    }
  }
}
