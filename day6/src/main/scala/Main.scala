import scala.io.Source

object Main extends App {
//  val filename = "src/resources/test_input.txt"
    val filename = "src/resources/input.txt"
  Source.fromFile(filename).getLines()
    .toList
    .head
    .split(",")
    .map(_.toInt)
    .foreach({ fish: Int => {
      LanternFish.addFish(fish)
      LanternFishMap.addFish(fish)
    }})

  for (_ <- 1 to 80) {
    LanternFish.progressDay()
  }
  for(_ <- 1 to 256) {
    LanternFishMap.progressDay()
  }
  println(LanternFish.countFish())
  println(LanternFishMap.countFish())
}

