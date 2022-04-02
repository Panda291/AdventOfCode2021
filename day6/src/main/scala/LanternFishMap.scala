import scala.collection.mutable

object LanternFishMap {
  var fishMap: mutable.Map[Int, Long] = mutable.Map().withDefaultValue(0)

  def addFish(fish: Int): Unit = {
    fishMap(fish) += 1
  }

  def progressDay(): Unit = {
    val newFishMap: mutable.Map[Int, Long] = mutable.Map()
    for (i <- 0 until 8) {
      newFishMap(i) = fishMap(i + 1)
    }
    newFishMap(6) = newFishMap(6) + fishMap(0)
    newFishMap(8) = fishMap(0)
    fishMap = newFishMap
  }

  def countFish(): Long = {
    var count: Long = 0
    for ((_, v) <- fishMap) {
      count += v
    }
    count
  }
}
