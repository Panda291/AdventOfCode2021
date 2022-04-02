import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object LanternFish {
  var fishList: ArrayBuffer[Int] = ArrayBuffer()

  def addFish(lanternFish: Int): Unit = {
    fishList += lanternFish
  }

  //  def progressDay(): Unit = {
  //    var newFishList: List[Int] = List()
  //    for (fish <- fishList) {
  //      if (fish == 0) {
  //        newFishList = newFishList :+ 6
  //        newFishList = newFishList :+ 8
  //      } else {
  //        newFishList = newFishList :+ fish - 1
  //      }
  //    }
  //    fishList = newFishList
  //  }

  def progressDay(): Unit = {
    for (i <- fishList.indices) {
      if (fishList(i) == 0) {
        fishList(i) = 6
        fishList += 8
      } else {
        fishList(i) = fishList(i) - 1
      }
    }
  }

  def countFish(): Int = {
    fishList.length
  }
}
