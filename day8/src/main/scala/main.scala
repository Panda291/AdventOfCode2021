import scala.collection.mutable
import scala.io.Source

object main extends App {
//  val filename = "src/resources/test_input.txt"
      val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
    .toList
    .map(_.split(""" \| """)
      .map(_.split(" ")))

  var count = 0
  input.map(_.last
    .filter({ item =>
      List(2, 3, 4, 7).contains(item.length)
    }))
    .foreach(count += _.length)

  println(s"Part1: $count")

  val temp = input.map({ entry =>
    var digits = entry.head.map(_.sorted).toList
    val value = entry.last.toList
    var maskMap = mutable.Map[String, Int]()

    val one = digits.filter(_.length == 2).head
    val seven = digits.filter(_.length == 3).head
    val four = digits.filter(_.length == 4).head
    val eight = digits.filter(_.length == 7).head

    digits = digits.filter({digit =>
      !List(2,3,4,7).contains(digit.length)
    })

    val nine = digits.filter({digit =>
      numberInNumber(digit, four)
    }).head
    digits = digits.filter(_ != nine)

    val zero = digits.filter({digit =>
      numberInNumber(digit, seven) && digit.length == 6
    }).head
    digits = digits.filter(_ != zero)

    val three = digits.filter({digit =>
      digit.length == 5 && numberInNumber(digit, seven)
    }).head
    digits = digits.filter(_ != three)

    val six = digits.filter(_.length == 6).head
    digits = digits.filter(_ != six)

    val five = digits.filter({digit =>
      numberInNumber(six, digit)
    }).head
    digits = digits.filter(_ != five)

    val two = digits.head

//    println(zero, one, two, three, four, five, six, seven, eight, nine)

    maskMap += (zero -> 0)
    maskMap += (one -> 1)
    maskMap += (two -> 2)
    maskMap += (three -> 3)
    maskMap += (four -> 4)
    maskMap += (five -> 5)
    maskMap += (six -> 6)
    maskMap += (seven -> 7)
    maskMap += (eight -> 8)
    maskMap += (nine -> 9)

    val maskedValue = value.map({elem =>
      maskMap(elem.sorted)
    }).mkString("").toInt
    maskedValue
  }).sum

  println(s"Part2: $temp")


  def numberInNumber(big: String, small: String): Boolean = {
    for (i <- small) {
      if (!big.contains(i)) return false
    }
    true
  }
}
