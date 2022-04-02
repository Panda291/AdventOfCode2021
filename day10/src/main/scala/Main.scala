import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object Main extends App {
//      val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()

  var characterStack: ListBuffer[Char] = ListBuffer()
  var illegalCharacters: ListBuffer[Char] = ListBuffer()
  var correctLineScores: ListBuffer[Long] = ListBuffer()
  for (line <- input) {
    characterStack = ListBuffer()
    breakable {
      for (char <- line) {
        char match {
          case '(' => characterStack.prepend('(')
          case '[' => characterStack.prepend('[')
          case '{' => characterStack.prepend('{')
          case '<' => characterStack.prepend('<')

          case ')' =>
            if (characterStack.head == '(') {
              characterStack -= characterStack.head
            } else {
              illegalCharacters += ')'
              break
            }
          case ']' =>
            if (characterStack.head == '[') {
              characterStack -= characterStack.head
            } else {
              illegalCharacters += ']'
              break
            }
          case '}' =>
            if (characterStack.head == '{') {
              characterStack -= characterStack.head
            } else {
              illegalCharacters += '}'
              break
            }
          case '>' =>
            if (characterStack.head == '<') {
              characterStack -= characterStack.head
            } else {
              illegalCharacters += '>'
              break
            }
        }
      }

      var correctScore: Long = 0
      for (char <- characterStack) {
        char match {
          case '(' => correctScore *= 5; correctScore += 1
          case '[' => correctScore *= 5; correctScore += 2
          case '{' => correctScore *= 5; correctScore += 3
          case '<' => correctScore *= 5; correctScore += 4
        }
      }
      correctLineScores += correctScore
    }
  }

  var score = 0
  for (char <- illegalCharacters) {
    char match {
      case ')' => score += 3
      case ']' => score += 57
      case '}' => score += 1197
      case '>' => score += 25137
    }
  }
  println(s"Part 1: $score")

  val middle = (correctLineScores.length / 2).ceil.toInt
  val part2Score = correctLineScores.sortWith(_ < _)(middle)

  println(s"Part 2: $part2Score")
}
