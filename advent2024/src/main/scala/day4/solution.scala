package day4
import scala.io.Source._

class solution(input: String):
  def matchLines(lines: List[String]): Int =
    lines.map(l => {
      val pattern = "XMAS".r
      val m1 = pattern.findAllIn(l)
      val m2 = pattern.findAllIn(l.reverse)
      m1.size + m2.size
    }).sum

  def matchXMASGrid(lines: List[String]): Int =
    (0 to lines.head.size - 4).iterator.map(i => {
      (0 to lines.size - 4).map(j =>  {
        var r = 0
        if (lines(j)(i) == 'X' && lines(j+1)(i+1) == 'M' && lines(j+2)(i+2) == 'A' && lines(j+3)(i+3) == 'S')
          r = r + 1
        if (lines(j)(i) == 'S' && lines(j + 1)(i + 1) == 'A' && lines(j + 2)(i + 2) == 'M' && lines(j + 3)(i + 3) == 'X')
          r = r + 1
        if (lines(j)(i+3) == 'X' && lines(j + 1)(i + 2) == 'M' && lines(j + 2)(i + 1) == 'A' && lines(j + 3)(i) == 'S')
          r = r + 1
        if (lines(j)(i+3) == 'S' && lines(j + 1)(i + 2) == 'A' && lines(j + 2)(i + 1) == 'M' && lines(j + 3)(i) == 'X')
          r = r + 1
        r
      }).sum
    }).sum

  def matchMASGrid(lines: List[String]): Int =
    (0 to lines.head.size - 3).iterator.map(i => {
      (0 to lines.size - 3).map(j => {
        val b1 = (lines(j)(i) == 'M' && lines(j + 1)(i + 1) == 'A' && lines(j + 2)(i + 2) == 'S')
           || (lines(j)(i) == 'S' && lines(j + 1)(i + 1) == 'A' && lines(j + 2)(i + 2) == 'M')
        val b2 = (lines(j)(i+2) == 'M' && lines(j + 1)(i + 1) == 'A' && lines(j + 2)(i) == 'S')
          || (lines(j)(i+2) == 'S' && lines(j+ 1)(i + 1) == 'A' && lines(j + 2)(i) == 'M')
        if(b1 && b2) 1 else 0
      }).sum
    }).sum

  def prob1(): Int =
    val lines = fromFile(input).getLines.toList
    matchLines(lines) + matchLines(lines.transpose.map(l => l.mkString)) + matchXMASGrid(lines)

  def prob2(): Int =
    val lines = fromFile(input).getLines.toList
    matchMASGrid(lines)

  def run(): Unit =
    println("===== Day 4 =====")
    print("Puzzle 1: ")
    println(prob1())

    print("Puzzle 2: ")
    println(prob2())

