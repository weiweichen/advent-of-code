package day3

import scala.io.Source._

class solution(input: String):
  def getMul(str: String): Int =
    str.drop(4).dropRight(1).split(",").map(w => w.toInt).product

  def prob1(): Int =
    val lines = fromFile(input).getLines.toList
    lines.map(l => {
      val pattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)".r
      val mi = pattern.findAllIn(l)
      val r = mi.map(str => {
        getMul(str)
      }).toList
      r.sum
    }).sum

  def prob2(): Int =
    val lines = fromFile(input).getLines.toList
    lines.flatMap(l => {
      val pattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|don't\\(\\)|do\\(\\)".r
      val mi = pattern.findAllIn(l)
      mi.toList
    }).foldLeft((0, true))((acc, curr) => {
      curr match  {
        case "don't()" => (acc._1, false)
        case "do()" => (acc._1, true)
        case _ =>
          if(acc._2) (acc._1 + getMul(curr), acc._2)
          else (acc._1, acc._2)
      }
    })._1
    
  def run(): Unit =
    print("Puzzle 1: ")
    println(prob1())

    print("Puzzle 2: ")
    println(prob2())
