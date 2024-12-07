package day7

import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.math.BigInt
import scala.math.pow


class solution(input: String):

  def check1(target: Long, numbers: List[Long]): Boolean =
    var result = false
    val numCombinations = (1 << (numbers.length - 1)) - 1
    // print(s" numCombo: ${numCombinations} ")
    (0 to numCombinations).foreach(i => {
      if (!result) {
        val v = numbers.tail.foldLeft((numbers.head, 0))((acc, number) => {
          val p = numbers.length - acc._2 - 1
          val currOp = (i % (1<<p)) / (1 << (p-1))
          if (currOp == 0) {(acc._1 + number, acc._2 + 1)} else {(acc._1 * number, acc._2+1)}
        })
        if(v._1 == target) result = true
      }
    })
    result

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    lines.map(line => {
      val s1 = line.split(": ")
      val target = s1.head.toLong
      val numbers = s1(1).split(" +").map(s => s.toLong).toList
      val c = check1(target, numbers)
      //println(s"$target, ${numbers.mkString("", ",", "")} ${c}")
      if(c) target else 0
    }).sum

  def check2(target: Long, numbers: List[Long]): Boolean =
    var result = false
    val numCombinations = pow(3, numbers.length - 1).toInt - 1
    // print(s" numCombo: ${numCombinations} ")
    (0 to numCombinations).foreach(i => {
      if (!result) {
        val v = numbers.tail.foldLeft((numbers.head, 0))((acc, number) => {
          val p = numbers.length - acc._2 - 1
          val currOp = (i % pow(3, p).toInt) / pow(3, p - 1).toInt
          if (currOp == 0) {
            (acc._1 + number, acc._2 + 1)
          } else if (currOp == 1){
            (acc._1 * number, acc._2 + 1)
          } else {
            (acc._1 * pow(10, number.toString.length).toInt + number, acc._2 + 1)
          }
        })
        if (v._1 == target) result = true
      }
    })
    result

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    lines.map(line => {
      val s1 = line.split(": ")
      val target = s1.head.toLong
      val numbers = s1(1).split(" +").map(s => s.toLong).toList
      val c = check2(target, numbers)
      //println(s"$target, ${numbers.mkString("", ",", "")} ${c}")
      if (c) {
        target
      } else 0
    }).sum

  def run(): Unit =
    println("===== Day 7 =====")
    print("Puzzle 1: ")
    println(prob1())

    print("Puzzle 2: ")
    val t0 = System.nanoTime()
    println(prob2())
    val t1 = System.nanoTime()
    val elapsed = {
      (t1 - t0) / 1.0e6
    }
    println(s"Part2 Elapsed time: $elapsed ms")




