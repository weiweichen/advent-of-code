package day11

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.*


class solution(input: String):
  def blink(value: Long, steps: Int) =
    var list = List(value)
    (1 to steps).foreach(step => {
      list = list.flatMap(s => {
        s match {
          case 0 => List(1)
          case _ =>
            val numDigits = math.floor(math.log10(s)).toInt
            if (numDigits % 2 == 1) {
              val base = math.pow(10, (numDigits + 1) / 2).toInt
              List(s / base, s % base)
            } else {
              List(s * 2024)
            }
        }
      })
    })
    list.groupBy(v => v).map((k, v) => (k, v.length))
 
  def calc(stones: List[Long], blinks: Int) : Long =
    var g = stones.groupBy(v => v).map((k, l) => (k, l.length.toLong))
    // println(s"\nstep 0: ${g.map((k, v) => s"($k, $v)").mkString("", " ", "")}")
    (1 to blinks).foreach(step => {
      g = g.map((k, v) => {
        blink(k, 1).map((nk, nv) => (nk, nv * v))
      }).flatten().groupBy((k, v) => k).map((k, m) => (k, m.map((_, v)=> v).sum.toLong))
      //println(s"\nstep $step: ${g.map((k, v) => s"($k, $v)").mkString("", " ", "")}")
    })
    g.map((k, l) => l).sum

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val stones = lines.head.split(" +").map(s => s.toLong).toList
    calc(stones, 25)

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val stones = lines.head.split(" +").map(s => s.toLong).toList
    calc(stones, 75)

  def run(): Unit =
    println("===== Day 11 =====")
    print("Puzzle 1: ")
    val t0 = System.nanoTime()
    println(prob1())
    val t1 = System.nanoTime()
    println(s"Part1 Elapsed time: ${(t1 - t0) / 1.0e6}ms")

    val t2 = System.nanoTime()
    print("Puzzle 2: ")
    println(prob2())
    val t3 = System.nanoTime()
    println(s"Part1 Elapsed time: ${(t3 - t2) / 1.0e6}ms")




