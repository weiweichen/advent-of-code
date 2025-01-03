package day1

import scala.io.Source._

class solution(input: String):
  def prob1(): Unit =
    val lines = fromFile(input).getLines
    val s = lines.map(line => line.split(" +").map(w => w.toInt))
    val (l, r) = s.flatMap(n => n).zipWithIndex.partition(_._2 % 2 == 0)
    val sortedLeft = l.map((value, index) => value).toList.sorted
    val sortedRight = r.map((value, index) => value).toList.sorted
    println("Puzzle 1: ")
    println(sortedLeft.zip(sortedRight).map((l, r) => (l - r).abs).sum)

  def prob2(): Unit =
    val lines = fromFile(input).getLines
    val s = lines.map(line => line.split(" +").map(w => w.toInt))
    val (l, r) = s.flatMap(n => n).zipWithIndex.partition(_._2 % 2 == 0)
    val left = l.map((value, index) => value).toList
    val histogramRight =
      r.map((value, index) => value).toList.groupBy(l => l).map((key, value) => (key, value.size)).toMap
    println("Puzzle 2: ")
    println(left.map(l => l * histogramRight.getOrElse(l, 0)).sum)

  def run(): Unit =
    println("===== Day 1 =====")
    val t0 = System.nanoTime()
    prob1()
    val t1 = System.nanoTime()
    println(s"Part1 Elapsed time: ${(t1 - t0) / 1.0e6}ms")
    
    val t2 = System.nanoTime()
    print("Puzzle 2: ")
    prob1()
    val t3 = System.nanoTime()
    println(s"Part2 Elapsed time: ${(t3 - t2) / 1.0e6}ms")
