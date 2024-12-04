package day2

import scala.io.Source._

class solution(input: String):
  def absFail(i: Int): Boolean =
    i.abs > 3 || i.abs < 1
    
  def isSafe(l: List[Int]): Boolean =
    val dist = l.sliding(2).map(p=> p(0) - p(1)).toList
    (dist.count(i => absFail(i)) == 0) && (dist.count(i => i < 0) == 0 || dist.count(i => i > 0) == 0)

  def prob1(): Int =
    val lines = fromFile(input).getLines
    val s = lines.map(line => line.split(" +").map(w => w.toInt).toList)
    s.count(l => isSafe(l))

  def dropElem(l: List[Int], i: Int): List[Int] =
    l.take(i) ++ l.drop(i + 1)

  def prob2(): Int =
    val lines = fromFile(input).getLines
    val s = lines.map(line => line.split(" +").map(w => w.toInt).toList).toList
    val s1 = s.filter(l => !isSafe(l)) 
    val s2 = s1.filter(l => 
      l.indices.iterator.count(i => isSafe(dropElem(l, i))) > 0
    ) 
    s.count(l => isSafe(l)) + s2.size

  def run(): Unit =
    println("===== Day 2 =====")
    print("Puzzle 1: ")
    println(prob1())

    print("Puzzle 2: ")
    println(prob2())

