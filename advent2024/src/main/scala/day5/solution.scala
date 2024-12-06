package day5

import scala.io.Source.*
import collection.mutable.HashSet
import scala.collection.mutable

class solution(input: String):

  def getOrdered(lines: List[String]): List[mutable.HashSet[Int]] =
    val orders = List.fill(100) {mutable.HashSet[Int]()}
    var parseOrder = true
    lines.indices.iterator.foreach(i => {
      if (lines(i).isEmpty) parseOrder = false
      if (parseOrder) {
        val s = lines(i).split('|').map(s => s.toInt)
        orders(s(0)).update(s(1), true)
      }
    })
    orders

  def prob1(): Int =
    val lines = fromFile(input).getLines.toList
    val orders = getOrdered(lines)
    lines.map(l => {
      if(l.isEmpty || l.split('|').length > 1 ) 0
      else {
        val seq = l.split(",").map(s => s.toInt)
        val sorted = seq.sortWith((i, j) => {
          orders(i).contains(j)
        })
        if(sorted.sameElements(seq)) sorted(sorted.length/2)
        else 0
      }
    }).sum

  def prob2(): Int =
    val lines = fromFile(input).getLines.toList
    val orders = getOrdered(lines)
    lines.map(l => {
      if (l.isEmpty || l.split('|').length > 1) 0
      else {
        val seq = l.split(",").map(s => s.toInt)
        val sorted = seq.sortWith((i, j) => {
          orders(i).contains(j)
        })
        if (!sorted.sameElements(seq)) sorted(sorted.length/ 2)
        else 0
      }
    }).sum

  def run(): Unit =
    println("===== Day 5 =====")
    print("Puzzle 1: ")
    println(prob1())

    print("Puzzle 2: ")
    println(prob2())


