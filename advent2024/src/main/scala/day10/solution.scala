package day10

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.BigInt
import scala.math.pow


case class Point(var x: Int, var y: Int):
  def legal(h: Int, w: Int): Boolean =
    x >= 0 && x < w && y >= 0 && y < h

  def next(h: Int, w: Int): List[Point] =
    List(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1)).filter(p => p.legal(h, w))

  override def toString: String =
    s"($x, $y)"

  override def equals(other: Any): Boolean =
    other match {
      case p: Point => x == p.x && y == p.y
      case _ => false
    }

class solution(input: String):
  def numHikes(start: List[Point], v: Int, h: Int, w: Int, grid: List[List[Int]], distinct: Boolean): List[Point] =
    if(v == 9) {start} else  {
      val next = start.flatMap(s => s.next(h, w)).filter(p => grid(p.y)(p.x) == v + 1)
      if(distinct)
        numHikes(next.distinct, v+1, h, w, grid, distinct)
      else
        numHikes(next, v+1, h, w, grid, distinct)
    }

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val grid = lines.map(l => l.toList.map(ch => ch.toString.toInt))
    val h = grid.length
    val w = grid.head.length
    grid.zipWithIndex.map((g, y) =>  {
      g.zipWithIndex.map((v, x) =>  {
        if (v != 0) {0} else {
          val r = numHikes(List(Point(x, y)), 0, h, w, grid, true)
          // println(s"p: $v ${r.length}")
          r.length
        }
      }).sum
    }).sum

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val grid = lines.map(l => l.toList.map(ch => ch.toString.toInt))
    val h = grid.length
    val w = grid.head.length
    grid.zipWithIndex.map((g, y) => {
      g.zipWithIndex.map((v, x) => {
        if (v != 0) {
          0
        } else {
          val r = numHikes(List(Point(x, y)), 0, h, w, grid, false)
          // println(s"p: $v ${r.length}")
          r.length
        }
      }).sum
    }).sum
    

  def run(): Unit =
    println("===== Day 10 =====")
    print("Puzzle 1: ")
    val t0 = System.nanoTime()
    println(prob1())
    val t1 = System.nanoTime()
    println(s"Part1 Elapsed time: ${(t1 - t0) / 1.0e6}ms")

    val t2 = System.nanoTime()
    print("Puzzle 2: ")
    println(prob2())
    val t3 = System.nanoTime()
    println(s"Part2 Elapsed time: ${(t3 - t2) / 1.0e6}ms")



