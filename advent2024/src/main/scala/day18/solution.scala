package day18

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.math.*

case class Point(var x: Int, var y: Int):
  def isIn(h: Int, w: Int): Boolean =
    x >= 0 && x < w && y >= 0 && y < h

  def isWall(memory: List[ListBuffer[Char]]): Boolean =
    memory(y)(x) == '#'

  def next(h: Int, w: Int, memory: List[ListBuffer[Char]]): List[Point] =
    List(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
      .map(d => Point(x + d.x, y + d.y))
      .filter(p => p.isIn(h, w) && !p.isWall(memory))

class solution(input: String):
  
  def paths(bytes: List[Point], h: Int, w: Int, numBytes: Int): Int =
    val memory = List.fill(h)(ListBuffer.fill(w)('.'))
    val distances = List.fill(h)(ListBuffer.fill(w)(Int.MaxValue))
    bytes.take(numBytes).foreach(b => memory(b.y)(b.x) = '#')
    val worklist = mutable.PriorityQueue[(Point, Int)]()(Ordering.by(p => -p._2))
    val start = Point(0, 0)
    val exit = Point(h - 1, w - 1)

    worklist.enqueue((start, 0))

    while (worklist.nonEmpty) {
      val (p, v) = worklist.dequeue()
      if (p == exit) {
        return v
      }

      if (distances(p.y)(p.x) > v) {
        distances(p.y)(p.x) = v
        p.next(h, w, memory).foreach(n => {
          worklist.enqueue((n, v + 1))
        })
      }
    }
    0


  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val bytes = lines.map(l =>  {
      val s = l.split(",")
      Point(s.head.toInt, s.last.toInt)
    })
    val h = 71
    val w = 71
    val numBytes = 1024 
    paths(bytes, h, w, numBytes)


  def prob2(): Point =
    val lines = fromFile(input).getLines.toList
    var lower = 0 
    var upper = lines.length - 1
    val h = 71
    val w = 71

    val bytes = lines.map(l => {
      val s = l.split(",")
      Point(s.head.toInt, s.last.toInt)
    })
    
    while(lower < upper) {
      val curr = (lower + upper) / 2
      if(paths(bytes, h, w, curr) > 0) {
        lower = curr + 1
      } else {
        upper = curr - 1
      }
    }
    
    if(paths(bytes, h, w, lower) > 0)
      bytes(lower)
    else  
      bytes(lower - 1)
    

  def run(): Unit =
    println("===== Day 18 =====")
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



