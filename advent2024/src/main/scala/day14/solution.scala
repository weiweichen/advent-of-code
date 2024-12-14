package day14

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.*

case class Point(var x: Int, var y: Int):
  def quadrant(h: Int, w: Int): Int =
    val hh = h / 2
    val hw = w / 2
    if(x < hw && y < hh) {1}
    else if (x < hw && y > hh) {2}
    else if (x > hw && y < hh) {3}
    else if (x > hw && y > hh) {4}
    else 0

  def half(h: Int, w: Int): Int =
    val hw = w / 2
    if (x < hw) {1}
    else if(x > hw) {2}
    else {0}

case class Robot(var start: Point, var velocity: Point):
  def move(h: Int, w: Int, sec: Int): Point =
    val nx = (start.x + velocity.x * sec)
    val ny = (start.y + velocity.y * sec)
    //println(s"$start ($nx, $ny)")
    val rx = if (nx > 0) {nx % w} else {w - nx.abs % w}
    val ry = if (ny > 0) {ny % h} else {h - ny.abs % h}
    val rx1 = if (rx == w) 0 else rx
    val ry1 = if (ry == h) 0 else ry
    Point(rx1, ry1)

class solution(input: String):
  def parse(lines: List[String]): List[Robot] =
    lines.map(l => {
      val s = l.split(" +")
      val p = s.head.drop(2).split(",").map(s => s.toInt)
      val v = s.last.drop(2).split(",").map(s => s.toInt)
      Robot(Point(p(0), p(1)), Point(v(0), v(1)))
    })

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val robots = parse(lines)
    val h = 103
    val w = 101
    val time = 100
    val pos = robots.map(robot => robot.move(h, w, time))
    val pos1 = pos.map(p => p.quadrant(h, w))
    val pos2 = pos1.filter(q => q != 0).groupBy(l => l).map((_, l) => l.length)

    //println(robots.mkString("\n", "\n", ""))
    //println(pos.mkString("", "\n", ""))
    //println(pos1.mkString("\n=========\n", "\n", ""))
    //println(pos2.mkString("\n=========\n", "\n", ""))
    pos2.product

  def printGarden(p: List[Point], h: Int, w: Int): Unit =
    val g = ListBuffer.fill(h) {ListBuffer.fill(w){'.'}}
    p.foreach(p => {g(p.y)(p.x) = 'x'})
    println(g.map(l => l.mkString("", "", "")).mkString("\n", "\n", ""))

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val robots = parse(lines)
    val h = 103
    val w = 101
    var time = 1
    var stop = false

    while(!stop) {
      val pos = robots.map(robot => robot.move(h, w, time))
      if(pos.groupBy(l => l).size == robots.size) {
        stop = true
        printGarden(pos, h, w)
      } else {
        time += 1
      }
    }
    time


  def run(): Unit =
    println("===== Day 13 =====")
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
