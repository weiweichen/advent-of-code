package day20

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

  def dist(p: Point): Long =
    (x - p.x).abs + (y - p.y).abs

class solution(input: String):
  val track:List[String] = fromFile(input).getLines.toList

  def find(ch: Char): Point =
    track.zipWithIndex.flatMap((l, y) =>  {
      l.zipWithIndex.map((s, x) => (s, Point(x, y)))
    }).filter((s, p) => s == ch).head._2

  def getTiles(ch: Char, h: Int, w: Int): List[Point] =
    track.zipWithIndex.flatMap((l, y) => {
      l.zipWithIndex.map((s, x) => (s, Point(x, y)))
    }).filter((s, p) => s == ch).map((_, p) => p).filter(p => p.x > 0 && p.x < w -1 && p.y > 0 && p.y < h -1)

  def race(start: Point, end: Point, h: Int, w: Int, cheat: Point, map: List[ListBuffer[Char]], upper: Long): Long =
    val scores = List.fill(h)(ListBuffer.fill(w)(Long.MaxValue))
    val workList = mutable.PriorityQueue[(Point, Long)]()(Ordering.by(p => -p._2))
    workList.enqueue((start, 0))

    while(workList.nonEmpty) {
      val p = workList.dequeue()
      if(p._1 == end) {
        return p._2
      }
      if(p._2 < scores(p._1.y)(p._1.x)) {
        val newV = (if (p == cheat) 2 else 1) + p._2
        if(newV <= upper) {
          scores(p._1.y)(p._1.x) = p._2
          p._1.next(h, w, map).foreach(n => workList.enqueue((n, newV)))
        }
      }
    }

    0

  def from(p: Point, h: Int, w: Int, map: List[ListBuffer[Char]]): List[ListBuffer[Long]] =
    val r = List.fill(h)(ListBuffer.fill(w)(Long.MaxValue))

    val workList = mutable.Queue[(Point, Long)]()
    val seen = mutable.HashSet[Point]()

    workList.enqueue((p, 0))

    while(workList.nonEmpty) {
      val curr = workList.dequeue()
      curr._1.next(h, w, map).foreach(p => {
        if(!seen.contains(p)) {
          workList.enqueue((p, curr._2 + 1))
        }
      })
      seen.update(curr._1, true)
      r(curr._1.y)(curr._1.x) = curr._2
    }
    r

  def solve(cheats: Long): (Long, Long) =
    val start = find('S')
    val end = find('E')
    val raceTrack = track.map(l => {
      val b = ListBuffer.fill(l.length)('.')
      l.zipWithIndex.foreach((ch, i) => b(i) = ch)
      b
    })
    val h = raceTrack.length
    val w = raceTrack.head.length
    val origin = race(start, end, h, w, Point(-1, -1), raceTrack, Long.MaxValue)
    //println(origin)

    val fromStart = from(start, h, w, raceTrack)
    val fromEnd = from(end, h, w, raceTrack)

    val pointS = getTiles('.', h, w).filter(p => fromStart(p.y)(p.x) < origin - cheats) :+ start
    val pointE = getTiles('.', h, w).filter(p => fromEnd(p.y)(p.x) < origin - cheats) :+ end
    //println(pointS.size)
    //println(pointE.size)

    val r = pointS.map(p=> {
      val ds = fromStart(p.y)(p.x)
      val p1 = pointE.filter(q => p.dist(q) < 3).map(q => {
        val dist = (p.x - q.x).abs + (p.y - q.y).abs
        val de = fromEnd(q.y)(q.x)
        val d = ds + dist + de
        if (d <= origin - cheats) 1 else 0
      }).sum

      val p2 = pointE.filter(q => p.dist(q) < 21).map(q => {
          val dist = (p.x - q.x).abs + (p.y - q.y).abs
          val de = fromEnd(q.y)(q.x)
          val d = ds + dist + de
          if (d <= origin - cheats) 1 else 0
      }).sum
      (p1, p2)
    })

    val part1 = r.map((p1, p2) => p1).sum
    val part2 = r.map((p1, p2) => p2).sum

    (part1, part2)

  def run(): Unit =
    println("===== Day 20 =====")
    val t0 = System.nanoTime()
    val(p1, p2) = solve(100)
    val t1 = System.nanoTime()
    println(s"Elapsed time: ${(t1 - t0) / 1.0e6}ms")

    print("Puzzle 1: ")
    println(p1)
    print("Puzzle 2: ")
    print(p2)
