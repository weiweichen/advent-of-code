package day16

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
    
  def isWall(map: List[String]): Boolean = 
    map(y)(x) == '#'
    
  def next(map: List[String], h: Int, w: Int, dir: Char): List[(Point, Long, Char)] =
    val nextDir = dir match {
      case '>' => List((Point(1, 0), 1, '>'), (Point(0, -1), 1001, '^'), (Point(0, 1), 1001, 'v'))
      case '<' => List((Point(-1, 0), 1, '<'), (Point(0, -1), 1001, '^'), (Point(0, 1), 1001, 'v'))
      case '^' => List((Point(0, -1), 1, '^'), (Point(-1, 0), 1001, '<'), (Point(1, 0), 1001, '>'))
      case 'v' => List((Point(0, 1), 1, 'v'), (Point(-1, 0), 1001, '<'), (Point(1, 0), 1001, '>'))
    }
    nextDir.map((p, v, d) => (Point(x + p.x, y + p.y), v.toLong, d))
           .filter((p, v, d) => p.isIn(h, w) && !p.isWall(map))
    

class solution(input: String):
  
  def update(scores: List[ListBuffer[Long]], p: Point, v: Long): Unit =
      scores(p.y)(p.x) = v

  def getScore(scores: List[ListBuffer[Long]], p: Point): Long =
    scores(p.y)(p.x)

  def update(scores: List[List[ListBuffer[Long]]], p: Point, dir: Char, v: Long): Unit =
      val dv = dir match {
        case '>' => 0
        case '<' => 1
        case '^' => 2
        case 'v' => 3
      }
      scores(p.y)(p.x)(dv) = v

  def getScore(scores: List[List[ListBuffer[Long]]], p: Point, dir: Char): Long =
    val dv = dir match {
      case '>' => 0
      case '<' => 1
      case '^' => 2
      case 'v' => 3
    }
    scores(p.y)(p.x)(dv)


  def str(i: Long): String =
    if (i == Long.MaxValue ) "xxxx"
    else  {
      val s = i.toString
      List.fill(4 - s.length)('0').mkString("", "", "") + s
    }

  def str(s: String): String =
    if (s == "") "    "
    else  {
      List.fill(4 - s.length)(' ').mkString("", "", "") + s
    }


  def prob1(): Long =
    val map = fromFile(input).getLines.toList
    val scores = List.fill(map.size) {ListBuffer.fill(map.head.length)(Long.MaxValue)}

    val start = Point(1, map.length-2)
    val end = Point(map.head.length-2, 1)
    val workList = mutable.Queue[(Point, Long, Char)]((start, 0, '>'))
    val h = map.length
    val w = map.head.length
    
    while(workList.nonEmpty) {
      val curr = workList.dequeue
      val cv = getScore(scores, curr._1)
      //println(s"curr ${curr}")
      if(cv >= curr._2) {
        update(scores, curr._1, curr._2)
        val next = curr._1.next(map, h, w, curr._3)
        next.foreach((p, v, d) =>  {
          val nv = curr._2 + v
          //println(s"  enqueue ($p, $d, ${cv + v})")
          workList.enqueue((p, nv, d))
        })
      }
    }
    //println(s"End value: ${getScore(scores, end)}")
    //println(scores.map(s => s.map(n => str(n)).mkString(" ", ", ", "")).mkString("", "\n", ""))
    
    getScore(scores, end)
    
    
  def prob2(): Long =
    val map = fromFile(input).getLines.toList
    val start = Point(1, map.length - 2)
    val end = Point(map.head.length - 2, 1)
    val h = map.length
    val w = map.head.length
    val workList = mutable.PriorityQueue[(Point, Long, Char, List[Point])]((start, 0, '>', List(start)))(Ordering.by(t => -t._2))
    var res = Long.MaxValue
    val tiles = mutable.Set[Point]()
    val scores = List.fill(map.size)(List.fill(map.head.length)(ListBuffer.fill(4)(Long.MaxValue)))

    while(workList.nonEmpty) {
      val curr = workList.dequeue()
      if (curr._1 == end) {
        res = res.min(curr._2)
        if (curr._2 == res) {
          curr._4.foreach(p => tiles.add(p))
        }
      }

      //println(s"curr ${curr}")
      val next = curr._1.next(map, h, w, curr._3)
      next.foreach((p, v, d) => {
        val nv = curr._2 + v
        val sv = getScore(scores, p, d)
        if(sv >= nv) {
          update(scores, p, d, nv)
          workList.enqueue((p, nv, d, curr._4 :+ p))
        }
      })
    }
    tiles.size

  def run(): Unit =
    println("===== Day 16 =====")
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

