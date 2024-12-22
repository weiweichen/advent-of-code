package day12

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.*

case class Point(var x: Int, var y: Int):
  def isIn(h: Int, w: Int): Boolean =
    x >= 0 && x < w && y >= 0 && y < h

  def next(h: Int, w: Int): List[Point] =
    List((-1, 0), (1, 0), (0, 1), (0, -1)).map((dx, dy) => Point((x + dx), y + dy)).filter(p => p.isIn(h, w))



class solution(input: String):

  def getAreaPerimeter(p: Point,
                   h: Int,
                   w: Int,
                   garden: List[List[Char]],
                   visited: ListBuffer[ListBuffer[Boolean]],
                   curr: (Long, Long)): (Long, Long) =

    if(visited(p.y)(p.x)) {
      curr
    } else {
      visited(p.y)(p.x) = true
      val n1 = p.next(h, w)
      val next = n1.filter(n => garden(n.y)(n.x) == garden(p.y)(p.x))
      val newPerimeter = 4 - next.size 
      val r = next.filter(n => !visited(n.y)(n.x)).map(n =>
        getAreaPerimeter(Point(n.x, n.y), h, w, garden, visited, curr))
      (r.map(v => v._1).sum + 1, r.map(v=> v._2).sum + newPerimeter)
    }

  def getArea(p: Point,
                   h: Int,
                   w: Int,
                   garden: List[List[Char]],
                   visited: ListBuffer[ListBuffer[Boolean]],
                   block: ListBuffer[ListBuffer[Char]], 
                   curr: Long): Long =

    if(visited(p.y)(p.x)) {
      curr
    } else {
      visited(p.y)(p.x) = true
      block(p.y)(p.x) = garden(p.y)(p.x)
      val n1 = p.next(h, w)
      val next = n1.filter(n => garden(n.y)(n.x) == garden(p.y)(p.x))
      next.filter(n => !visited(n.y)(n.x)).map(n =>
        getArea(Point(n.x, n.y), h, w, garden, visited, block, curr)).sum + 1
    }
 
  def getSides(c: Char, blocks: ListBuffer[ListBuffer[Char]], h: Int, w: Int): Long =
    val s1 = blocks.zipWithIndex.map((line, y) => {
      val s = line.zipWithIndex.foldLeft((0, 0))((acc, l) =>  {
        if (l._1 != c) {acc} else {
          val up = if(blocks(y-1)(l._2) != c && (blocks(y)(l._2-1) != c || blocks(y-1)(l._2 - 1) == c)) 1 else 0
          val down = if(blocks(y+1)(l._2) != c && (blocks(y)(l._2-1) != c || blocks(y+1)(l._2 -1) == c)) 1 else 0
          (acc._1 + up, acc._2 + down)
        }
      })
      //println(s"$line $s")
      s._1 + s._2
    }).sum

    val s2 = (0 until w).map(x =>  {
      val s = (0 until h).foldLeft((0, 0))((acc, y) => {
        val l = blocks(y)(x)
        if (l != c) {
          acc
        } else {
          val left = if (blocks(y)(x-1) != c && (blocks(y-1)(x) != c || blocks(y - 1)(x - 1) == c)) 1 else 0
          val right = if (blocks(y)(x+1) != c && (blocks(y-1)(x) != c || blocks(y - 1)(x + 1) == c)) 1 else 0
          (acc._1 + left, acc._2 + right)
        }
      })
      s._1 + s._2
    }).sum
    //println(s"s1: $s1 s2: $s2")
    s1 + s2

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val h = lines.size + 2
    val w = lines.head.length + 2
    val f = List.fill(w)('#')
    val garden = lines.map(line => line.toList.+:('#').:+('#')).+:(f).:+(f)

    val visited = ListBuffer.fill(h)(ListBuffer.fill(w)(false))
    (0 until w).foreach(x =>  {
      visited(0)(x) = true
      visited(x)(0) = true
      visited(h-1)(x) = true
      visited(x)(w-1) = true
    })

    //println(garden.mkString("\n", "\n", ""))
    garden.zipWithIndex.flatMap((row, y) => {
      row.zipWithIndex.map((pot, x) => {
        if(visited(y)(x)) {(0.toLong, 0.toLong)} else {
          val r = getAreaPerimeter(Point(x, y), h, w, garden, visited, (0, 0))
          //println(s"starting point ($y, $x) area: ${r._1} perimeter: ${r._2}")
          r
        }
      })
    }).map((a, p) => a * p).sum
    

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val h = lines.size + 2
    val w = lines.head.length + 2
    val f = List.fill(w)('#')
    val garden = lines.map(line => line.toList.+:('#').:+('#')).+:(f).:+(f)

    val visited = ListBuffer.fill(h)(ListBuffer.fill(w)(false))
    (0 until w).foreach(x => {
      visited(0)(x) = true
      visited(x)(0) = true
      visited(h - 1)(x) = true
      visited(x)(w - 1) = true
    })

    //println(garden.mkString("\n", "\n", ""))

    garden.zipWithIndex.flatMap((row, y) => {
      row.zipWithIndex.map((pot, x) => {
        if (visited(y)(x)) {
          (0.toLong, 0.toLong)
        } else {
          val block = ListBuffer.fill(h)(ListBuffer.fill(w)('.'))
          val r = getArea(Point(x, y), h, w, garden, visited, block, 0)
          //println(s"starting point ($y, $x) area: ${r}")
          val sides = getSides(garden(y)(x), block, h, w)
          //println(s"sides $sides")
          //println(block.map(b => b.mkString("", "", "")).mkString("", "\n", ""))
          (r, sides) 
        }
      })
    }).map((a, p) => a * p).sum

    

  def run(): Unit =
    println("===== Day 12 =====")
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





