package day8

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.BigInt
import scala.math.pow

class Point(var x: Int, var y: Int):
  def nextPoint(dir: Point): Point =
    Point(x + dir.x, y + dir.y)

  override def toString: String =
    s"($x, $y)"

  def outOfRange(height: Int, width: Int): Boolean =
    x < 0 || x >= width || y < 0 || y >= height
    
  def antinodes(other: Point, height: Int, width: Int): List[Point] =
    val dx = (x - other.x).abs
    val dy = (y - other.y).abs
    val r =
    if(x <= other.x && y <= other.y) {
      // left-up
      List(Point(x - dx, y - dy), Point(other.x + dx, other.y + dy))
    } else if (x <= other.x && y > other.y) {
      // left-down
      List(Point(x - dx, y + dy), Point(other.x + dx, other.y - dy))
    } else if (x > other.x && y <= other.y) {
      // right-up
      List(Point(x + dx, y - dy), Point(other.x - dx, other.y + dy))
    } else {
      // right-down
      List(Point(x + dx, y + dy), Point(other.x - dx, other.y - dy))
    }

    r.filter(p =>  {
      p.x >= 0 && p.x < width && p.y >=0 && p.y < height
    })

  def antinodes1(other: Point, height: Int, width: Int): List[Point] =
    val dx = (x - other.x).abs
    val dy = (y - other.y).abs
    val self = Point(x, y)
    //println(s"p1: ${self.toString}, p2: ${other.toString}")
    
    // p1 will always do -dx, -dy
    // p2 will always do +dx, +dy
    val (p1, p2, dxx, dyy) =
      if(x <= other.x && y <= other.y) {
        (self, other, dx, dy)
        // left-up
      } else if (x <= other.x && y > other.y) {
        // left-down
        (self, other, dx, -dy)
      } else if (x > other.x && y <= other.y) {
        // right-up
        (self, other, -dx, dy)
      } else {
        // right-down
        (other, self, dx, dy)
      }
      
    val result = new ListBuffer[Point]()
    var currP = p1 
    var stop = false
    while(!stop) {
      val nextP = Point(currP.x - dxx, currP.y - dyy) 
      stop = nextP.outOfRange(height, width)
      if(!stop) {
       result.append(nextP)
       //println(s"next: ${nextP.toString}") 
      }
      currP = nextP
    }
    
    currP = p2
    stop = false
    while (!stop) {
      val nextP = Point(currP.x + dxx, currP.y + dyy)
      stop = nextP.outOfRange(height, width)
      if (!stop) {
        result.append(nextP)
        //println(s"next1: ${nextP.toString} ${nextP.outOfRange(height, width)} ${height} ${width}")
      }
      currP = nextP
    }
      
    result.toList

class solution(input: String):
  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val antennas = new mutable.HashMap[Char, ListBuffer[Point]]()
    lines.zipWithIndex.foreach((line, y) =>  {
      line.zipWithIndex.foreach((ch, x) => {
        if(ch != '.') {
          antennas.getOrElseUpdate(ch, ListBuffer[Point]()).append(Point(x,y))
        }
      })
    })

    //println(antennas.map((ch, list) => s"$ch:  ${list.map(p => p.toString).mkString("", ",", "")}").mkString("", "\n", "\n"))
    val height = lines.length
    val width = lines.head.length

    val grid = new ListBuffer[ListBuffer[Char]]()
    lines.indices.iterator.foreach(i => {
      val buf = ListBuffer.empty[Char]
      lines(i).foreach(c => buf.append(c))
      grid.append(buf)
    })

    for (elem <- antennas) {
      val points = elem._2
      (0 until points.length - 1).foreach(i =>  {
        (i + 1 until points.length).foreach(j => {
          points(i).antinodes(points(j), height, width).foreach(p => grid(p.y)(p.x) = '#')
        })
      })
    }

    // println(grid.map(g => g.mkString).mkString("\n", "\n", ""))
    grid.map(g => g.count(c => c == '#')).sum


  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val antennas = new mutable.HashMap[Char, ListBuffer[Point]]()
    lines.zipWithIndex.foreach((line, y) => {
      line.zipWithIndex.foreach((ch, x) => {
        if (ch != '.') {
          antennas.getOrElseUpdate(ch, ListBuffer[Point]()).append(Point(x, y))
        }
      })
    })

    //println(antennas.map((ch, list) => s"$ch:  ${list.map(p => p.toString).mkString("", ",", "")}").mkString("", "\n", "\n"))
    val height = lines.length
    val width = lines.head.length

    val grid = new ListBuffer[ListBuffer[Char]]()
    lines.indices.iterator.foreach(i => {
      val buf = ListBuffer.empty[Char]
      lines(i).foreach(c => buf.append(c))
      grid.append(buf)
    })

    for (elem <- antennas) {
      val points = elem._2
      (0 until points.length - 1).foreach(i => {
        (i + 1 until points.length).foreach(j => {
          points(i).antinodes1(points(j), height, width).foreach(p => grid(p.y)(p.x) = '#')
        })
      })
    }

    //println(grid.map(g => g.mkString).mkString("\n", "\n", ""))
    grid.map(g => g.count(c => {
      c == '#' || (c != '.' && antennas(c).length > 1)
    })).sum 


  def run(): Unit =
    println("===== Day 7 =====")
    print("Puzzle 1: ")
    println(prob1())

    print("Puzzle 2: ")
    println(prob2())
    val t1 = System.nanoTime()
