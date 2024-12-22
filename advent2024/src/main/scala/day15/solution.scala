package day15

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.*

case class Point(var x: Int, var y: Int):
  def next(dir: Char): Point =
    dir match {
      case '<' => Point(x - 1, y)
      case '>' => Point(x + 1, y)
      case '^' => Point(x, y - 1)
      case 'v' => Point(x, y + 1)
      case _ => Point(x, y)
    }

  def prev(dir: Char): Point =
    dir match {
      case '<' => Point(x + 1, y)
      case '>' => Point(x - 1, y)
      case '^' => Point(x, y + 1)
      case 'v' => Point(x, y - 1)
      case _ => Point(x, y)
    }
  def right : Point =
    Point(x + 1, y)

  def gpsCoord: Long =
    y * 100 + x
    
  def gpsBigCoord: Long =
    y * 100 + (x - 1)

case class Map(var map: List[ListBuffer[Char]]):
  def initRobotPos(): List[Point] =
    map.zipWithIndex.flatMap((l, y) => {
      l.zipWithIndex.map((c, x) => (c, Point(x, y)))
    }).filter((c, p) => c == '@').map((c, p) => p)

  override def toString: String =
    s"${map.map(l => l.mkString("", "", "")).mkString("", "\n", "")}"

  def item(p: Point): Char =
    map(p.y)(p.x)

  def isWall(p: Point): Boolean =
    map(p.y)(p.x) == '#'

  def isObject(p: Point): Boolean =
    map(p.y)(p.x) == 'O'

  def isEmptySpace(p: Point): Boolean =
    map(p.y)(p.x) == '.'

  def isBigObject(p: Point): Boolean =
    map(p.y)(p.x) == '[' || map(p.y)(p.x) == ']'

  def update(ch: Char, p: Point) =
    map(p.y)(p.x) = ch

  def robotMoveOneStep(dir: Char, init: Point): Point =
    val next = init.next(dir)
    if(isWall(next)) {
      init
    } else if(isObject(next)) {
      var n = next
      while(isObject(n)) {
        n = n.next(dir)
      }
      if(isWall(n)) {
        init
      } else {
        update('O', n)
        update('@', next)
        update('.', init)
        next
      }
    } else {
      update('.', init)
      update('@', next)
      next
    }

  def nextHBigObjectPoints(points: List[Point], dir: Char): List[Point] =
    points.map(p => p.next(dir)).filter(p => isBigObject(p)).map(p => item(p) match {
        case '[' => if(dir == '>') Point(p.x + 1, p.y) else p 
        case ']' => if(dir == '>') p else Point(p.x - 1, p.y) 
      }).filter(p => isBigObject(p))

  def nextVBigObjectPoints(points: List[Point], dir: Char): (List[Point], Boolean) =
    // only stores `[` position
    val n = points.flatMap(p => {
      if(item(p) == '@') {
        List(p.next(dir))  
      } else {
        val right = Point(p.x + 1, p.y)
        List(p.next(dir), right.next(dir))
      }
    })
    if(n.exists(p => isWall(p))) {
      (List(), true)
    } else {
      val next = n.filter(p => isBigObject(p)).map(p =>  {
        item(p) match {
          case ']' => Point(p.x - 1, p.y)
          case '[' => p
        }
      })
      (next, false)
    }

  def robotMoveOneBigStep(dir: Char, init: Point): Point =
    val next = init.next(dir)
    if (isWall(next)) {
      init
    } else if (isEmptySpace(next)) {
      update('.', init)
      update('@', next)
      next
    } else {
      var points = List(init)
      if(dir == '<' || dir == '>') {
        while (points.nonEmpty) {
          val ns = nextHBigObjectPoints(points, dir)
          // println(s"Points: $ns $dir")
          if(ns.isEmpty) {
            val n = points.head.next(dir)
            if(isWall(n)) {
              return init
            } else {
              var curr = init
              var currP = item(init)
              var nextP = item(init)
              while(curr != n) {
                val next = curr.next(dir)
                nextP = item(next)
                update(currP, next)
                curr = next
                currP = nextP
              }
              update('.', init)
              update('@', next)
            }
          }
          points = ns
        }
        next
      } else {
        var ns  = nextVBigObjectPoints(points, dir)
        var linesToUpdate = List[List[Point]]()
        while(ns._1.nonEmpty && !ns._2 ) {
          linesToUpdate = linesToUpdate :+ ns._1
          // println(s"nextV: $ns")
          ns = nextVBigObjectPoints(ns._1, dir)
        }

        if(ns._2) {
          // hits wall
          init
        } else {
          // update things
          linesToUpdate.reverse.foreach(l =>  {
            l.foreach(p => {
              update('.', p)
              update('.', p.right)
              update('[', p.next(dir))
              update(']', p.right.next(dir))
            })
          })
          update('.', init)
          update('@', next)
          next
        }
      }
    }


  def objectGPSCoordSum : Long =
    map.zipWithIndex.flatMap((l, y) => {
      l.zipWithIndex.map((c, x) => (c, Point(x, y)))
    }).filter((c, p) => c == 'O').map((c, p) => p.gpsCoord).sum

  def objectGPSBigCoordSum : Long =
    map.zipWithIndex.flatMap((l, y) => {
      l.zipWithIndex.map((c, x) => (c, Point(x, y)))
    }).filter((c, p) => c == '[').map((c, p) => p.gpsCoord).sum


class solution(input: String):

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val m = lines.takeWhile(p => p.nonEmpty).map(l => {
      val buf = ListBuffer.empty[Char]
      l.toList.foreach(c => buf.append(c))
      buf
    })
    val moves = lines.splitAt(m.size + 1)._2.flatMap(l => l.toList)

    val map = Map(m)
    val robot = map.initRobotPos().head
    val steps = 5
    // println(map)
    // println(robot)
    // print(s"#moves ${moves.size}")
    // println(moves.toString)

    var pos = robot
    moves.zipWithIndex.foreach((m, step) =>  {
      //println(s"\nRobot is at $pos moves $m")
      pos = map.robotMoveOneStep(m, pos)
    })

    // println(map)
    // println(s"\nRobot is at $pos")
    map.objectGPSCoordSum


  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val inputMap = lines.takeWhile(p => p.nonEmpty)
    val m = inputMap.map(l =>  {
      val buf = ListBuffer.empty[Char]
      l.foreach {
        case '#' => buf.append('#').append('#')
        case '.' => buf.append('.').append('.')
        case '@' => buf.append('@').append('.')
        case 'O' => buf.append('[').append(']')
        case _ =>
      }
      buf
    })

    val moves = lines.splitAt(inputMap.size + 1)._2.flatMap(l => l.toList)
    val map = Map(m)
    val robot = map.initRobotPos().head
    // println(robot)
    // println(s"#moves ${moves.size}")
    // println(moves.toString)

    // println(map)

    var pos = robot
    moves.zipWithIndex.foreach((m, step) => {
      // println(s"\nRobot is at $pos moves $m")
      pos = map.robotMoveOneBigStep(m, pos)
      // println(map)
    })
    
    // println(map)
    // println(s"\nRobot is at $pos")
    map.objectGPSBigCoordSum



  def run(): Unit =
    println("===== Day 15 =====")
    println("Puzzle 1: ")
    val t0 = System.nanoTime()
    println(prob1())
    val t1 = System.nanoTime()
    println(s"Part1 Elapsed time: ${(t1 - t0) / 1.0e6}ms")

    val t2 = System.nanoTime()
    println("Puzzle 2: ")
    println(prob2())
    val t3 = System.nanoTime()
    println(s"Part2 Elapsed time: ${(t3 - t2) / 1.0e6}ms")

