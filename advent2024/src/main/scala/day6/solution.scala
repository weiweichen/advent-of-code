package day6

import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

class Point(var x: Int, var y: Int):
  def canExit(h: Int, w: Int): Boolean =
    (x < 0 || x == w) || (y < 0 || y == h)

  def nextPoint(dir: Point): Point =
    Point(x + dir.x, y + dir.y)

class solution(input: String):
  def findGuard(lines: List[String]): Point =
    val r = lines.indices.iterator.map(i => {
      lines(i).indices.iterator.map(j =>  {
        if(lines(i)(j) == '^') (i, j)
        else (-1, -1)
      })
    }).flatMap(i => i).toList.filter((i, j) => i != -1 && j != -1 ).head
    Point(r._2, r._1)

  def nextDir(curr: Point): Point =
    (curr.x, curr.y) match  {
      case (-1, 0) => Point(0, -1)
      case (0, -1) => Point(1, 0)
      case (1, 0) => Point(0, 1)
      case (0, 1) => Point(-1, 0)
      case _ => Point(0, 0)
    }

  def prob1Grid(input: String): ListBuffer[ListBuffer[Char]] =
    val lines = fromFile(input).getLines.toList
    val guard = findGuard(lines)
    val grid = new ListBuffer[ListBuffer[Char]]()
    lines.indices.iterator.foreach(i => {
      val buf = ListBuffer.empty[Char]
      lines(i).foreach(c => buf.append(c))
      grid.append(buf)
    })
    val height = lines.size
    val width = lines.head.length
    var currGuard = guard
    var currDir = Point(0, -1)
    while (!currGuard.canExit(height, width)) {
      grid(currGuard.y)(currGuard.x) = '@'
      val next = currGuard.nextPoint(currDir)
      if (!next.canExit(height, width)) {
        if (grid(next.y)(next.x) != '#') {
          currGuard = next
        } else {
          currDir = nextDir(currDir)
        }
      } else {
        currGuard = next
      }
    }
    grid

  def prob1(): Int =
    val grid = prob1Grid(input)
    grid.map(i => i.count(c => c == '@')).sum

  def prob2(): Int =
    val lines = fromFile(input).getLines.toList
    val height = lines.size
    val width = lines.head.length

    var count = 0
    val guard = findGuard(lines)
    val threshold = lines.map(l => l.count(c => c == '.')).sum
    val grid1 = prob1Grid(input)

    (0 until height).foreach(y => {
      (0 until width).foreach(x => {
        if(grid1(y)(x) == '@') {
          val grid = new ListBuffer[ListBuffer[Char]]()
          lines.indices.iterator.foreach(i => {
            val buf = ListBuffer.empty[Char]
            lines(i).foreach(c => buf.append(c))
            grid.append(buf)
          })
          grid(y)(x) = '#'
          val orders = List.fill(height) {
            List.fill(width) {
              HashSet[(Int, Int)]()
            }
          }
          var currGuard = guard
          var currDir = Point(0, -1)
          var step = 0
          var stop = false
          while (!currGuard.canExit(height, width) && !stop) {
            if(orders(currGuard.y)(currGuard.x).contains(currDir.y, currDir.x)) {
              stop = true
            } else {
              orders(currGuard.y)(currGuard.x).update((currDir.y, currDir.x), true)
              val next = currGuard.nextPoint(currDir)
              if (!next.canExit(height, width)) {
                if (grid(next.y)(next.x) != '#') {
                  currGuard = next
                } else {
                  currDir = nextDir(currDir)
                }
              } else {
                currGuard = next
              }
            }
          }
          if (stop)
            count += 1
        }
      })
    })
    count

  def run(): Unit =
    println("===== Day 6 =====")
    print("Puzzle 1: ")
    println(prob1())

    print("Puzzle 2: ")
    val t0 = System.nanoTime()
    println(prob2())
    val t1 = System.nanoTime()
    val elapsed = {
      (t1 - t0) / 1.0e6
    }
    println(s"Part2 Elapsed time: $elapsed ms")



