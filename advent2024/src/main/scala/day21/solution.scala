package day21

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.math.*


case class Point(var x: Int, var y: Int)

class solution(input: String):
  val lines: List[String] = fromFile(input).getLines.toList
  val keyPad1: Map[Char, Point] = Map(
    'A' -> Point(2, 3),
    '0' -> Point(1, 3),
    ' ' -> Point(0, 3),
    '1' -> Point(0, 2),
    '2' -> Point(1, 2),
    '3' -> Point(2, 2),
    '4' -> Point(0, 1),
    '5' -> Point(1, 1),
    '6' -> Point(2, 1),
    '7' -> Point(0, 0),
    '8' -> Point(1, 0),
    '9' -> Point(2, 0),
  )

  val keyPad2: Map[Char, Point] = Map(
    'A' -> Point(2, 0),
    '^' -> Point(1, 0),
    ' ' -> Point(0, 0),
    '<' -> Point(0, 1),
    'v' -> Point(1, 1),
    '>' -> Point(2, 1),
  )

  def fewest_steps(layer: Int, keys: String, values: mutable.HashMap[(Int, Char, Char), Long]): Long =
    //println(s"${keys} ${keys.sliding(2, 1).toList}")
    ("A" + keys).toList.sliding(2, 1).map(s => values((layer, s.head, s.last))).sum

  def calc(code: String, steps: Int): Long =
    println(code)

    val values = mutable.HashMap[(Int, Char, Char), Long]()
    "A^ <v>".foreach(ch1 => {
      "A^ <v>".foreach(ch2 => {
        values.update((0, ch1, ch2), 1)
      })
    })

    (1 to steps).foreach(step =>  {
      //println(s"step: $step")
      val keyPad = if (step == steps) keyPad1 else keyPad2
      keyPad.foreach((ks, ps) =>  {
        keyPad.foreach((ke, pe) =>  {
          val hs = (if(ps.x < pe.x) ">" else "<") * (ps.x - pe.x).abs
          val vs = (if(ps.y < pe.y) "v" else "^") * (ps.y - pe.y).abs
          //println(s"hs: $hs, vs: $vs $ps, $pe")
          val hValue = if (Point(pe.x, ps.y) != keyPad(' ')) fewest_steps(step - 1, hs + vs + "A", values) else Long.MaxValue
          val vValue = if (Point(ps.x, pe.y) != keyPad(' ')) fewest_steps(step - 1, vs + hs + "A", values) else Long.MaxValue
          values.update((step, ks, ke), hValue.min(vValue))
        })
      })
    })

    fewest_steps(steps, code, values)


  def prob1(): Long =
    lines.map(line => calc(line, 3) * line.substring(0, 3).toLong).sum

  def prob2(): Long =
    lines.map(line => calc(line, 26) * line.substring(0, 3).toLong).sum


  def run(): Unit =
    println("===== Day 21 =====")
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

