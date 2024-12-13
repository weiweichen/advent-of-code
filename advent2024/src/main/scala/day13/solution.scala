package day13

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.*

case class Claw(var x: Long, var y: Long)

case class Config(var a: Claw, var b: Claw, x: Long, y:Long)


class solution(input: String):

  def parse(lines: List[String]): List[Config] =
    lines.sliding(4, 4).map(l =>  {
      val al = l(0).split(" +")
      val ay = al.last.split("\\+").last.toLong
      val ax = al(2).split("\\+").last.split(",").head.toLong

      val bl = l(1).split(" +")
      val by = bl.last.split("\\+").last.toLong
      val bx = bl(2).split("\\+").last.split(",").head.toLong

      val gl = l(2).split(" +")
      val gy = gl.last.split("=").last.toLong
      val gx = gl(1).split("=").last.split(",").head.toLong
      Config(Claw(ax, ay), Claw(bx, by), gx, gy)
    }).toList

  def solve(config: Config): Option[(Long, Long)] =
    // ax * s1 + bx * s2 = X
    // ay * s1 + by * s2 = Y
    // s1 = (X * by - Y * bx) / (ax * by - ay * bx)
    // s2 = (X * ay - Y * ax) / (ay * bx - ax * by)
    // Do the maths!!

    val s1N = config.x * config.b.y - config.y * config.b.x
    val s1D = config.a.x * config.b.y - config.a.y * config.b.x
    val s2N = config.x * config.a.y - config.y * config.a.x
    val s2D = config.a.y * config.b.x - config.a.x * config.b.y

    if(s1D == 0 || s2D == 0) {
      // If this is printed, I need to do handle a special case, but it doesn't happen with the input :-)
      println(s"Something is wrong $config")
      return None
    }

    if(s1N % s1D == 0 && s2N % s2D == 0) Some((s1N / s1D, s2N / s2D)) else None

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val configs = parse(lines)
    //println(configs.mkString("\n", "\n", ""))
    configs.map(config => solve(config))
      .map {
        case Some(sa, sb) =>
          sa * 3 + sb
        case _ =>
          0
      }.sum

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val configs = parse(lines).map(config => 
      Config(config.a, config.b, config.x + "10000000000000".toLong, config.y + "10000000000000".toLong))
    //println(configs.mkString("\n", "\n", ""))
    configs.map(config => {
      solve(config)
    }).map {
      case Some(sa, sb) =>
        sa * 3 + sb
      case _ =>
        0
    }.sum

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
