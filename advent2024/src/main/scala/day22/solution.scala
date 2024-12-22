package day22

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

  def nextSecret(in: Int ): Int =
    val v1 = (((in << 6) ^ in) & (0xffffff))
    val v2 = (((v1 >> 5) ^ v1) & (0xffffff))
    val v3 = (((v2 << 11) ^ v2) & (0xffffff))
    v3

  def nthSecret(n: Int, in: Int): Int =
    (0 until n).foldLeft(in)((acc, _) => nextSecret(acc))

  def priceChange(n: Int, in: Int): List[(Int, Int)] =
    (0 until n).foldLeft((List[(Int, Int)](), in))((acc, _) =>
      val nextAcc = nextSecret(acc._2)
      val l = acc._1 :+ ((nextAcc % 10) - (acc._2 % 10), nextAcc %10)
      (l, nextAcc)
    )._1

  def prob1(): Long =
    lines.map(l =>  {
      //println(s"$l: ${nthSecret(2000, l.toLong)}")
      nthSecret(2000, l.toInt).toLong
    }).sum


  def prob2(): Int =
    val prices = lines.map(l =>  {
       val f = priceChange(2000, l.toInt)
       val map = mutable.HashMap[String, List[Int]]()

       f.sliding(4, 1)
        .map(s => (s.map((d, v) => d).map(s => s.toString).mkString, s.last._2)).toList
        .foreach((s, v) =>  {
          val l = map.getOrElseUpdate(s, List[Int]())
          map.update(s, l :+ v)
        })
        // print(map)
        map
    })

    val result = mutable.HashMap[String, Int]()
    prices.foreach(h => {
      h.foreach((p, l) => {
        if (!result.contains(p)) {
          val s = prices.map(t => {
            val v = t.getOrElse(p, List(0)).head
            v
          }).sum
          result.update(p, s)
        }
      })
    })

    result.map((p, s) => s).max

  def run(): Unit =
    println("===== Day 21 =====")
    print("Puzzle 1: \n")
    val t0 = System.nanoTime()
    println(prob1())
    val t1 = System.nanoTime()
    println(s"Part1 Elapsed time: ${(t1 - t0) / 1.0e6}ms")

    val t2 = System.nanoTime()
    print("Puzzle 2: ")
    println(prob2())
    val t3 = System.nanoTime()
    println(s"Part2 Elapsed time: ${(t3 - t2) / 1.0e6}ms")


