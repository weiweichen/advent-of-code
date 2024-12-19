package day19

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.math.*


class solution(input: String):

  def isValid(design: String, towels: Set[String], longest: Int): Boolean =
    if(design.isEmpty) return true
    (1 to longest.min(design.length)).exists(i =>  {
      val ct = design.take(i)
      if(towels.contains(ct)) {
        isValid(design.drop(i), towels, longest)
      } else {
        false
      }
    })

  def calc(design: String, towels: Set[String], longest: Int, cache: mutable.HashMap[String, Long]): Long =
    if (design.isEmpty) return 1 
    (1 to longest.min(design.length)).map(i => {
      val ct = design.take(i)
      if (!towels.contains(ct)) 
        0
      else {
        val next = design.drop(i) 
        cache.get(next) match {
          case Some(v) => v
          case _ => 
            cache.getOrElseUpdate(next, calc(design.drop(i), towels, longest, cache))
        }
      }
    }).sum

  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val towels = lines.head.split(", ").toSet
    val longest = towels.map(t => t.length).max
    val designs = lines.drop(2)

    designs.count(d => isValid(d, towels, longest))

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val towels = lines.head.split(", ").toSet
    val longest = towels.map(t => t.length).max
    val designs = lines.drop(2)
    
    designs.filter(d => isValid(d, towels, longest)).map(d =>  {
      val cache = mutable.HashMap[String, Long]()
      calc(d, towels, longest, cache)
    }).sum

  def run(): Unit =
    println("===== Day 19 =====")
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
