package day25

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.math.*


case class Lock(var input: List[String]):
  def heights(): List[Int] =
    (0 until input.head.length).map(i =>  {
      input.indices.drop(1).foldLeft(0)((acc, j) => if(input(j)(i) == '#') acc + 1 else acc)
    }).toList

case class Key(var input: List[String]):
  def heights(): List[Int] =
    (0 until input.head.length).map(i =>  {
      input.indices.reverse.drop(1).foldLeft(0)((acc, j) => if(input(j)(i) == '#') acc + 1 else acc)
    }).toList

class solution(input: String):
  val lines: List[String] = fromFile(input).getLines.toList

  def prob1(): Long =
    val inputs = lines.sliding(8, 8).map(s => s.dropRight(1))
    
    val lks = inputs.foldLeft((List[Lock](), List[Key]()))((acc, input) =>  {
      val locks = acc._1
      val keys = acc._2
      if(input.head.count(s => s == '#') == input.head.length)
        (locks :+ Lock(input), keys)
      else
        (locks, keys :+ Key(input))
    })
    
    val locks = lks._1
    val keys = lks._2
    val lockHs = locks.map(lock => lock.heights())
    val keyHs = keys.map(key => key.heights())
    
    val r = lockHs.map(lh =>  {
      keyHs.count(kh =>  {
        lh.zip(kh).count((l, k) => (l + k) < 6) == 5
      })
    }).sum
    
    println(s"locks: ${locks.length}")
    println(s"keys: ${keys.length}")
    // println(s"lockHs: ${lockHs}")
    // println(s"keyHs: ${keyHs}")
    
    r 
    

  def run(): Unit =
    println("===== Day 25 =====")
    val t0 = System.nanoTime()
    println(prob1())
    val t1 = System.nanoTime()
    println(s"Elapsed time: ${(t1 - t0) / 1.0e6}ms")

