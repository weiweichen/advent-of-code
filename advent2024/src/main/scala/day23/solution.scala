package day23

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.math.*

case class Key(var key: String, var level: Int)

case class Node(var name: String,
                var idx: Int,
                var neighbours: mutable.HashSet[String])

class Graph(edges: List[String]):
  val nodes =  mutable.HashMap[String, Node]()
  val scc3 = mutable.HashSet[String]()

  def build(): Unit =
    edges.foreach(e =>  {
      val n = e.split("-")
      val v1 = n.head
      val v2 = n.last
      nodes.getOrElseUpdate(v1, Node(v1, nodes.size, mutable.HashSet[String]()))
      nodes.getOrElseUpdate(v2, Node(v2, nodes.size, mutable.HashSet[String]()))
      nodes(v1).neighbours.update(v2, true)
      nodes(v2).neighbours.update(v1, true)
    })

  def findSCC3(): Unit =
    nodes.foreach((name, node) => {
      val neighbors = node.neighbours.toList
      neighbors.zipWithIndex.foreach((v1, i) =>  {
        (i + 1 until neighbors.size).foreach(j =>  {
          if(nodes(v1).neighbours.contains(neighbors(j))) {
            val cls = List(name, v1, neighbors(j)).sorted.mkString("", "-", "")
            scc3.update(cls, true)
          }
        })
      })
    })
    
  val cache = mutable.HashMap[Key, Option[List[String]]]()

  def findConnected(inputs: List[String], level: Int): Option[List[String]] =
    if(level == 0) return Some(List[String]())
    var res: Option[List[String]] = None
    val key = inputs.sorted.mkString("", "-", "")
    if(cache.contains(Key(key, level)))
      return cache(Key(key, level))

    inputs.takeWhile(v =>  {
      val nv = nodes(v)
      val newInput = inputs.filter(i => nv.neighbours.contains(i))
      val r = findConnected(newInput, level - 1)
      r match  {
        case Some(rv) => res = Some(rv :+ v)
        case _ =>
      }
      r.isEmpty
    })
    cache.update(Key(key, level), res)
    res

class solution(input: String):
  val lines: List[String] = fromFile(input).getLines.toList

  def prob1(): Long =
    val graph = Graph(lines)
    graph.build()
    graph.findSCC3()

    graph.scc3.count(s => {
      s.split("-").exists(s => s(0) == 't')
    })

  def prob2(): String =
    val graph = Graph(lines)
    graph.build()
    var prev = List[String]()
    val nodeName = graph.nodes.map((name, _) => name).toList

    (1 to 100).takeWhile(i =>  {
      val r = graph.findConnected(nodeName, i)
      r match  {
        case Some(rv) => prev = rv
        case _ =>
      }
      r.isDefined
    })
    prev.sorted.mkString("", ",", "")

  def run(): Unit =
    println("===== Day 23 =====")
    print("Puzzle 1: \n")
    val t0 = System.nanoTime()
    println(prob1())
    val t1 = System.nanoTime()
    println(s"Part1 Elapsed time: ${(t1 - t0) / 1.0e6}ms")

    val t2 = System.nanoTime()
    print("Puzzle 2: \n")
    println(prob2())
    val t3 = System.nanoTime()
    println(s"Part2 Elapsed time: ${(t3 - t2) / 1.0e6}ms")



