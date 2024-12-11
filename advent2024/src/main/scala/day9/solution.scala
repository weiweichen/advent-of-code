package day9

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math.BigInt
import scala.math.pow


class solution(input: String):
  def prob1(): Long =
    val lines = fromFile(input).getLines.toList
    val inputs = lines.head.toList.map(c => c.toString.toInt)
    val blocks = new ListBuffer[Int]()

    var idx = 0
    inputs.zipWithIndex.foreach((v, i) =>  {
      val fv = if (i % 2 == 0) i/2 else -1
      (0 until v).foreach(_ => blocks.append(fv))
    })

    //println(inputs.map(v => v.toString).mkString("", "", ""))
    //println(blocks.map(v => v.toString).mkString("", "", ""))

    var i = 0
    var j = blocks.size - 1
    var sum: Long = 0

    while(i <= j)  {
      if(blocks(i) != -1) {
        sum += blocks(i) * i
        i +=1
      } else {
        if(blocks(j) == -1) {
          j -= 1
        } else {
          sum += blocks(j) * i
          blocks.update(i,blocks(j))
          i += 1
          j -= 1
        }
      }
    }
    //println(blocks.map(v => v.toString).mkString("", "", ""))
    sum


  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    val inputs = lines.head.toList.map(c => c.toString.toLong)

    val files = inputs.zipWithIndex.filter((_, i) => i % 2 == 0).map((v, _) => v)
    val segments = inputs.zipWithIndex.filter((_, i) => i % 2 == 1).map((v, _) => v)
    //println(files.map(s => s.toInt).mkString("", ",", ""))
    //println(segments.map(s => s.toInt).mkString("", ",", ""))

    val fblocks = new ListBuffer[(Long, Long, Long)]() // value, origSize, offset
    val sblocks = new ListBuffer[(List[(Long, Long)], Long, Long, Long)]() // value, sizeLeft, offset, origSize
    var idx: Long = 0
    inputs.zipWithIndex.foreach((v, i) => {
      if(i % 2 == 0) {
        fblocks.append(((i/2).toLong, v, idx))
      } else {
        // segments
        sblocks.append((List[(Long, Long)](), v, idx, v))
      }
      idx += v
    })

    //println(fblocks.map(v => s"(${v._1}, ${v._2}, ${v._3})").mkString("", "|", ""))
    //println(sblocks.map(v => s"(${v._1}, ${v._2}, ${v._3})").mkString("", "|", ""))

    fblocks.indices.reverse.foreach(j => {
      val f = fblocks(j)
      val v = sblocks.zipWithIndex.filter(s => (s._1._2 >= f._2) && (f._3 > s._1._3))
      if (v.nonEmpty)  {
        val seg = sblocks(v.head._2)
        sblocks.update(v.head._2, (seg._1 :+ (f._1, f._2), seg._2 - f._2, seg._3, seg._4))
        fblocks.update(j, (-1, f._2, f._3))
      }
    })

    //println(fblocks.map(v => s"(${v._1}, ${v._2}, ${v._3})").mkString("", "|", ""))
    //println(sblocks.map(v => s"(${v._1}, ${v._2}, ${v._3})").mkString("", "|", ""))

    val s1 = fblocks.filter(f => f._1 != -1).map(f => (f._3 * 2 + f._2 - 1) * f._2 / 2 * f._1).sum
    val s2 = sblocks.filter(s => s._1.nonEmpty).map(f => {
      val r = f._1.foldLeft((0.toLong, f._3))((acc, v) => {
        val sum = ((acc._2 * 2 + v._2 - 1) * v._2 / 2) * v._1
        val newoffset = acc._2 + v._2
        (sum + acc._1, newoffset)
      })._1
      r
    }).sum
    s1 + s2

  def run(): Unit =
    println("===== Day 9 =====")
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


