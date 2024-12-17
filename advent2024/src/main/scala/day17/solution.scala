package day17

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.math.*


case class Machine(var A: Long, var B: Long, var C: Long, var program: List[Int]):
  var pc = 0
  var output = List[String]()

  def combo(operand: Int): Long =
    operand match  {
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case 3 => 3
      case 4 => A
      case 5 => B
      case 6 => C
    }

  def adv(): Unit =
    val v = A / pow(2, combo(program(pc + 1)))
    A = (if(v >= 0) v.floor else -(v.abs.floor)).toLong
    pc += 2

  def bxl(): Unit =
    B = B ^ program(pc + 1)
    pc += 2

  def bst(): Unit =
    B = combo(program(pc + 1)) % 8
    pc += 2

  def jnz(): Unit =
    if(A == 0) { pc += 2}
    else {
      pc = program(pc + 1)
    }

  def bxc(): Unit =
    B = B ^ C
    pc += 2

  def out(): Unit =
    val v = combo(program(pc + 1)) % 8
    output = output :+ v.toString
    pc += 2

  def bdv(): Unit =
    val v = A / pow(2, combo(program(pc + 1)))
    B = (if(v >= 0) v.floor else -(v.abs.floor)).toLong
    pc += 2

  def cdv(): Unit =
    val v = A / pow(2, combo(program(pc + 1)))
    C = (if(v >= 0) v.floor else -(v.abs.floor)).toLong
    pc += 2

  def halt(): Boolean =
    pc >= program.size

  def run(): Unit =
    program(pc) match  {
      case 0 => adv()
      case 1 => bxl()
      case 2 => bst()
      case 3 => jnz()
      case 4 => bxc()
      case 5 => out()
      case 6 => bdv()
      case 7 => cdv()
    }

  override def toString: String =
    s"A: ${A}, B: ${B}, C: ${C}, pc: ${pc}, ${output}"

  def outputStr() : String =
    s"${output.mkString("", ",", "")}"

  def runUntil(outputLimit: Int): Unit =
    while(!halt() && output.length < outputLimit) {
      run()
    }

class solution(input: String):

  def prob1(): String =
    val lines = fromFile(input).getLines.toList
    val A = lines(0).split(": ").last.toInt
    val B = lines(1).split(": ").last.toInt
    val C = lines(2).split(": ").last.toInt
    val program = lines(4).split(": ").last.split(",").map(p => p.toInt).toList
    // println(s"A: $A")
    // println(s"B: $B")
    // println(s"C: $C")
    // println(s"program: ${program}")

    val machine = Machine(A, B, C, program)
    // println(s"machine: $machine")
    while(!machine.halt()) {
      machine.run()
      //println(s"machine: $machine")
    }

    machine.output.mkString("", ",", "")

  def prob2(): Long =
    val lines = fromFile(input).getLines.toList
    var A = 0.toLong
    val B = lines(1).split(": ").last.toLong
    val C = lines(2).split(": ").last.toLong
    val program = lines(4).split(": ").last.split(",").map(p => p.toInt).toList
    // println(s"A: $A")
    // println(s"B: $B")
    // println(s"C: $C")
    // println(s"program: ${program}")
    val pstr = program.mkString("", ",", "")

    // Program: 2,4,1,2,7,5,4,3,0,3,1,7,5,5,3,0
    // 2,4 B = A % 8
    // 1,2 B = B ^ 2
    // 7,5 C = A >> B
    // 4,3 B = B ^ C
    // 0,3 A = A >> 3
    // 1,7 B = B ^ 7
    // 5,5 output(B % 8)
    // 3,0 jnz(0)


    // B = ((A % 8) ^ 2) ^ (A >> ((A % 8) ^ 2)) ^ 7
    // A = A >> 3
    // output(B)

    var results = mutable.HashSet[Long](0)

    (0 until program.size).foreach(i =>  {
      val subP = program.drop(program.size - 1 - i).mkString("", ",", "")
      val cr = mutable.HashSet[Long]()
      results.foreach(r =>  {
        (0 to 7).foreach(d =>  {
          val machine = Machine((r << 3) + d, 0, 0, program)
          machine.runUntil(i + 1)
          if(machine.outputStr() == subP)
            cr.add((r << 3) + d)
        })
      })
      results = cr
    })

    results.min()

  def run(): Unit =
    println("===== Day 17 =====")
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


