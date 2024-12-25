package day24

import scala.collection.mutable
import scala.io.Source.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.math.*

case class Op(var a: String, var b: String, var op: String)

case class AddExpr(var d: String, var carryOver: String)

class Graph(lines: List[String]):
  var wireValues = mutable.HashMap[String, Int]()
  var wireExprs = mutable.HashMap[String, String]()
  var exprWires = mutable.HashMap[String, String]()
  var ops = Map[String, Op]()
  var gates = mutable.HashSet[String]()

  def isXorYWire(in: String): Boolean =
    in(0) == 'x' || in(0) == 'y'

  def build(): Unit =
    val inLines = lines.takeWhile(l => l.nonEmpty)
    val gateLines = lines.drop(inLines.size + 1)
    // println(s"lines $inLines")
    // println(s"gates $gateLines")

    wireValues = mutable.HashMap[String, Int]()
    inLines.foreach(l => {
      val s = l.split(": ")
      wireValues.update(s.head, s.last.toInt)
      gates.update(s.head, true)
    })

    ops = gateLines.map(l => {
      val s = l.split(" -> ")
      val g = s.head.split(" ")
      gates.update(s.last, true)
      if(isXorYWire(g(0)) && isXorYWire(g(2))) {
        wireExprs.update(s.last, s"(${s.head})")
      }

      exprWires.update(s"(${g(0)} ${g(1)} ${g(2)})", s.last)
      exprWires.update(s"(${g(2)} ${g(1)} ${g(0)})", s.last)
      s.last -> Op(g(0), g(2), g(1))
    }).toMap

    //println(s"${exprWires.toList.sorted.map(s => s).mkString("", "\n", "")}")
    //println(s"${exprWires.size}")

  def build2(): Unit =
    val inLines = lines.takeWhile(l => l.nonEmpty)
    val gateLines = lines.drop(inLines.size + 1)
    // println(s"lines $inLines")
    // println(s"gates $gateLines")
    gateLines.foreach(l => {
      val s = l.split(" -> ")
      val g = s.head.split(" ")
      gates.update(s.last, true)
      wireExprs.update(s.last, s"(${s.head})")

      exprWires.update(s"(${g(0)} ${g(1)} ${g(2)})", s.last)
      exprWires.update(s"(${g(2)} ${g(1)} ${g(0)})", s.last)
      s.last -> Op(g(0), g(2), g(1))
    })
  //println(s"${exprWires.toList.sorted.map(s => s).mkString("", "\n", "")}")
  //println(s"${exprWires.size}")


  def computeGate(gate: String): Int =
    val op = ops(gate)
    val a = wireValues.getOrElseUpdate(op.a, computeGate(op.a))
    val b = wireValues.getOrElseUpdate(op.b, computeGate(op.b))
    val v = op.op match  {
      case "AND" => a & b
      case "OR" => a | b
      case "XOR" => a ^ b
      case _ => 0
    }
    wireValues.update(gate, v)
    v

  def compute(): Unit =
    gates.foreach(g =>  {
      if(!wireValues.contains(g))
        computeGate(g)
    })
    //println(s"${wireValues.toList.sorted.reverse}")

  var zExprs = mutable.HashMap[String, AddExpr]()
  var pairsToSwap = ListBuffer[String]() 
  
  def swap(w1: String, w2: String): Unit =
    val w1Expr = wireExprs(w1).tail.dropRight(1).split(" ")
    val w2Expr = wireExprs(w2).tail.dropRight(1).split(" ")
    
    exprWires(s"(${w1Expr(0)} ${w1Expr(1)} ${w1Expr(2)})") = w2
    exprWires(s"(${w1Expr(2)} ${w1Expr(1)} ${w1Expr(0)})") = w2
    exprWires(s"(${w2Expr(0)} ${w2Expr(1)} ${w2Expr(2)})") = w1
    exprWires(s"(${w2Expr(2)} ${w2Expr(1)} ${w2Expr(0)})") = w1
    pairsToSwap = pairsToSwap :+ w1 :+ w2

  def computeZ(n: Int) : Unit =
    // Bit operations to do addition z = x + y 
    // z(0) = x(0) ^ y(0)
    // c(0) = x(0) & y(0)
    // z(n) = (x(n) ^ y(n)) ^ c(n - 1)
    // c(n) = (x(n) & y(n)) | (((x(n) ^ y(n)) & c(n - 1))) 
    val sn = if(n < 10) "0"+n.toString else n.toString

    val xorExpr = s"(x${sn} XOR y${sn})"
    val andExpr = s"(x${sn} AND y${sn})"

    if(n == 0) {
      val xor = exprWires.getOrElse(xorExpr, "")
      val and = exprWires.getOrElse(andExpr, "")
      zExprs.update(s"z$sn", AddExpr(xor, and))
    } else {
      val sprev = if(n - 1 < 10) "0" + (n-1).toString else (n-1).toString
      var prevC = zExprs(s"z$sprev").carryOver
      val z = s"z$sn"

      var xor = exprWires.getOrElse(xorExpr, "")
      var e3 = s"(${xor} XOR ${prevC})"
      var w3 = exprWires.getOrElse(e3, "")
      if(w3.nonEmpty && w3 != z) {
        // swap w3 and z 
        //println(s"swap $w3, $z")
        swap(w3, z) 
        w3 = exprWires.getOrElse(e3, "")
      } else if (w3.isEmpty) {
        val e = wireExprs(z).drop(1).dropRight(1).split(" ") 
        val w1 = List(e(0), e(2)).filter(s => !(s == xor || s == prevC)).head
        val w2 = List(xor, prevC).filter(s => !(s == e(0) || s == e(2))).head
        //println(s"swap $w1, $w2, $xor, $prevC")
        swap(w1, w2)
        if(xor == w2) {
          xor = w1
        } else {
          prevC = w1
        }
        e3 = s"(${xor} XOR ${prevC})"
        w3 = exprWires.getOrElse(e3, "")
      }
      //println(s"e3: $e3, w3: $w3")

      val e1 = s"($xor AND $prevC)"
      val w1 = exprWires.getOrElse(e1, "")
      //println(s"e1: $e1, w1: $w1")

      val and = exprWires.getOrElse(andExpr, "")
      val e2 = s"(${and} OR ${w1})"
      val w2 = exprWires.getOrElse(e2, "")
      //println(s"e2: $e2, w2: $w2")

      zExprs.update(s"z$sn", AddExpr(w3, w2))
    }
    
  def computeZs(): Unit =
    (0 until 45).foreach(i => {
      computeZ(i)
    })

class solution(input: String):
  val lines: List[String] = fromFile(input).getLines.toList

  def prob1(): Long =
    val graph = Graph(lines)
    graph.build()
    graph.compute()

    val r = graph.wireValues
                 .filter((g, v) => g(0) == 'z').toList
                 .sorted.reverse
                 .foldLeft(0.toLong)((acc, v) => (acc << 1) + v._2.toLong)
    r



  def prob2(): String =
    val graph = Graph(lines)
    graph.build2()
    graph.computeZs()
    // val r = graph.exprWires.toList.sorted
    // println(r.map((g, expr) => s"$g -> $expr").mkString("\n", "\n", ""))
    graph.pairsToSwap.sorted.mkString("", ",", "")
    

  def run(): Unit =
    println("===== Day 24 =====")
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
