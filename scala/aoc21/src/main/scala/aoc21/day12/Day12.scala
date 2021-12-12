package aoc21.day12

import aoc21.common.{CommonParsers, SolutionWithParser, putIfAbsent}
import cats.parse.Parser
import scala.collection.mutable.Map as MutableMap

enum CaveSize:
  case Small
  case Large

case class CaveReference(name: String, size: CaveSize)

case class Cave(name: String, size: CaveSize, adjacencies: List[CaveReference]):
  def reference = CaveReference(name, size)

case class CaveGraph(start: Cave, end: Cave, map: Map[String, Cave])

object Parsing {
  val smallCaveRef: Parser[CaveReference] =
    Parser.charIn('a' to 'z').rep.map(l => CaveReference(l.toList.mkString, CaveSize.Small))
  val largeCaveRef: Parser[CaveReference] =
    Parser.charIn('A' to 'Z').rep.map(l => CaveReference(l.toList.mkString, CaveSize.Large))
  val caveRef: Parser[CaveReference] = smallCaveRef | largeCaveRef

  val caveAdjacency = (caveRef <* Parser.char('-')) ~ caveRef

  val caveGraphParser: Parser[CaveGraph] = CommonParsers.lineSeparated(caveAdjacency).map { adjacencies =>
    val cavesMap: MutableMap[String, Cave] = MutableMap()

    for
      (c1, c2) <- adjacencies
    do
      val c1Adjacencies = c2 :: cavesMap.get(c1.name).map(_.adjacencies).getOrElse(List())
      val c2Adjacencies = c1 :: cavesMap.get(c2.name).map(_.adjacencies).getOrElse(List())

      cavesMap(c1.name) = Cave(c1.name, c1.size, c1Adjacencies)
      cavesMap(c2.name) = Cave(c2.name, c2.size, c2Adjacencies)

    CaveGraph(cavesMap("start"), cavesMap("end"), cavesMap.toMap)
  }
}

object Day12 extends SolutionWithParser[CaveGraph, Int] {
  override def dayNumber: Int = 12

  override def parser: Parser[CaveGraph] = Parsing.caveGraphParser

  override def solvePart1(input: CaveGraph): Int =
    println(input)
    ???

  override def solvePart2(input: CaveGraph): Int = ???
}

@main def run = Day12.testSolution("small")