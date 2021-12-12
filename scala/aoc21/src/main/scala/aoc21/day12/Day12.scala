package aoc21.day12

import aoc21.common.{CommonParsers, SolutionWithParser, putIfAbsent}
import cats.parse.Parser

enum CaveSize:
  case Small
  case Large

case class CaveReference(name: String, size: CaveSize)

case class Cave(name: String, size: CaveSize, adjacencies: List[Cave])

case class CaveGraph(start: Cave, end: Cave)



object Parsing {
  val smallCaveRef: Parser[CaveReference] =
    Parser.charIn('a' to 'z').rep.map(l => CaveReference(l.toList.mkString, CaveSize.Small))
  val largeCaveRef: Parser[CaveReference] =
    Parser.charIn('A' to 'Z').rep.map(l => CaveReference(l.toList.mkString, CaveSize.Large))
  val caveRef: Parser[CaveReference] = smallCaveRef | largeCaveRef

  val caveAdjacency = (caveRef <* Parser.char('-')) ~ caveRef

  val caveGraphParser: Parser[CaveGraph] = CommonParsers.lineSeparated(caveAdjacency).map { adjacencies =>
    val cavesMap: Map[String, Cave] = adjacencies.foldLeft(Map()) { case (m, (c1, c2)) =>
      m
        .putIfAbsent(c1.name, Cave(c1.name, c1.size, List()))
        .putIfAbsent(c2.name, Cave(c2.name, c1.size, List()))
    }
    CaveGraph(cavesMap("start"), cavesMap("end"))
  }
}



object Day12 extends SolutionWithParser[CaveGraph, Int] {
  override def dayNumber: Int = 12

  override def parser: Parser[CaveGraph] = Parsing.caveGraphParser

  override def solvePart1(input: CaveGraph): Int = ???

  override def solvePart2(input: CaveGraph): Int = ???
}

@main def run = Day12.testSolution("small")