package aoc21.day12

import aoc21.common.SolutionWithParser
import cats.parse.Parser

enum CaveSize:
  case Small
  case Large

case class Cave(size: CaveSize, adjacencies: List[Cave])

case class CaveGraph(start: Cave, end: Cave)

val caveParser = ???

val adjParser = ???

val caveGraphParser = ???

object Day12 extends SolutionWithParser[CaveGraph, Int] {
  override def dayNumber: Int = 12

  override def parser: Parser[CaveGraph] = caveGraphParser

  override def solvePart1(input: CaveGraph): Int = ???

  override def solvePart2(input: CaveGraph): Int = ???
}

@main def run = Day12.testSolution("small")