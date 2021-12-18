package aoc21.day18

import aoc21.common.SolutionWithParser
import cats.parse.Parser

case class SnailfishNumber()


object Parsing:
  ???


object Day18 extends SolutionWithParser[List[SnailfishNumber], Int]:
  override def dayNumber: Int = 18

  override def parser: Parser[List[SnailfishNumber]] = ???

  override def solvePart1(input: List[SnailfishNumber]): Int = ???

  override def solvePart2(input: List[SnailfishNumber]): Int = ???


@main def run = Day18.testSolution()
