package aoc21.day23

import aoc21.common.SolutionWithParser
import cats.parse.Parser


case class Burrow()


object Day23 extends SolutionWithParser[Burrow, Int]:
  override def dayNumber: Int = 23

  override def parser: Parser[Burrow] = ???

  override def solvePart1(input: Burrow): Int = ???

  override def solvePart2(input: Burrow): Int = ???


@main def run = Day23.testSolution()
