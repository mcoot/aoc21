package aoc21.day20

import aoc21.common.SolutionWithParser
import cats.parse.Parser


case class Input()


object Day20 extends SolutionWithParser[Input, Int]:
  override def dayNumber: Int = 20

  override def parser: Parser[Input] = ???

  override def solvePart1(input: Input): Int = ???

  override def solvePart2(input: Input): Int = ???


@main def run = Day20.testSolution()

