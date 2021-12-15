package aoc21.day15

import aoc21.common.SolutionWithParser
import cats.parse.Parser


case class Cavern()


object Day15 extends SolutionWithParser[Cavern, Int]:
  override def dayNumber: Int = 15

  override def parser: Parser[Cavern] = ???

  override def solvePart1(input: Cavern): Int = ???

  override def solvePart2(input: Cavern): Int = ???


@main def run = Day15.testSolution()