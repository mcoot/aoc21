package aoc21.day16

import aoc21.common.SolutionWithParser
import cats.parse.Parser

case class Transmission()


object Day16 extends SolutionWithParser[Transmission, Int]:
  override def dayNumber: Int = 16

  override def parser: Parser[Transmission] = ???

  override def solvePart1(input: Transmission): Int = ???

  override def solvePart2(input: Transmission): Int = ???


@main def run = Day16.testSolution("g")

