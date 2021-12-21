package aoc21.day19

import aoc21.common.SolutionWithParser
import cats.parse.Parser


case class ScannerData()

]``
object Day19 extends SolutionWithParser[List[ScannerData], Int]:
  override def dayNumber: Int = 19

  override def parser: Parser[List[ScannerData]] = ???

  override def solvePart1(input: List[ScannerData]): Int = ???

  override def solvePart2(input: List[ScannerData]): Int = ???


@main def run = Day19.testSolution()
