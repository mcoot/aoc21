package aoc21.day21

import aoc21.common.SolutionWithParser
import cats.parse.Parser

object Parsing:
  ???


object Day21 extends SolutionWithParser[(Int, Int), Int]:
  override def dayNumber: Int = 21

  override def parser: Parser[(Int, Int)] = ???

  override def solvePart1(input: (Int, Int)): Int = ???

  override def solvePart2(input: (Int, Int)): Int = ???


@main def run = Day21.testSolution()
