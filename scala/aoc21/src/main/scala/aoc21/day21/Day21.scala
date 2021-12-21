package aoc21.day21

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

object Parsing:
  val startingPos = (Parser.string("Player ") ~ CommonParsers.int ~ Parser.string(" starting position: ")) *> CommonParsers.int

  val startingPosPair = (startingPos <* CommonParsers.newLine) ~ startingPos


object Day21 extends SolutionWithParser[(Int, Int), Int]:
  override def dayNumber: Int = 21

  override def parser: Parser[(Int, Int)] = startingPosPair

  override def solvePart1(input: (Int, Int)): Int = ???

  override def solvePart2(input: (Int, Int)): Int = ???


@main def run = Day21.testSolution()
