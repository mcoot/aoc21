package aoc21.day22

import aoc21.common.SolutionWithParser
import cats.parse.Parser

case class Instruction()


object Parsing:
  ???


object Day22 extends SolutionWithParser[List[Instruction], Int]:
  override def dayNumber: Int = 22

  override def parser: Parser[List[Instruction]] = ???

  override def solvePart1(input: List[Instruction]): Int = ???

  override def solvePart2(input: List[Instruction]): Int = ???


@main def run = Day22.testSolution("a")