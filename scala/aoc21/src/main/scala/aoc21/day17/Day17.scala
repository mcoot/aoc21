package aoc21.day17

import aoc21.common.SolutionWithParser
import cats.parse.Parser


case class TargetAreaDefn(x1: Int, x2: Int, y1: Int, y2: Int)


object Parsing:
  ???


object Day17 extends SolutionWithParser[TargetAreaDefn, Int]:
  override def dayNumber: Int = 17

  override def parser: Parser[TargetAreaDefn] = ???

  override def solvePart1(input: TargetAreaDefn): Int = ???

  override def solvePart2(input: TargetAreaDefn): Int = ???


@main def run = Day17.testSolution()
