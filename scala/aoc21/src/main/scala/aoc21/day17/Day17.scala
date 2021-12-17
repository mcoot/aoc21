package aoc21.day17

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class TargetAreaDefn(x1: Int, x2: Int, y1: Int, y2: Int)


object Parsing:
  val range: Parser[(Int, Int)] = (CommonParsers.int <* Parser.string("..")) ~ CommonParsers.int

  val targetAreaDefn: Parser[TargetAreaDefn] = for
    _ <- Parser.string("target area: x=")
    xRange <- range
    _ <- Parser.string(", y=")
    yRange <- range
  yield
    TargetAreaDefn(xRange(0), xRange(1), yRange(0), yRange(1))


object Day17 extends SolutionWithParser[TargetAreaDefn, Int]:
  override def dayNumber: Int = 17

  override def parser: Parser[TargetAreaDefn] = Parsing.targetAreaDefn

  override def solvePart1(input: TargetAreaDefn): Int =
    println(input)
    ???

  override def solvePart2(input: TargetAreaDefn): Int = ???


@main def run = Day17.testSolution()
