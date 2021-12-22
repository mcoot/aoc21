package aoc21.day22

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class Instruction(action: Boolean, xRange: (Int, Int), yRange: (Int, Int), zRange: (Int, Int))


object Parsing:
  val range: Parser[(Int, Int)] = for
    a <- CommonParsers.int
    _ <- Parser.string("..")
    b <- CommonParsers.int
  yield
    (a, b)

  val action: Parser[Boolean] = Parser.string("on").map(_ => true) | Parser.string("off").map(_ => false)

  val instruction: Parser[Instruction] = for
    a <- action
    _ <- Parser.string(" x=")
    xr <- range
    _ <- Parser.string(",y=")
    yr <- range
    _ <- Parser.string(",z=")
    zr <- range
  yield
    Instruction(a, xr, yr, zr)


object Day22 extends SolutionWithParser[List[Instruction], Int]:
  override def dayNumber: Int = 22

  override def parser: Parser[List[Instruction]] = CommonParsers.lineSeparated(Parsing.instruction)

  override def solvePart1(input: List[Instruction]): Int =
    ???

  override def solvePart2(input: List[Instruction]): Int = ???


@main def run = Day22.testSolution("a")