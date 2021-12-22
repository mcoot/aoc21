package aoc21.day22

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser
import scala.collection.mutable.Set as MutableSet

case class Instruction(action: Boolean, xBounds: (Int, Int), yBounds: (Int, Int), zBounds: (Int, Int)):
  def xRange: Range = xBounds(0) to xBounds(1)
  def yRange: Range = yBounds(0) to yBounds(1)
  def zRange: Range = zBounds(0) to zBounds(1)


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

def boundRange(r: Range, minBound: Int, maxBound: Int): Range =
  Math.max(r.min, minBound) to Math.min(r.max, maxBound) by r.step


def cubesOnAfterInstruction(state: Set[(Int, Int, Int)], instruction: Instruction): Set[(Int, Int, Int)] =
  val affected = (for
    x <- boundRange(instruction.xRange, -50, 50)
    y <- boundRange(instruction.yRange, -50, 50)
    z <- boundRange(instruction.zRange, -50, 50)
  yield
    (x, y, z)).toSet

  if instruction.action then
    state ++ affected
  else
    state.diff(affected)


def reboot(instructions: List[Instruction]): Set[(Int, Int, Int)] =
  instructions.foldLeft(Set[(Int, Int, Int)]()) { case (state, instruction) =>
    cubesOnAfterInstruction(state, instruction)
  }


object Day22 extends SolutionWithParser[List[Instruction], Int]:
  override def dayNumber: Int = 22

  override def parser: Parser[List[Instruction]] = CommonParsers.lineSeparated(Parsing.instruction)

  override def solvePart1(input: List[Instruction]): Int =
    val onCubes = reboot(input)
    onCubes.size

  override def solvePart2(input: List[Instruction]): Int = ???


@main def run = Day22.testSolution("b")