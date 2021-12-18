package aoc21.day18

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

enum SnailfishNumber:
  case Literal(value: Int)
  case Pair(left: SnailfishNumber, right: SnailfishNumber)


object Parsing:
  val literal: Parser[SnailfishNumber] = CommonParsers.int.map(SnailfishNumber.Literal(_))

  val snailfishNumber: Parser[SnailfishNumber] = for
    _ <- Parser.string("[")
    left <- literal | snailfishNumber
    _ <- Parser.string(",")
    right <- literal | snailfishNumber
    _ <- Parser.string("]")
  yield
    SnailfishNumber.Pair(left, right)


object Day18 extends SolutionWithParser[List[SnailfishNumber], Int]:
  override def dayNumber: Int = 18

  override def parser: Parser[List[SnailfishNumber]] = CommonParsers.lineSeparated(Parsing.snailfishNumber)

  override def solvePart1(input: List[SnailfishNumber]): Int =
    ???

  override def solvePart2(input: List[SnailfishNumber]): Int = ???


@main def run = Day18.testSolution()
