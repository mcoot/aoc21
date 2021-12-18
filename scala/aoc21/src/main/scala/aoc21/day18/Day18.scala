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


def rawAddSn(sn1: SnailfishNumber, sn2: SnailfishNumber): SnailfishNumber = SnailfishNumber.Pair(sn1, sn2)

def reduceSn(sn: SnailfishNumber): SnailfishNumber = ???

def addSn(sn1: SnailfishNumber, sn2: SnailfishNumber): SnailfishNumber = reduceSn(rawAddSn(sn1, sn2))


def magnitude(sn: SnailfishNumber): Int = sn match
  case SnailfishNumber.Literal(n) => n
  case SnailfishNumber.Pair(l, r) => magnitude(l) * 3 + magnitude(r) * 2


object Day18 extends SolutionWithParser[List[SnailfishNumber], Int]:
  override def dayNumber: Int = 18

  override def parser: Parser[List[SnailfishNumber]] = CommonParsers.lineSeparated(Parsing.snailfishNumber)

  override def solvePart1(input: List[SnailfishNumber]): Int =
    val finalSn = input.reduce(addSn)
    magnitude(finalSn)

  override def solvePart2(input: List[SnailfishNumber]): Int = ???


@main def run = Day18.testSolution()
