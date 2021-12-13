package aoc21.day11

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Numbers, Parser}

object Parsing:
  val octopusEnergy: Parser[Int] = Numbers.digit.map(_.toInt - '0')

  val octopusLine: Parser[Array[Int]] = octopusEnergy.rep(10, 10).map(l => Array(l.toList:_*))

  val octopusArray: Parser[Array[Array[Int]]] = CommonParsers.lineSeparated(octopusLine).map(l => Array(l:_*))

object Day11 extends SolutionWithParser[Array[Array[Int]], Int] {
  override def dayNumber: Int = 11

  override def parser: Parser[Array[Array[Int]]] = Parsing.octopusArray

  override def solvePart1(input: Array[Array[Int]]): Int = ???

  override def solvePart2(input: Array[Array[Int]]): Int = ???
}

@main def run = Day11.testSolution()