package aoc21.day7

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

def part1TotalFuel(crabs: Vector[Int], alignment: Int): Int = crabs.map(c => Math.abs(c - alignment)).sum

def part2TotalFuel(crabs: Vector[Int], alignment: Int) = crabs.map { c =>
    val moveDistance = Math.abs(c - alignment)
    (0 to moveDistance).sum
  }.sum

// Assume sorted crabs
def findBestAlignmentNaive(crabs: Vector[Int], fuelFunc: (Vector[Int], Int) => Int) =
  var best = -1
  for alignment <- crabs(0) to crabs.last do
    val totalFuel = fuelFunc(crabs, alignment)
    if best == -1 || totalFuel < best then
      best = totalFuel
  best


object Day7 extends SolutionWithParser[List[Int], Int] {
  override def dayNumber: Int = 7

  override def parser: Parser[List[Int]] = CommonParsers.intList

  override def solvePart1(input: List[Int]): Int = findBestAlignmentNaive(input.toVector.sorted, part1TotalFuel)

  override def solvePart2(input: List[Int]): Int = findBestAlignmentNaive(input.toVector.sorted, part2TotalFuel)
}

@main def run = Day7.runSolution