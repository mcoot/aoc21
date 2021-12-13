package aoc21.day11

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Numbers, Parser}
import scala.collection.mutable.ArrayBuffer

object Parsing:
  val octopusEnergy: Parser[Int] = Numbers.digit.map(_.toInt - '0')

  val octopusLine: Parser[Array[Int]] = octopusEnergy.rep(10, 10).map(l => Array(l.toList:_*))

  val octopusArray: Parser[Array[Array[Int]]] = CommonParsers.lineSeparated(octopusLine).map(l => Array(l:_*))


def adjacentPositionsTo(arr: Array[Array[Int]], x: Int, y: Int): List[(Int, Int)] =
  val adjacent: ArrayBuffer[(Int, Int)] = ArrayBuffer()
  if x > 0 then
    adjacent.addOne((x - 1, y))
    if y > 0 then
      adjacent.addOne(x - 1, y - 1)
    if y < arr.length - 1 then
      adjacent.addOne(x - 1, y + 1)
  if y > 0 then
    adjacent.addOne((x, y - 1))
  if x < arr(y).length - 1 then
    adjacent.addOne(x + 1, y)
    if y > 0 then
      adjacent.addOne(x + 1, y - 1)
    if y < arr.length - 1 then
      adjacent.addOne(x + 1, y + 1)
  if y < arr.length - 1 then
    adjacent.addOne(x, y + 1)
  adjacent.toList



def stepSim(arr: Array[Array[Int]]): Long =
  var flashCount = 0L

  // Every octopus increases energy by 1
  for y <- arr.indices do
    for x <- arr(y).indices do
      arr(y)(x) += 1

  // Set off flashes
  var flashRecorded = true
  while flashRecorded do
    flashRecorded = false
    for y <- arr.indices do
      for x <- arr(y).indices do
        if arr(y)(x) > 9 then
          flashRecorded = true
          flashCount += 1
          // Flash all adjacencies
          for (adjX, adjY) <- adjacentPositionsTo(arr, x, y) do
            // Increase the energy level of this octopus unless it already flashed this step
            if arr(adjY)(adjX) > 0 then
              arr(adjY)(adjX) += 1
          // Reset this octopus so it can't flash again
          arr(y)(x) = 0

  flashCount


object Day11 extends SolutionWithParser[Array[Array[Int]], Long] {
  override def dayNumber: Int = 11

  override def parser: Parser[Array[Array[Int]]] = Parsing.octopusArray

  override def solvePart1(input: Array[Array[Int]]): Long =
    val inputClone = input.map(_.clone)
    (0 until 100).foldLeft(0L) { case (priorFlashes, step) =>
      priorFlashes + stepSim(inputClone)
    }

  override def solvePart2(input: Array[Array[Int]]): Long =
    val inputClone = input.map(_.clone)
    var steps = 0L
    var flashesThisStep = 0L
    while flashesThisStep != 100L do
      flashesThisStep = stepSim(inputClone)
      steps += 1
    steps
}

@main def run = Day11.runSolution