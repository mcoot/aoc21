package aoc21.day6

import scala.collection.mutable.Map as MutableMap

import cats.parse.{Numbers, Parser}
import aoc21.common.{CommonParsers, SolutionWithParser}


def runFishNaive(input: List[Int], days: Int): Long =
  var fish = Vector(input:_*)
  for day <- 0 until days do
    val newFish = fish.filter(_ == 0).length
    fish = fish.map(age => if age == 0 then 6 else age - 1) ++ Vector.fill(newFish)(8)
  fish.length

def runFishCounts(input: List[Int], days: Int): Long =
  // Initialise counts of each fish number
  var fish: MutableMap[Int, Long] = MutableMap(
    input
    .groupBy(identity)
    .map { case (k, l) => (k, l.length.toLong) }
    .toSeq:_*
  )

  // Progress sim through the days
  for day <- 0 until days do
    val newFish: MutableMap[Int, Long] = MutableMap()
    // Spawn new fish
    newFish.addOne((8, fish.getOrElse(0, 0)))

    // Decrement age timer on fish
    for (k: Int, v: Long) <- fish do
      val newAge: Int = if k == 0 then 6 else k - 1
      val newCount: Long = newFish.getOrElse(newAge, 0l) + v
      if newCount > 0 then
        newFish.addOne(newAge, newCount)

    fish = newFish

  // Get total number of fish
  fish.map { case (k, v) => v }.sum


object Day6 extends SolutionWithParser[List[Int], Long]:
  override def dayNumber: Int = 6

  override def parser: Parser[List[Int]] = CommonParsers.commaSeparated(CommonParsers.int)

  override def solvePart1(input: List[Int]): Long = runFishCounts(input, 80)


  override def solvePart2(input: List[Int]): Long = runFishCounts(input, 256)


@main def run = Day6.runSolution