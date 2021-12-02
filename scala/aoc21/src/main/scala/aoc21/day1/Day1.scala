package aoc21.day1

import aoc21.common.Solution

import scala.io.Source

// Be nice I've literally never written a line of scala before, scala3 or otherwise

object Day1 extends Solution[List[Int], Int]:
  override def dayNumber = 1

  override def processInput(rawInput: Source): List[Int] = rawInput.getLines.map(s => s.toInt).toList

  override def solvePart1(depths: List[Int]) = depths.zip(depths.prepended(depths(0)))
    .count { (a, b) => a > b }

  override def solvePart2(depths: List[Int]): Int =
    // Nb: was told that there's a .sliding method after solving this, lol
    // Zip three copies of the depths as optionals, offset by one
    val optionalDepths = depths.map(Some(_))
    val zipped3 = optionalDepths.prependedAll(List(None, None))
      .lazyZip(optionalDepths.prepended(None))
      .lazyZip(optionalDepths)
      .toList
    // Calculate the sum for all sliding windows where we have all three measurements
    val slidingSums = for
      t <- zipped3
    yield
      t match
        case (Some(a), Some(b), Some(c)) => Some(a + b + c)
        case _ => None
    // Do the same as part 1 for all the sums we have
    solvePart1(slidingSums.collect {
      case Some(s) => s
    })


@main def run = Day1.runSolution