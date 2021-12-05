package aoc21.day3

import aoc21.common.Solution

import scala.io.Source
import scala.collection.mutable.Set as MutableSet

def countOnes(nums: List[String], position: Int): Int =
  nums
  .map(_(position) == '1')
  .count(b => b)

def countOnesInEachPosition(nums: List[String]): List[Int] =
  nums(0).indices.map(countOnes(nums, _)).toList

def mostCommonBitsInEachPosition(nums: List[String]): List[Int] =
  countOnesInEachPosition(nums).map { n =>
    if n >= nums.length / 2 then
      1
    else
      0
  }

def leastCommonBitsInEachPosition(nums: List[String]): List[Int] =
  countOnesInEachPosition(nums).map { n =>
    if n >= nums.length / 2 then
      0
    else
      1
  }

def findOnCriterion(nums: List[String])(oneIfCriterion: (Int, List[String]) => Boolean) =
  var remaining = nums

  for
    pos <- nums(0).indices
    if remaining.length > 1
  do
    val ones = countOnes(remaining, pos)
    val filterValue = if oneIfCriterion(ones, remaining) then
      '1'
    else
      '0'
    remaining = remaining.filter(_(pos) == filterValue)

  if remaining.length != 1 then
    throw new Exception(s"Should only have one remaining item, not ${remaining.length}")

  remaining(0)

def bitListToLong(l: List[Int]): Long =
  l.foldLeft(0)((acc, bit) => (acc << 1) + bit)

def bitStringToLong(s: String): Long = java.lang.Long.parseLong(s, 2)

object Day3 extends Solution[List[String], Long]:
  override def dayNumber: Int = 3

  override def processInput(rawInput: Source): List[String] = rawInput.getLines.toList

  override def solvePart1(input: List[String]): Long =
    val threshold = input.length / 2
    val counts = countOnesInEachPosition(input)

    val gamma = bitListToLong(mostCommonBitsInEachPosition(input))
    val epsilon = bitListToLong(leastCommonBitsInEachPosition(input))

    gamma * epsilon

  override def solvePart2(input: List[String]): Long =
    val oxygen = bitStringToLong(findOnCriterion(input) { (ones, remaining) => ones >= remaining.length / 2.0 })
    val co2 = bitStringToLong(findOnCriterion(input) { (ones, remaining) => ones < remaining.length / 2.0 })
    oxygen * co2


@main def run = Day3.runSolution