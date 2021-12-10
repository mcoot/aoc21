package aoc21.day9

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Numbers, Parser}
import scala.collection.mutable.Set as MutableSet

case class HeightMap(arr: Array[Array[Int]])

val rowParser = Numbers.digits.map(_.map(_.toInt - '0').toArray)
val heightMapParser = CommonParsers.lineSeparated(rowParser)
  .map(l => HeightMap(l.toArray))

def getAdjacencies(hmap: HeightMap, row: Int, col: Int) =
  val adjacencies: MutableSet[(Int, Int)] = MutableSet()
  if row > 0 then
    adjacencies.add((row-1, col))
  if col > 0 then
    adjacencies.add((row, col-1))
  if row < hmap.arr.length - 1 then
    adjacencies.add((row+1, col))
  if col < hmap.arr(0).length - 1 then
    adjacencies.add((row, col+1))
  adjacencies.toSet

def isLowPoint(hmap: HeightMap, row: Int, col: Int) =
  getAdjacencies(hmap, row, col).forall {
    case (r, c) => hmap.arr(row)(col) < hmap.arr(r)(c)
  }

def findLowPoints(hmap: HeightMap) =
  for
    row <- hmap.arr.indices
    col <- hmap.arr(0).indices
    if isLowPoint(hmap, row, col)
  yield
    (row, col)


def findLowPointRiskLevels(hmap: HeightMap) =
  for
    row <- hmap.arr.indices
    col <- hmap.arr(0).indices
    if isLowPoint(hmap, row, col)
  yield
    hmap.arr(row)(col) + 1


// Explore a basin, counting the number of cells
def exploreBasin(hmap: HeightMap, row: Int, col: Int, depth: Int = 0): Int =
  // Mark this cell as visited
  hmap.arr(row)(col) = -1
  val adjacencies = getAdjacencies(hmap, row, col)
  var size = 1
  val adjacentSizes = for
    (r, c) <- adjacencies
    if hmap.arr(r)(c) != -1 && hmap.arr(r)(c) != 9
  do
    size += exploreBasin(hmap, r, c, depth + 1)
  size


def getBasinSizes(hmap: HeightMap): List[Int] =
  var basinSizes: List[Int] = List()
  val lowPoints = findLowPoints(hmap)
  for
    (row, col) <- lowPoints
  do
    basinSizes = exploreBasin(hmap, row, col) :: basinSizes
  basinSizes

object Day9 extends SolutionWithParser[HeightMap, Int] {
  override def dayNumber = 9

  override def parser: Parser[HeightMap] = heightMapParser

  override def solvePart1(input: HeightMap): Int = findLowPointRiskLevels(input).sum

  override def solvePart2(input: HeightMap): Int = getBasinSizes(input).sorted.takeRight(3).product
}

@main def run = Day9.testSolution()