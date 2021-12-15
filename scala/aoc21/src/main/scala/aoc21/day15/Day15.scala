package aoc21.day15

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Numbers, Parser}

import scala.collection.mutable
import scala.collection.mutable.{PriorityQueue, Map as MutableMap, Set as MutableSet}

class Cavern(val arr: Array[Array[Int]]):
  def apply(pos: (Int, Int)) = pos match
    case (row, col) => arr(row)(col)

  def numRows = arr.length

  def numCols = arr(0).length

  def start = (0, 0)

  def goal = (numRows - 1, numCols - 1)

  def adjacent(pos: (Int, Int)): Set[(Int, Int)] = pos match
    case (row, col) =>
      val s: MutableSet[(Int, Int)] = MutableSet()
      if row > 0 then
        s.add((row - 1, col))
      if col > 0 then
        s.add((row, col - 1))
      if row < numRows - 1 then
        s.add((row + 1, col))
      if col < numCols - 1 then
        s.add((row, col + 1))
      s.toSet

// Cavern tiled 5 times horizontally and vertically
// with each tile incrementing the values of a cell by one, wrapping around after 9
class TiledCavern(arr: Array[Array[Int]]) extends Cavern(arr):
  override def apply(pos: (Int, Int)) = pos match
    case (row, col) =>
      val rowTile = row / arr.length
      val colTile = col / arr(0).length
      val tileDist = rowTile + colTile
      val rawValue = arr(row % arr.length)(col % arr(0).length)
      (rawValue + tileDist - 1) % 9 + 1

  override def numRows = arr.length * 5

  override def numCols = arr(0).length * 5


object Parsing:
  def positionRisk = Numbers.digit.map(_.toInt - '0')

  val line: Parser[Array[Int]] = positionRisk.rep.map(l => Array(l.toList:_*))

  val cavern: Parser[Cavern] = CommonParsers.lineSeparated(line).map(l => Cavern(Array(l:_*)))


def dijkstra(cavern: Cavern): List[(Int, Int)] =
  // Hackily not actually storing unvisited nodes...
  val visited: MutableSet[(Int, Int)] = MutableSet()
  val distances: MutableMap[(Int, Int), Int] = MutableMap()
  val unvisitedQueue: PriorityQueue[((Int, Int), Int)] = PriorityQueue()(Ordering.by { case (_, dist) => -dist })
  val cameFrom: MutableMap[(Int, Int), (Int, Int)] = MutableMap()
  distances(cavern.start) = 0
  unvisitedQueue.addOne((cavern.start, distances(cavern.start)))
  // Just say the start pos came from itself, since we'll terminate on that anyway
  cameFrom(cavern.start) = cavern.start

  var pos = unvisitedQueue.dequeue()(0)

  while !visited.contains(cavern.goal) do
    val distToCurrent = distances(pos)

    val toVisit = cavern.adjacent(pos).filter(!visited.contains(_))
    for adj <- toVisit do
      val distThroughHere = distToCurrent + cavern(adj)
      if !distances.contains(adj) || distThroughHere < distances(adj) then
        distances(adj) = distThroughHere
        unvisitedQueue.addOne((adj, distances(adj)))
        cameFrom(adj) = pos
    visited.add(pos)
    if !visited.contains(cavern.goal) then
      pos = unvisitedQueue.dequeue()(0)

  var path: List[(Int, Int)] = List(cavern.goal)
  while path(0) != cavern.start do
    path = cameFrom(path(0)) :: path
  path

def getPathRisk(cavern: Cavern, path: List[(Int, Int)]) = path.drop(1).map { case (r, c) => cavern((r, c)) }.sum


object Day15 extends SolutionWithParser[Cavern, Int]:
  override def dayNumber: Int = 15

  override def parser: Parser[Cavern] = Parsing.cavern

  override def solvePart1(input: Cavern): Int =
    val path = dijkstra(input)
    getPathRisk(input, path)

  override def solvePart2(input: Cavern): Int =
    val tiledCavern = new TiledCavern(input.arr)
    val path = dijkstra(tiledCavern)
    getPathRisk(tiledCavern, path)


@main def run = Day15.runSolution